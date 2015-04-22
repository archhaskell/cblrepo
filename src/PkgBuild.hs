{-
 - Copyright 2011-2014 Per Magnus Therning
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module PkgBuild where

import PkgDB
import Util.Misc
import Util.Translation
import qualified Util.Cabal as Cbl

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans.Except
import System.Directory
import System.FilePath
import System.IO
import System.Unix.Directory
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.ByteString.Lazy as BSL (writeFile)

pkgBuild :: Command ()
pkgBuild = do
    pkgs <- asks  $ pkgs . optsCmd
    void $ mapM (runExceptT . generatePkgBuild) pkgs >>= exitOnAnyLefts

generatePkgBuild :: String -> ExceptT String Command ()
generatePkgBuild pkg = do
        db <- asks dbFile >>= liftIO . readDb
        patchDir <- asks  $ patchDir . optsCmd
        ghcVer <- asks $ ghcVer . optsCmd
        ghcRel <- asks $ ghcRel . optsCmd
        (ver, fa) <- maybe (throwE $ "Unknown package: " ++ pkg) (return . (pkgVersion &&& pkgFlags)) $ lookupPkg db pkg
        ---
        (cblFileRaw, genericPkgDesc) <- runCabalParseWithTempDir $ Cbl.readFromIdx (pkg, ver)
        let cblFile = dosToUnix cblFileRaw
        pkgDescAndFlags <- either (const $ throwE ("Failed to finalize package: " ++ pkg)) return
            (finalizePkg ghcVer db fa genericPkgDesc)
        let archPkg = translate ghcVer ghcRel db (snd pkgDescAndFlags) (fst pkgDescAndFlags)
        archPkgWPatches <- liftIO $ addPatches patchDir archPkg
        archPkgWHash <- withTempDirExceptT "/tmp/cblrepo." (addHashes archPkgWPatches cblFile)
        liftIO $ createDirectoryIfMissing False (apPkgName archPkgWHash)
        liftIO $ withWorkingDirectory (apPkgName archPkgWHash) $ do
            copyPatches "." archPkgWHash
            hPKGBUILD <- openFile "PKGBUILD" WriteMode
            hPutDoc hPKGBUILD $ pretty archPkgWHash
            hClose hPKGBUILD
            maybe (return ()) (void . runExceptT . applyPatch "PKGBUILD")
                (apPkgbuildPatch archPkgWHash)
            when (apHasLibrary archPkgWHash) $ do
                hInstall <- openFile (apPkgName archPkgWHash <.> "install") WriteMode
                let archInstall = aiFromAP archPkgWHash
                hPutDoc hInstall $ pretty archInstall
                hClose hInstall
                maybe (return ()) (void . runExceptT . applyPatch (apPkgName archPkgWHash <.> "install"))
                    (apInstallPatch archPkgWHash)
            BSL.writeFile ("original.cabal") cblFile

runCabalParseWithTempDir :: Cbl.CabalParse a -> ExceptT String Command a
runCabalParseWithTempDir f = do
    aD <- asks appDir
    pD <- asks $ patchDir . optsCmd
    r <- liftIO $ withTemporaryDirectory "/tmp/cblrepo." $ \ destDir -> do
        let cpe = Cbl.CabalParseEnv aD pD destDir
        Cbl.runCabalParse cpe f
    reThrowE r
