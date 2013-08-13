{-
 - Copyright 2011 Per Magnus Therning
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

import Control.Monad.Error
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Distribution.Text
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.Unix.Directory
import Text.PrettyPrint.ANSI.Leijen

pkgBuild :: Command ()
pkgBuild = do
    db <- cfgGet dbFile >>= liftIO . readDb
    pD <- cfgGet  $ patchDir . optsCmd
    pkgs <- cfgGet  $ pkgs . optsCmd
    mapM (runErrorT . generatePkgBuild db pD) pkgs >>= exitOnErrors >> return ()

-- TODO:
-- generatePkgBuild :: CblDB -> String -> String -> ErrorT String IO ()
generatePkgBuild db patchDir pkg = let
        appendPkgVer = pkg ++ "," ++ (display $ pkgVersion $ fromJust $ lookupPkg db pkg)
    in do
        maybe (throwError $ "Unknown package: " ++ pkg) (const $ return ()) (lookupPkg db pkg)
        genericPkgDesc <- withTempDirErrT "/tmp/cblrepo." (readCabal patchDir appendPkgVer)
        pkgDescAndFlags <- either (const $ throwError ("Failed to finalize package: " ++ pkg)) return (finalizePkg db genericPkgDesc)
        let archPkg = translate db (snd pkgDescAndFlags) (fst pkgDescAndFlags)
        archPkgWPatches <- liftIO $ addPatches patchDir archPkg
        archPkgWHash <- withTempDirErrT "/tmp/cblrepo." (addHashes archPkgWPatches)
        liftIO $ createDirectoryIfMissing False (apPkgName archPkgWHash)
        liftIO $ withWorkingDirectory (apPkgName archPkgWHash) $ do
            copyPatches "." archPkgWHash
            hPKGBUILD <- openFile "PKGBUILD" WriteMode
            hPutDoc hPKGBUILD $ pretty archPkgWHash
            hClose hPKGBUILD
            maybe (return ()) (\ pfn -> (runErrorT $ applyPatch "PKGBUILD" pfn) >> return ()) (apPkgbuildPatch archPkgWHash)
            when (apHasLibrary archPkgWHash) $ do
                hInstall <- openFile (apPkgName archPkgWHash <.> "install") WriteMode
                let archInstall = aiFromAP archPkgWHash
                hPutDoc hInstall $ pretty archInstall
                hClose hInstall
                maybe (return ()) (\ pfn -> (runErrorT $ applyPatch (apPkgName archPkgWHash <.> "install") pfn)>> return ()) (apInstallPatch archPkgWHash)
