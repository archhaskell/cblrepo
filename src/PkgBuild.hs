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
import System.Exit
import System.IO
import System.Unix.Directory
import Text.PrettyPrint.ANSI.Leijen

pkgBuild :: ReaderT Cmds IO ()
pkgBuild = do
    db <- cfgGet dbFile >>= liftIO . readDb
    pD <- cfgGet patchDir
    pkgs <- cfgGet pkgs
    mapM (runErrorT . generatePkgBuild db pD) pkgs >>= exitOnErrors >> return ()

-- TODO:
--  - flags
-- generatePkgBuild :: CblDB -> String -> String -> ErrorT String IO ()
generatePkgBuild db patchDir pkg = let
        appendPkgVer = pkg ++ "," ++ (display $ pkgVersion $ fromJust $ lookupPkg db pkg)
    in do
        maybe (throwError $ "Unknown package: " ++ pkg) (const $ return ()) (lookupPkg db pkg)
        genericPkgDesc <- withTempDirErrT "/tmp/cblrepo." (readCabal patchDir appendPkgVer)
        pkgDesc <- either (const $ throwError ("Failed to finalize package: " ++ pkg)) (return . fst) (finalizePkg db genericPkgDesc)
        let archPkg = translate db pkgDesc
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
                hInstall <- openFile (apPkgName archPkgWHash ++ ".install") WriteMode
                let archInstall = aiFromAP archPkgWHash
                hPutDoc hInstall $ pretty archInstall
                hClose hInstall
