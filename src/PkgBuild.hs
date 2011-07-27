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

import OldPkgDB
import Util.Misc
import Util.Translation

import Control.Monad.Reader
import Data.Maybe
import Distribution.Text
import System.Exit
import System.Unix.Directory
import System.Directory
import System.IO(openFile)
import System.IO
import Text.PrettyPrint.ANSI.Leijen

-- TODO:
--  - patches:
--      .cabal patch - put into sources array, copied into package dir
--      PKGBUILD patch - applied
--      build patch - put into source array, copied into package dir
--  - flags
pkgBuild :: ReaderT Cmds IO ()
pkgBuild = let
        failFinalize _ = error "pkgBuild: unexpected failure to finalize a package"
    in do
        db <- cfgGet dbFile >>= liftIO . readDb
        pD <- cfgGet patchDir
        pkgs <- cfgGet pkgs
        unless (all isJust (map (lookupPkg db) pkgs)) $
            liftIO (mapM_ (printNotAPkg db) pkgs >> exitFailure)
        let cbls = map (appendPkgVer db) pkgs
        genPDs <- liftIO $ mapM (\ c -> withTemporaryDirectory "/tmp/cblrepo." (readCabal pD c)) cbls
        let pds = map (either failFinalize id . finalizePkg db) genPDs
        let aps = map (translate db . fst) pds
        apsP <- liftIO $ mapM (addPatches pD) aps
        apsF <- liftIO $ mapM (\ a -> withTemporaryDirectory "/tmp/cblrepo." (addHashes a)) apsP
        liftIO $ mapM_ (\ a -> createDirectoryIfMissing False (apPkgName a)) apsF
        liftIO $ mapM_ (\ a -> withWorkingDirectory (apPkgName a) $ do
            copyPatches "." a
            hF <- openFile "PKGBUILD" WriteMode 
            hPutDoc hF $ pretty a
            hClose hF
            maybe (return ()) (\ pfn -> applyPatch "PKGBUILD" pfn) (apPkgbuildPatch a)
            when (apHasLibrary a) $ do
                hFI <- openFile (apPkgName a ++ ".install") WriteMode
                let ai = aiFromAP a
                hPutDoc hFI $ pretty ai
                hClose hFI
            ) apsF

printNotAPkg db pkg = maybe doPrint (const $ return ()) (lookupPkg db pkg)
    where
        doPrint = putStrLn $ "Unknown package: " ++ pkg

appendPkgVer db pkg = let
        displayVer = display . pkgVersion . fromJust . lookupPkg db
    in pkg ++ "," ++ (displayVer pkg)
