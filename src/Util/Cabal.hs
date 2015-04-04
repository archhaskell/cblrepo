{-# LANGUAGE FlexibleContexts #-}
{-
 - Copyright 2014 Per Magnus Therning
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

module Util.Cabal
    where

import Util.HackageIndex

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import System.Directory
import System.Exit
import System.FilePath
import System.Posix
import System.Process

data CabalParseEnv = CabalParseEnv
    { cpeAppDir :: FilePath
    , cpePatchDir :: FilePath
    , cpeDestDir :: FilePath
    } deriving (Eq, Show)

-- | Type used for reading Cabal files
type CabalParse a = ReaderT CabalParseEnv (ErrorT String IO) a

-- | Run a Cabal parse action in the provided environment.
runCabalParse :: CabalParseEnv -> CabalParse a -> IO (Either String a)
runCabalParse cpe f = runErrorT $ runReaderT f cpe

runCabalParseE :: CabalParseEnv -> CabalParse a -> (ErrorT String IO) a
runCabalParseE cpe f = runReaderT f cpe

readFromFile :: FilePath -- ^ file name
    -> CabalParse GenericPackageDescription
readFromFile fn = do
    destDir <- asks cpeDestDir
    let destFn = destDir </> takeFileName fn
    liftIO $ copyFile fn destFn
    patch destFn
    liftIO $ readPackageDescription silent destFn

readFromIdx :: (String, Version) -- ^ package name and version
    -> CabalParse GenericPackageDescription
readFromIdx (pN, pV) = do
    destDir <- asks cpeDestDir
    appDir <- asks cpeAppDir
    let destFn = destDir </> pN <.> ".cabal"
    copyCabal appDir destFn
    patch destFn
    liftIO $ readPackageDescription silent destFn

    where
        copyCabal appDir destFn = do
            idx <- liftIO $ readIndexFile appDir
            cbl <- maybe (throwError $ "Failed to extract contents for " ++ pN ++ " " ++ display pV) return
                (extractCabal idx pN pV)
            liftIO $ BSL.writeFile destFn cbl

-- |  Patch a Cabal file.
--
-- The name of the patch file is based on the name of the Cabal package: @<cabal
-- pkg name>.patch@.  The file is patched only if a patch with the correct name
-- is found in the patch directory (provided via 'CabalParseEnv').
patch :: FilePath -- ^ file to patch
    -> CabalParse ()
patch fn = do
    pkgName <- liftIO $ extractName fn
    patchDir <- asks cpePatchDir
    let patchFn = patchDir </> pkgName <.> "cabal"
    applyPatchIfExist fn patchFn

    where
        extractName fn = liftM name $ readPackageDescription silent fn
        name = display . pkgName . package . packageDescription

        applyPatchIfExist origFn patchFn =
            liftIO (fileExist patchFn) >>= flip when (applyPatch origFn patchFn)

        applyPatch origFn patchFn = do
            (ec, _, _) <- liftIO $ readProcessWithExitCode "patch" [origFn, patchFn] ""
            case ec of
                ExitSuccess -> return ()
                ExitFailure _ ->
                    throwError ("Failed patching " ++ origFn ++ " with " ++ patchFn)
