{-# LANGUAGE FlexibleContexts #-}
module Extract
    where

import Util.HackageIndex
import Util.Misc

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.Version
import Distribution.Text (display)
import System.FilePath

extract :: Command ()
extract = do
    aD <- asks appDir
    pkgsNVersions <- asks $ cmdExtractPkgs . optsCmd
    --
    idx <- liftIO $ readIndexFile aD
    _ <- mapM (runErrorT . extractAndSave idx) pkgsNVersions >>= exitOnErrors
    return ()

extractAndSave :: (MonadError String m, MonadIO m) => BSL.ByteString -> (String, Version) -> m ()
extractAndSave idx (pkg, ver) = maybe (throwError errorMsg) (liftIO . BSL.writeFile destFn) (extractCabal idx pkg ver)
    where
        destFn = pkg <.> "cabal"
        errorMsg = "Failed to extract Cabal for " ++ pkg ++ " " ++ display ver
