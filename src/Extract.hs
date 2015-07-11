{-# LANGUAGE FlexibleContexts #-}
module Extract
    where

import Util.HackageIndex
import Util.Misc

import Control.Monad.Trans.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.Version
import Distribution.Text (display)
import System.FilePath

extract :: Command ()
extract = do
    aD <- asks $ appDir . fst
    pkgsNVersions <- asks $ cmdExtractPkgs . optsCmd . fst
    --
    idx <- liftIO $ readIndexFile aD
    _ <- mapM (runExceptT . extractAndSave idx) pkgsNVersions >>= exitOnAnyLefts
    return ()

extractAndSave :: MonadIO m => BSL.ByteString -> (String, Version) -> ExceptT String m ()
extractAndSave idx (pkg, ver) = maybe (throwE errorMsg) (liftIO . BSL.writeFile destFn) (extractCabal idx pkg ver)
    where
        destFn = pkg <.> "cabal"
        errorMsg = "Failed to extract Cabal for " ++ pkg ++ " " ++ display ver
