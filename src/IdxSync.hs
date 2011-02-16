module IdxSync where

import Utils

import Control.Monad.Reader
import System.FilePath
import Control.Monad.Error

idxSync :: ReaderT Cmds IO ()
idxSync = do
    aD <- cfgGet appDir
    r <- liftIO $ runErrorT $ getFromURL "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    liftIO $ either (\ e -> error e) (\ b -> writeFile (aD </> "00-index.tar.gz") b) r
