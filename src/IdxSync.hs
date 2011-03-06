module IdxSync where

import Util.Misc

import Control.Monad.Reader
import System.FilePath
import Control.Monad.Error

idxSync :: ReaderT Cmds IO ()
idxSync = do
    aD <- cfgGet appDir
    liftIO $ getFromURL "http://hackage.haskell.org/packages/archive/00-index.tar.gz" (aD </> "00-index.tar.gz")
