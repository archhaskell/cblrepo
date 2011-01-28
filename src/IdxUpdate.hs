module IdxUpdate where

import Utils

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString as BS
import Network.Download
import System.FilePath

idxUpdate :: ReaderT Cmds IO ()
idxUpdate = do
    aD <- cfgGet appDir
    r <- liftIO $ openURI "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    liftIO $ either (\ e -> error e) (\ b -> BS.writeFile (aD </> "00-index.tar.gz") b) r
