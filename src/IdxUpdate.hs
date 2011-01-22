module IdxUpdate where

import Data.ByteString as BS
import Network.Download
import System.FilePath

idxUpdate dn = do
    r <- openURI "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    either (\ e -> error e) (\ b -> BS.writeFile (dn </> "00-index.tar.gz") b) r
