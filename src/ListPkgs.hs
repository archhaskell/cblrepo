module ListPkgs where

import Util.Misc
import PkgDB

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Distribution.Text
import System.FilePath

listPkgs :: ReaderT Cmds IO ()
listPkgs = do
    iB <- cfgGet incBase
    db <- cfgGet dbFile >>= liftIO . readDb
    let pkgs = if not iB
            then filter (not . isBasePkg) db
            else db
    liftIO $ mapM_ printCblPkgShort pkgs

printCblPkgShort (p, (v, _)) =
    putStrLn $ p ++ " ==" ++ (display v)
