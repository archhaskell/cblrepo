module BuildPkgs where

import PkgDB
import Utils

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import System.FilePath

buildPkgs :: ReaderT Cmds IO ()
buildPkgs = do
    db <- cfgGet dbFile >>= liftIO . readDb
    pkgs <- cfgGet pkgs
    liftIO $ mapM_ putStrLn $ transitiveDependants db pkgs
