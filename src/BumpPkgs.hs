module BumpPkgs where

import PkgDB
import Utils

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import System.FilePath

bumpPkgs :: ReaderT Cmds IO ()
bumpPkgs = do
    db <- cfgGet dbFile >>= liftIO . readDb
    pkgs <- cfgGet pkgs
    liftIO $ mapM_ putStrLn $ transDependants db pkgs

transDependants db pkgs = filter (not . flip elem pkgs) $ transitiveDependants db pkgs
