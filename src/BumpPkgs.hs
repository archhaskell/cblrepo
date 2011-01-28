module BumpPkgs where

import PkgDB
import Utils

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe

bumpPkgs :: ReaderT Cmds IO ()
bumpPkgs = do
    db <- cfgGet (fromJust . dbLoc) >>= liftIO . readDb
    pkgs <- cfgGet pkgs
    liftIO $ mapM_ putStrLn $ transDependants db pkgs

transDependants db pkgs = filter (not . flip elem pkgs) $ transitiveDependants db pkgs
