module BumpPkgs where

import PkgDB
import Util.Misc

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import System.FilePath

bumpPkgs :: ReaderT Cmds IO ()
bumpPkgs = do
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    dR <- cfgGet dryRun
    pkgs <- cfgGet pkgs
    incl <- cfgGet inclusive
    let bpkgs = transDependants db incl pkgs
    let newDb = foldl (\ db p -> bumpRelease db p) db bpkgs
    if dR
        then liftIO $ putStrLn "Would bump:" >> mapM_ putStrLn bpkgs
        else liftIO $ saveDb newDb dbFn

transDependants db i pkgs = filter ((||) i . (not . flip elem pkgs)) $ transitiveDependants db pkgs
