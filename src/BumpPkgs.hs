module BumpPkgs where

import PkgDB

import Data.List

bumpPkgs dbFp pkgs = do
    db <- readDb dbFp
    mapM_ putStrLn $ transDependants db pkgs

transDependants db pkgs = filter (not . flip elem pkgs) $ transitiveDependants db pkgs
