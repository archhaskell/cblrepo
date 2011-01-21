module BumpPkgs where

import PkgDB

import Data.List

bumpPkgs dbFp pkgs = do
    db <- readDb dbFp
    mapM_ putStrLn $ transDependants db pkgs

transDependants db pkgs = filter (not . flip elem pkgs) $ nub $ concat $ map (transDep db) pkgs
    where
        transDep db pkg = transDep' db [pkg] []

        transDep' _ [] res = res
        transDep' db (pkg:pkgs) res = transDep' db newPkgs (pkg:res)
            where
                newPkgs = pkgs ++ (lookupDependants db pkg)
