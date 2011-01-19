module PrintUsers where

import PkgDB

import Data.List

printUsers dbFp pkgs = do
    db <- readDb dbFp
    mapM_ putStrLn $ transDependants db pkgs

-- todo: add arg to control whether filter is applied or not
-- currently it prints all packages that should be bumped, which isn't
-- necessarily what we want
transDependants db pkgs = filter (not . flip elem pkgs) $ reverse $ nub $ concat $ map (transDep db) pkgs
    where
        -- finds the transitive dependants for a single package, but in reverse
        -- build order
        transDep db pkg = transDep' db [pkg] []

        transDep' _ [] res = res
        transDep' db (pkg:pkgs) res = let
                newPkgs = pkgs ++ (lookupDependants db pkg)
            in transDep' db newPkgs (pkg:res)
