module BuildPkgs where

import PkgDB
import Data.List

buildPkgs dbFp pkgs = do
    db <- readDb dbFp
    mapM_ putStrLn $ transUsers db pkgs

transUsers db pkgs = keepLast $ concat $ map transUsersOfOne pkgs
    where
        transUsersOfOne pkg = pkg : (keepLast $ concat $ map (transUsersOfOne) (lookupDependants db pkg))
        keepLast = reverse . nub . reverse
