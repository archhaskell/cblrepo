module BuildPkgs where

import PkgDB
import Data.List

buildPkgs dbFp pkgs = do
    db <- readDb dbFp
    mapM_ putStrLn $ transitiveDependants db pkgs
