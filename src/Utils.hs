{-# LANGUAGE DeriveDataTypeable #-}

module Utils where

import Data.Typeable
import Data.Data
import Distribution.Package
import Distribution.Text

-- {{{1 Dependency
depName (Dependency (PackageName n) _) = n
depVersionRange (Dependency _ vr) = vr

-- {{{ print functions
printUnSat (n, ds) = do
    putStrLn $ "Failed to satisfy the following dependencies for " ++ n ++ ":"
    mapM_ (putStrLn . ("  " ++) . display) ds

printBrksOth  ((n, v), brks) = do
    putStrLn $ "Adding " ++ n ++ " " ++ (display v) ++ " would break:"
    mapM_ (\ (bN, (Just bD)) -> putStrLn $ "  " ++ bN ++ " : " ++ (display bD)) brks

-- {{{1 program variables
progName = "cblrepo"
dbName = progName ++ ".db"

-- {{{1 command line argument type
data Cmds
    = AddBasePkg {dbLoc :: Maybe String, pkgVers :: [(String, String)]}
    | AddPkg {dbLoc :: Maybe String, cbls :: [FilePath]}
    | BumpPkgs {dbLoc :: Maybe String, pkgs :: [String]}
    | BuildPkgs {dbLoc :: Maybe String, pkgs :: [String]}
    | IdxUpdate {dbLoc :: Maybe String}
    | Updates {dbLoc :: Maybe String}
    | ListPkgs {dbLoc :: Maybe String, incBase :: Bool}
    deriving(Show, Data, Typeable)
