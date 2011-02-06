{-# LANGUAGE DeriveDataTypeable #-}

module Utils where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Data
import Data.Typeable
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
    = AddBasePkg { appDir :: String, dryRun :: Bool, pkgVers :: [(String, String)] }
    | AddPkg { appDir :: String, dryRun :: Bool, cbls :: [FilePath] }
    | BuildPkgs { appDir :: String, pkgs :: [String] }
    | BumpPkgs { appDir :: String, pkgs :: [String] }
    | IdxUpdate { appDir :: String }
    | IdxVersion { appDir :: String, pkgs :: [String] }
    | ListPkgs { appDir :: String, incBase :: Bool }
    | Updates { appDir :: String }
    | Urls { appDir :: String, pkgVers :: [(String, String)] }
    deriving (Show, Data, Typeable)

cfgGet f = liftM f ask
