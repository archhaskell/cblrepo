{-# LANGUAGE DeriveDataTypeable #-}

module Utils where

import Control.Monad
import Control.Monad.Reader
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
    = AddBasePkg { appDir :: FilePath, dbFile :: FilePath, dryRun :: Bool, pkgVers :: [(String, String)] }
    | AddPkg { appDir :: FilePath, dbFile :: FilePath, dryRun :: Bool, cbls :: [FilePath] }
    | BuildPkgs { appDir :: FilePath, dbFile :: FilePath, pkgs :: [String] }
    | BumpPkgs { appDir :: FilePath, dbFile :: FilePath, pkgs :: [String] }
    | IdxSync { appDir :: FilePath }
    | IdxVersion { appDir :: FilePath, pkgs :: [String] }
    | ListPkgs { appDir :: FilePath, dbFile :: FilePath, incBase :: Bool }
    | Updates { appDir :: FilePath, dbFile :: FilePath }
    | Urls { appDir :: FilePath, pkgVers :: [(String, String)] }
    deriving (Show, Data, Typeable)

cfgGet f = liftM f ask
