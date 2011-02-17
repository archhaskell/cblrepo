{-# LANGUAGE DeriveDataTypeable #-}
module Utils where

import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Data
import Data.Typeable
import Distribution.Package
import Distribution.Text
import System.Exit
import System.IO
import System.Process
import qualified Control.Exception as CE

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

-- {{{1 URL and process stuff
-- taken from cabal2arch
getFromURL :: String -> FilePath -> ErrorT String IO String
getFromURL url fn = liftIO (myReadProcess "curl" ["-f", "-o", fn, url] "") >>= (\ r ->
    either
    (\ _ -> throwError $ "Unable to retrieve " ++ url)
    (\ s -> liftIO $ return s)
    r)

myReadProcess :: FilePath -- ^ command to run
    -> [String] -- ^ any arguments
    -> String -- ^ standard input
    -> IO (Either (ExitCode, String, String) String) -- ^ either the stdout, or an exitcode and any output
myReadProcess cmd _args input = CE.handle (return . handler) $ do
    (inh, outh, errh, pid) <- runInteractiveProcess cmd _args Nothing Nothing

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ (CE.evaluate (length output) >> putMVar outMVar ())

    errput  <- hGetContents errh
    errMVar <- newEmptyMVar
    _ <- forkIO $ (CE.evaluate (length errput) >> putMVar errMVar ())

    when (not (null input)) $ hPutStr inh input
    takeMVar outMVar
    takeMVar errMVar
    ex <- CE.catch (waitForProcess pid) ((const :: a -> CE.SomeException -> a) $ return ExitSuccess)
    hClose outh
    hClose inh -- done with stdin
    hClose errh -- ignore stderr

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left (ex, errput, output)

    where
        handler (ExitFailure e) = Left (ExitFailure e,"","")
        handler e = Left (ExitFailure 1, show e, "")
