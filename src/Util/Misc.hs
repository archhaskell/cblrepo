{-
 - Copyright 2011-2013 Per Magnus Therning
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module Util.Misc where

-- {{{1 imports
import qualified PkgDB as DB

import Control.Exception (onException)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Either
import Data.Version
import Distribution.Compiler
import Distribution.Package as P
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Options.Applicative
import Options.Applicative.Types
import Safe (lastMay)
import System.Exit
import System.IO
import System.Posix.Files
import System.Process
import System.Unix.Directory
import Text.ParserCombinators.ReadP hiding (many)

-- {{{1 dependency
depName (Dependency (PackageName n) _) = n
depVersionRange (Dependency _ vr) = vr

-- {{{ print functions
printUnSat (n, ds) = do
    putStrLn $ "Failed to satisfy the following dependencies for " ++ n ++ ":"
    mapM_ (putStrLn . ("  " ++) . display) ds

printBrksOth  ((n, v), brks) = do
    putStrLn $ "Adding " ++ n ++ " " ++ display v ++ " would break:"
    mapM_ (\ (bN, Just bD) -> putStrLn $ "  " ++ bN ++ " : " ++ display bD) brks

-- {{{1 program variables
progName = "cblrepo"
dbName = progName ++ ".db"

ghcDefVersion = Version [7, 8, 3] []
ghcVersionDep :: Version -> Int -> String
ghcVersionDep ghcVer ghcRel = "ghc=" ++ display ghcVer ++ "-" ++ show ghcRel

indexUrl = "http://hackage.haskell.org/packages/index.tar.gz"
indexFileName = "index.tar.gz"

-- {{{1 command line parser helpers
readPkgNVersion :: ReadP (String, Version)
readPkgNVersion = do
    n <- many (satisfy (/= ','))
    char ','
    v <- parseVersion
    return (n, v)

readFlag :: ReadP (FlagName, Bool)
readFlag = readNegFlag <++ readPosFlag
    where
        readNegFlag = do
            char '-'
            n <- many (satisfy (/= ','))
            return (FlagName n, False)

        readPosFlag = do
            n0 <- get
            n <- many (satisfy (/= ','))
            return (FlagName (n0 : n), True)

ghcVersionArgReader :: ReadM Version
ghcVersionArgReader = do
    arg <- readerAsk
    case lastMay $ readP_to_S parseVersion arg of
        Just (v, "") -> return v
        _ -> fail $ "cannot parse value `" ++ arg ++ "`"

pkgNVersionArgReader :: ReadM (String, Version)
pkgNVersionArgReader = do
    s <- readerAsk
    case lastMay (readP_to_S readPkgNVersion s) of
        Just (r, "") -> return r
        _ -> fail $ "Cannot parse '" ++ s ++ "' as PKG,VER"

ghcPkgArgReader :: ReadM (String, Version)
ghcPkgArgReader = pkgNVersionArgReader

distroPkgArgReader :: ReadM (String, Version, String)
distroPkgArgReader = let
        readDistroPkg = do
            (n, v) <- readPkgNVersion
            char ','
            r <- many (satisfy (/= ','))
            return (n, v, r)

    in do
        s <- readerAsk
        case lastMay (readP_to_S readDistroPkg s) of
            Just (r, "") -> return r
            _ -> fail $ "Cannot parse '" ++ s ++ "' as PKG,VER,REL"

strCblFileArgReader :: ReadM (FilePath, FlagAssignment)
strCblFileArgReader = let
        readWithFlags = do
            fn <- many $ satisfy (/= ':')
            char ':'
            fas <- sepBy readFlag (char ',')
            return (fn, fas)

        readWithoutFlags = do
            fn <- many $ satisfy (/= ':')
            return (fn, [])

    in do
        s <-readerAsk
        case lastMay (readP_to_S (readWithFlags <++ readWithoutFlags) s) of
            Just (r, "") -> return r
            _ -> fail $ "Cannot parse '" ++ s ++ "' as FILE[:FLAG,-FLAG,..]"

strCblPkgArgReader :: ReadM (String, Version, FlagAssignment)
strCblPkgArgReader = let
        readWithFlags = do
            (n, v) <- readPkgNVersion
            char ':'
            fas <- sepBy readFlag (char ',')
            return (n, v, fas)

        readWithoutFlags = do
            (n, v) <- readPkgNVersion
            return (n, v, [])

    in do
        s <- readerAsk
        case lastMay (readP_to_S (readWithFlags <++ readWithoutFlags) s) of
            Just (r, "") -> return r
            _ -> fail $ "Cannot parse: " ++ s

-- Helper for grabbing things out of the options
optGet :: MonadReader o m => (o -> s) -> m s
optGet f = liftM f ask

-- {{{1 command line argument type
data Cmds
    = CmdAdd
        { patchDir :: FilePath, ghcVer :: Version, cmdAddGhcPkgs :: [(String, Version)]
        , cmdAddDistroPkgs :: [(String, Version, String)], cmdAddFileCbls :: [(FilePath, FlagAssignment)]
        , cmdAddCbls :: [(String, Version, FlagAssignment)] }
    | CmdBuildPkgs { pkgs :: [String] }
    | CmdBumpPkgs { inclusive :: Bool, pkgs :: [String] }
    | CmdSync { unused :: Bool }
    | CmdVersions { latest :: Bool, pkgs :: [String] }
    | CmdListPkgs
        { listGhc :: Bool, listDistro :: Bool, noListRepo :: Bool
        , hackageFmt :: Bool, pkgs :: [String] }
    | CmdUpdates { idxStyle :: Bool }
    | CmdPkgBuild { ghcVer :: Version, ghcRel :: Int, patchDir :: FilePath, pkgs :: [String] }
    | CmdConvertDb { inDbFile :: FilePath, outDbFile :: FilePath }
    | CmdRemovePkg { pkgs :: [String] }
    | CmdExtract { cmdExtractPkgs :: [(String, Version)] }
    deriving (Show)

data Opts = Opts
    { appDir :: FilePath
    , dbFile :: FilePath
    , dryRun :: Bool
    , optsCmd :: Cmds
    } deriving (Show)

-- {{{1 getFromURL
getFromURL url fn = do
    (ec, _, er) <- readProcessWithExitCode "curl" ["-f", "-L", "-o", fn, url] ""
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr ("Failed downloading " ++ url)
            hPutStrLn stderr er
            exitFailure

-- {{{1 applyPatchIfExist
applyPatch origFilename patchFilename = do
    (ec, _, _) <- liftIO $ readProcessWithExitCode "patch" [origFilename, patchFilename] ""
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ ->
            throwError ("Failed patching " ++ origFilename ++ " with " ++ patchFilename)

applyPatchIfExist origFilename patchFilename =
    liftIO (fileExist patchFilename) >>= flip when (applyPatch origFilename patchFilename)

-- {{{1 finalising package descriptions
finalizePkg ghcVersion db fa gpd = let
        n = ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) gpd
    in finalizePackageDescription
        fa
        (DB.checkAgainstDb db n)
        (Platform X86_64 buildOS) -- platform
        (CompilerId GHC ghcVersion)  -- compiler version
        [] -- no additional constraints
        gpd

-- {{{1 Command type
type Command = ReaderT Opts IO

runCommand cmds func = runReaderT func cmds

-- {{{1 ErrorT
reThrowError :: MonadError a m => Either a b -> m b
reThrowError = either throwError return

withTempDirErrT :: (MonadError e m, MonadIO m) => FilePath -> (FilePath -> ErrorT e IO b) -> m b
withTempDirErrT fp func = do
    r <- liftIO $ withTemporaryDirectory fp (runErrorT . func)
    reThrowError r

exitOnErrors vs = let
        es = lefts vs
    in
        if not $ null $ lefts vs
            then liftIO $ mapM_ (hPutStrLn stderr) es >> exitFailure
            else return (rights vs)

exitOnException msg a = onException a $ hPutStrLn stderr msg >> exitFailure
