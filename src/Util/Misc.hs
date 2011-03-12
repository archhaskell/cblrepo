{-# LANGUAGE DeriveDataTypeable #-}
module Util.Misc where

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.Data
import Data.List
import Data.Typeable
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BS

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
    | AddPkg { appDir :: FilePath, dbFile :: FilePath, patchDir :: FilePath, dryRun :: Bool, cbls :: [FilePath] }
    | BuildPkgs { appDir :: FilePath, dbFile :: FilePath, pkgs :: [String] }
    | BumpPkgs { appDir :: FilePath, dbFile :: FilePath, pkgs :: [String] }
    | IdxSync { appDir :: FilePath }
    | IdxVersion { appDir :: FilePath, pkgs :: [String] }
    | ListPkgs { appDir :: FilePath, dbFile :: FilePath, incBase :: Bool }
    | Updates { appDir :: FilePath, dbFile :: FilePath }
    | Urls { appDir :: FilePath, pkgVers :: [(String, String)] }
    deriving (Show, Data, Typeable)

defAddBasePkg = AddBasePkg "" "" True []
defAddPkg = AddPkg "" "" "" True []
defBuildPkgs =  BuildPkgs "" "" []
defBumpPkgs =  BumpPkgs "" "" []
defIdxSync =  IdxSync ""
defIdxVersion =  IdxVersion "" []
defListPkgs =  ListPkgs "" "" True
defUpdates =  Updates "" ""
defUrls =  Urls "" []
defPkgBuild =  PkgBuild "" "" "" []

cfgGet f = liftM f ask

-- {{{1 URL and process stuff
getFromURL url fn = do
    (ec, _, er) <- readProcessWithExitCode "curl" ["-f", "-o", fn, url] ""
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr ("Failed downloading " ++ url)
            hPutStrLn stderr er
            exitFailure

-- {{{1 readCabal
data LocType = Url | Idx | File

-- | Read in a Cabal file.
readCabal patchDir loc tmpDir = let
        locType
            | isInfixOf "://" loc = Url
            | ',' `elem` loc = Idx
            | otherwise = File

        copyCabal tmpDir loc = copyFile loc fn >> return fn
            where fn = tmpDir </> takeFileName loc

        downloadCabal tmpDir loc = getFromURL loc fn >> return fn
            where
                fn = tmpDir </> takeFileName loc

        extractCabal tmpDir loc = let
                (p, (_: v)) = span (/= ',') loc
                path = p </> v </> p ++ ".cabal"
                pkgStr = p ++ " " ++ v
                fn = tmpDir </> (p ++ ".cabal")

                esFindEntry p (Next e es) = if p == (entryPath e)
                    then Just e
                    else esFindEntry p es
                esFindEntry _ _ = Nothing

                eGetContent e = let
                        ec = entryContent e
                    in case ec of
                        NormalFile c _ -> Just $ BS.unpack c
                        _ -> Nothing

            in do
                fp <- getAppUserDataDirectory "cblrepo"
                es <- liftM (Tar.read . GZip.decompress)
                    (BS.readFile $ fp </> "00-index.tar.gz")
                e <- maybe (error $ "No entry for " ++ pkgStr)
                    return
                    (esFindEntry path es)
                cbl <- maybe (error $ "Failed to extract contents for " ++ pkgStr)
                    return
                    (eGetContent e)
                writeFile fn cbl
                return fn

        extractName fn = liftM name $ readPackageDescription silent fn
            where
                packageName (PackageName s) = s
                name = packageName . pkgName . package . packageDescription

        applyPatch oFn pFn = do
            (ec, _, err) <- readProcessWithExitCode "patch" [oFn, pFn] ""
            case ec of
                ExitSuccess -> return ()
                ExitFailure _ ->
                    hPutStrLn stderr "Failed patching the .cabal file" >> exitFailure

    in do
        cblFn <- case locType of
            File -> copyCabal tmpDir loc
            Idx -> extractCabal tmpDir loc
            Url -> downloadCabal tmpDir loc
        pn <- extractName cblFn
        let patchFn = patchDir </> ("patch.cabal." ++ pn)
        return patchFn
        fileExist patchFn >>= flip when (applyPatch cblFn patchFn)
        readPackageDescription silent cblFn

-- {{{1 allPatches
allPatches pn patchDir = let
        cblPatch = patchDir </> "patch.cabal." ++ pn
        pkgPatch = patchDir </> "patch.pkgbuild." ++ pn
        bldPatch = patchDir </> "patch.build." ++ pn
    in do
        cE <- fileExist cblPatch
        pE <- fileExist pkgPatch
        bE <- fileExist bldPatch
        return (if cE then Just cblPatch else Nothing,
            if pE then Just pkgPatch else Nothing,
            if bE then Just bldPatch else Nothing)
