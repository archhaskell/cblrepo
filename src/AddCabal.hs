module AddCabal where

import PkgDB
import Utils

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Version
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Distribution.Package as P
import System.Posix.Files
import System.Unix.Directory
import System.Process
import System.Exit
import System.IO

addCabal :: ReaderT Cmds IO ()
addCabal = do
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    cbls <- cfgGet cbls
    dR <- cfgGet dryRun
    genPkgs <- liftIO $ mapM (\ c -> withTemporaryDirectory "/tmp/cblrepo." (readCabal c)) cbls
    let pkgNames = map ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) genPkgs
    let tmpDb = filter (\ p -> not $ pkgName p `elem` pkgNames) db
    case doAddCabal tmpDb genPkgs of
        Left (unSats, brksOthrs) -> liftIO (mapM_ printUnSat unSats >> mapM_ printBrksOth brksOthrs)
        Right newDb -> liftIO $ unless dR $ saveDb newDb dbFn

data LocType = Url | Idx | File

readCabal loc tmpDir = let
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
                packageName (P.PackageName s) = s
                name = packageName . P.pkgName . package . packageDescription

        applyPatch oFn pFn = do
            (ec, _, err) <- readProcessWithExitCode "patch" [oFn, pFn] ""
            case ec of
                ExitSuccess -> return ()
                ExitFailure _ -> hPutStrLn stderr "Failed patching the .cabal file" >> exitFailure

    in do
        fn <- case locType of
            File -> copyCabal tmpDir loc
            Idx -> extractCabal tmpDir loc
            Url -> downloadCabal tmpDir loc
        pn <- extractName fn
        let patchName = "patch.cabal." ++ pn
        doPatch <- fileExist patchName
        if doPatch
            then applyPatch fn patchName
            else return ()
        readPackageDescription silent fn

doAddCabal db pkgs = let
        (succs, fails) = partition (canBeAdded db) pkgs
        newDb = foldl addPkg2 db (map (fromJust . finalizeToCblPkg db) succs)
        unSats = catMaybes $ map (finalizeToDeps db) fails
        genPkgName = ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription)
        genPkgVer = P.pkgVersion . package . packageDescription
        brksOthrs = filter (not . null . snd) $ map (\ p -> ((genPkgName p, genPkgVer p), checkDependants db (genPkgName p) (genPkgVer p))) fails
    in case (succs, fails) of
        (_, []) -> Right newDb
        ([], _) -> Left (unSats, brksOthrs)
        (_, _) -> doAddCabal newDb fails


canBeAdded db p = let
        finable = either (const False) (const True) (finalizePkg db p)
        n = ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) p
        v = P.pkgVersion $ package $ packageDescription p
        depsOK = null $ checkDependants db n v
    in finable && depsOK

finalizeToCblPkg db p = case finalizePkg db p of
    Right (pd, _) -> Just $ createCblPkg pd
    _ -> Nothing

finalizeToDeps db p = case finalizePkg db p of
    Left ds -> Just $ (((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) p, ds)
    _ -> Nothing

finalizePkg db = finalizePackageDescription
    [] -- no flags
    (checkAgainstDb db)
    (Platform X86_64 buildOS) -- platform
    (CompilerId GHC (Version [6,12,3] []))  -- compiler version
    [] -- no additional constraints

checkAgainstDb db dep = let
        dN = depName dep
        dVR = depVersionRange dep
    in case lookupPkg db dN of
        Nothing -> False
        Just (_, (v, _)) -> withinRange v dVR
