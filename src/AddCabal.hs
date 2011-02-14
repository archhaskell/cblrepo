module AddCabal where

import PkgDB
import Utils

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
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
import Network.Download
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Distribution.Package as P

addCabal :: ReaderT Cmds IO ()
addCabal = do
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    cbls <- cfgGet cbls
    dR <- cfgGet dryRun
    genPkgs <- liftIO $ mapM readCabal cbls
    let pkgNames = map ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) genPkgs
    let tmpDb = filter (\ p -> not $ pkgName p `elem` pkgNames) db
    case doAddCabal tmpDb genPkgs of
        Left (unSats, brksOthrs) -> liftIO (mapM_ printUnSat unSats >> mapM_ printBrksOth brksOthrs)
        Right newDb -> liftIO $ unless dR $ saveDb newDb dbFn

data LocType = Url | Idx | File

readCabal loc = let
        locType
            | isInfixOf "://" loc = Url
            | ',' `elem` loc = Idx
            | otherwise = File

        readFile = readPackageDescription silent loc

        readURI = do
            r <- openURIString loc
            case r of
                Left e -> error e
                Right cbl -> case parsePackageDescription cbl of
                    ParseFailed e -> error $ show e
                    ParseOk _ pd -> return pd
        readIdx = let
                (p, (_: v)) = span (/= ',') loc
                path = p </> v </> p ++ ".cabal"
                pkgStr = p ++ " " ++ v

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
                case parsePackageDescription cbl of
                    ParseFailed e -> error $ show e
                    ParseOk _ pd -> return pd

    in case locType of
        Url -> readURI
        File -> readFile
        Idx -> readIdx

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
