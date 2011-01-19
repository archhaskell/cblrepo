module AddCabal where

import PkgDB
import SystemPkgs
import Utils

import Data.List
import Data.Version
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import qualified Distribution.Package as P
import Data.Maybe

addCabal dbFp cbls = do
    db <- readDb dbFp
    genPkgs <- mapM readCabalFile cbls
    let pkgNames = map ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) genPkgs
    let tmpDb = filter (\ p -> not $ pkgName p `elem` pkgNames) db
    case doAddCabal tmpDb genPkgs of
        Left (unSats, brksOthrs) -> do
            mapM_ printUnSat unSats
            mapM_ printBrksOth brksOthrs
        Right newDb -> do
            putStrLn "Success"
            saveDB newDb dbFp

readCabalFile = readPackageDescription silent

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
