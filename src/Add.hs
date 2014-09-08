{-
 - Copyright 2011-2014 Per Magnus Therning
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

module Add where

-- {{{1 imports
-- {{{2 local
import PkgDB
import Util.Misc

-- {{{2 system
import Control.Monad.Error
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Version
import qualified Distribution.Package as P
import Control.Arrow
import Data.Monoid

-- {{{1 types
data PkgType
    = GhcType String Version
    | DistroType String Version String
    | RepoType GenericPackageDescription
    deriving (Eq, Show)

-- {{{1 add
add :: Command ()
add = do
    dbFn <- optGet  dbFile
    db <- liftIO $ readDb dbFn
    ad <- optGet appDir
    pd <- optGet  $ patchDir . optsCmd
    dr <- optGet  dryRun
    ghcVersion <- optGet $ ghcVer . optsCmd
    idxPkgs <- optGet $ cmdAddCbls . optsCmd
    --
    ghcPkgs <- optGet  $ cmdAddGhcPkgs . optsCmd
    distroPkgs <- optGet  $ cmdAddDistroPkgs . optsCmd
    genFilePkgs <- optGet (cmdAddFileCbls . optsCmd) >>= mapM ((runErrorT . withTempDirErrT "/tmp/cblrepo." . readCabalFromFile ad pd) . fst)
    genIdxPkgs <- mapM ((runErrorT . withTempDirErrT "/tmp/cblrepo." . readCabalFromIdx ad pd) . (\ (a, b, _) -> (a, b))) idxPkgs
    pkgs <- exitOnErrors $ argsToPkgType ghcPkgs distroPkgs (genFilePkgs ++ genIdxPkgs)
    --
    let pkgNames = map getName pkgs
        tmpDb = foldl delPkg db pkgNames
        oldFlags = map (maybe ([], []) (pkgName &&& pkgFlags) . lookupPkg db . getName) pkgs
        argFlags = map (\ (a, _, b) -> (a, b)) idxPkgs
        flags = combineFlags argFlags oldFlags
    case addPkgs ghcVersion tmpDb flags pkgs of
        Left (unsatisfiables, breaksOthers) -> liftIO (mapM_ printUnSat unsatisfiables >> mapM_ printBrksOth breaksOthers)
        Right newDb -> liftIO $ unless dr $ saveDb newDb dbFn

argsToPkgType ghcPkgs distroPkgs repoPkgs = let
        toGhcType (n, v) = maybe
            (Left $ "Not a valid version given for " ++ n ++ " (" ++ v ++ ")")
            (Right . GhcType n)
            (simpleParse v :: Maybe Version)
        toDistroType (n, v, r) = maybe
            (Left $ "Not a valid version given for " ++ n ++ " (" ++ v ++ ")")
            (\ v' -> Right $ DistroType n v' r)
            (simpleParse v :: Maybe Version)
        toRepoType = either Left (Right . RepoType)
    in map toGhcType ghcPkgs ++ map toDistroType distroPkgs ++ map toRepoType repoPkgs

getName (GhcType n _) = n
getName (DistroType n _ _) = n
getName (RepoType gpd) = (\ (P.PackageName n) -> n) $ P.pkgName $ package $ packageDescription gpd

-- {{{1 addPkgs
addPkgs :: Version -> CblDB -> [(String, FlagAssignment)] -> [PkgType] -> Either ([(String, [P.Dependency])], [((String, Version), [(String, Maybe P.Dependency)])]) CblDB
addPkgs ghcVer db flags pkgs = let
        (succs, fails) = partition (canBeAdded ghcVer db flags) pkgs
        newDb = foldl addPkg2 db (map (pkgTypeToCblPkg ghcVer db flags) succs)
        unsatisfieds = mapMaybe (finalizeToUnsatisfiableDeps ghcVer db flags) fails
        breaksOthers = mapMaybe (findBreaking db) fails
    in case (succs, fails) of
        (_, []) -> Right newDb
        ([], _) -> Left (unsatisfieds, breaksOthers)
        (_, _) -> addPkgs ghcVer newDb flags fails

canBeAdded :: Version -> CblDB -> [(String, FlagAssignment)] -> PkgType -> Bool
canBeAdded _ db _ (GhcType n v) = null $ checkDependants db n v
canBeAdded _ db _ (DistroType n v _) = null $ checkDependants db n v
canBeAdded ghcVer db flags pkg@(RepoType gpd) =  finable && depsOK
    where
        fa = fromMaybe [] $ lookup (getName pkg) flags
        finable = either (const False) (const True) (finalizePkg ghcVer db fa gpd)
        n = ((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) gpd
        v = P.pkgVersion $ package $ packageDescription gpd
        depsOK = null $ checkDependants db n v

pkgTypeToCblPkg _ _ _ (GhcType n v) = createGhcPkg n v
pkgTypeToCblPkg _ _ _ (DistroType n v r) = createDistroPkg n v r
pkgTypeToCblPkg ghcVer db flags pkg@(RepoType gpd) =
    let fa = fromMaybe [] $ lookup (getName pkg) flags
    in fromJust $ case finalizePkg ghcVer db fa gpd of
       Right (pd, fa) -> Just $ createCblPkg pd fa
       Left _ -> Nothing

finalizeToUnsatisfiableDeps ghcVer db flags pkg@(RepoType gpd) =
    let fa = fromMaybe [] $ lookup (getName pkg) flags
    in case finalizePkg ghcVer db fa gpd of
       Left ds -> Just (((\ (P.PackageName n) -> n ) . P.pkgName . package . packageDescription) gpd, ds)
       _ -> Nothing

finalizeToUnsatisfiableDeps _ _ _ _ = Nothing

findBreaking db (GhcType n v) = let
        d = checkDependants db n v
    in if null d
        then Nothing
        else Just ((n, v), d)
findBreaking db (DistroType n v _) = let
        d = checkDependants db n v
    in if null d
        then Nothing
        else Just ((n, v), d)
findBreaking db (RepoType gpd) = let
        n = (\ (P.PackageName n) -> n) $ P.pkgName $ package $ packageDescription gpd
        v = P.pkgVersion $ package $ packageDescription gpd
        d = checkDependants db n v
    in if null d
        then Nothing
        else Just ((n, v), d)

combineFlags a b = zip keys $ mapMaybe (uncurry mappend . (\ k -> (lookup k a, lookup k b))) keys
    where
        keys = nub $ map fst (a ++ b)
