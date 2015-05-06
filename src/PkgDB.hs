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

{-# LANGUAGE TemplateHaskell #-}

module PkgDB
    ( CblPkg
    , pkgName
    , pkgPkg
    , pkgVersion
    , pkgXRev
    , pkgDeps
    , pkgFlags
    , pkgRelease
    --
    , isGhcPkg
    , isDistroPkg
    , isRepoPkg
    , isBasePkg
    --
    , createGhcPkg
    , createDistroPkg
    , createRepoPkg
    , createCblPkg
    --
    , CblDB
    , addPkg
    , addPkg2
    , delPkg
    , bumpRelease
    , lookupPkg
    , transitiveDependants
    , checkDependants
    , checkAgainstDb
    , saveDb
    , readDb
    ) where

-- {{{1 imports
import Control.Arrow
import Control.Exception as CE
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Distribution.PackageDescription
import System.IO.Error
import qualified Distribution.Package as P
import qualified Distribution.Version as V

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..), SumEncoding(..))
import qualified Data.ByteString.Lazy.Char8 as C

import qualified Util.Dist

-- {{{1 types
data Pkg
    = GhcPkg GhcPkgD
    | DistroPkg DistroPkgD
    | RepoPkg RepoPkgD
    deriving (Eq, Show)

data GhcPkgD = GhcPkgD { gpVersion :: V.Version }
    deriving (Eq, Show)

data DistroPkgD = DistroPkgD
    { dpVersion :: V.Version
    , dpXrev :: Int
    , dpRelease :: Int
    } deriving (Eq, Show)

data RepoPkgD = RepoPkgD
    { rpVersion :: V.Version
    , rpXrev :: Int
    , rpDeps :: [P.Dependency]
    , rpFlags :: FlagAssignment, rpRelease :: Int
    } deriving (Eq, Show)

data CblPkg = CP String Pkg
    deriving (Eq, Show)

type CblDB = [CblPkg]

instance Ord CblPkg where
    compare (CP n1 (GhcPkg d1)) (CP n2 (GhcPkg d2)) =
            compare (n1, gpVersion d1) (n2, gpVersion d2)
    compare (CP _ GhcPkg {}) _ = LT
    compare _ (CP _ GhcPkg {}) = GT

    compare (CP n1 (DistroPkg d1)) (CP n2 (DistroPkg d2)) =
            compare (n1, dpVersion d1, dpRelease d1) (n2, dpVersion d2, dpRelease d2)
    compare (CP _ DistroPkg {}) _ = LT
    compare _ (CP _ DistroPkg {}) = GT

    compare (CP n1 (RepoPkg d1)) (CP n2 (RepoPkg d2)) =
            compare (n1, rpVersion d1, rpRelease d1) (n2, rpVersion d2, rpRelease d2)

-- {{{1 packages
pkgName :: CblPkg -> String
pkgName (CP n _) = n

pkgPkg :: CblPkg -> Pkg
pkgPkg (CP _ p) = p

pkgVersion :: CblPkg -> V.Version
pkgVersion (CP _ (GhcPkg d)) = gpVersion d
pkgVersion (CP _ (DistroPkg d)) = dpVersion d
pkgVersion (CP _ (RepoPkg d)) = rpVersion d

pkgXRev :: CblPkg -> Int
pkgXRev (CP _ (DistroPkg d)) = dpXrev d
pkgXRev (CP _ (RepoPkg d)) = rpXrev d
pkgXRev _ = 0

pkgDeps :: CblPkg -> [P.Dependency]
pkgDeps (CP _ (RepoPkg d)) = rpDeps d
pkgDeps _ = []

pkgFlags :: CblPkg -> FlagAssignment
pkgFlags (CP _ (RepoPkg d)) = rpFlags d
pkgFlags _ = []

pkgRelease :: CblPkg -> Int
pkgRelease (CP _ (GhcPkg _)) = (-1)
pkgRelease (CP _ (DistroPkg d)) = dpRelease d
pkgRelease (CP _ (RepoPkg d)) = rpRelease d

createGhcPkg :: String -> V.Version -> CblPkg
createGhcPkg n v = CP n (GhcPkg $ GhcPkgD v)

createDistroPkg :: String -> V.Version -> Int -> Int -> CblPkg
createDistroPkg n v x r = CP n (DistroPkg (DistroPkgD v x r))

createRepoPkg :: String -> V.Version -> Int -> [P.Dependency] -> FlagAssignment -> Int -> CblPkg
createRepoPkg n v x d fa r = CP n (RepoPkg $ RepoPkgD v x d fa r)

createCblPkg :: PackageDescription -> FlagAssignment -> CblPkg
createCblPkg pd fa = createRepoPkg name version xrev deps fa 1
    where
        name = Util.Dist.pkgNameStr pd
        version = P.pkgVersion $ package pd
        xrev = Util.Dist.pkgXRev pd
        deps = buildDepends pd

getDependencyOn :: String -> CblPkg -> Maybe P.Dependency
getDependencyOn n p = find (\ d -> Util.Dist.depName d == n) (pkgDeps p)

isGhcPkg :: CblPkg -> Bool
isGhcPkg (CP _ GhcPkg {}) = True
isGhcPkg _ = False

isDistroPkg :: CblPkg -> Bool
isDistroPkg (CP _ DistroPkg {}) = True
isDistroPkg _ = False

isRepoPkg :: CblPkg -> Bool
isRepoPkg (CP _ RepoPkg {}) = True
isRepoPkg _ = False

isBasePkg :: CblPkg -> Bool
isBasePkg = not . isRepoPkg

-- {{{1 database
emptyPkgDB :: CblDB
emptyPkgDB = []

addPkg :: CblDB -> String -> Pkg -> CblDB
addPkg db n p = nubBy cmp newdb
    where
        cmp (CP n1 _) (CP n2 _) = n1 == n2
        newdb = CP n p:db

addPkg2 :: CblDB -> CblPkg -> CblDB
addPkg2 db (CP n p) = addPkg db n p

addGhcPkg :: CblDB -> String -> V.Version -> CblDB
addGhcPkg db n v = addPkg2 db (createGhcPkg n v)

addDistroPkg :: CblDB -> String -> V.Version -> Int -> Int -> CblDB
addDistroPkg db n v x r = addPkg2 db (createDistroPkg n v x r)

delPkg :: CblDB -> String -> CblDB
delPkg db n = filter (\ p -> n /= pkgName p) db

bumpRelease :: CblDB -> String -> CblDB
bumpRelease db n = maybe db (addPkg2 db . doBump) (lookupPkg db n)
    where
        doBump (CP n' (RepoPkg d)) = CP n' (RepoPkg d { rpRelease = (rpRelease d + 1) })
        doBump p = p

lookupPkg :: CblDB -> String -> Maybe CblPkg
lookupPkg [] _ = Nothing
lookupPkg (p:db) n
    | n == pkgName p = Just p
    | otherwise = lookupPkg db n

lookupDependants :: [CblPkg] -> String -> [String]
lookupDependants db n = filter (/= n) $ map pkgName $ filter (`doesDependOn` n) db
    where
        doesDependOn p n = n `elem` map Util.Dist.depName (pkgDeps p)

transitiveDependants :: CblDB -> [String] -> [String]
transitiveDependants db names = keepLast $ concatMap transUsersOfOne names
    where
        transUsersOfOne n = n : transitiveDependants db (lookupDependants db n)
        keepLast = reverse . nub . reverse

checkDependants :: CblDB -> String -> V.Version -> [(String, Maybe P.Dependency)]
checkDependants db n v = let
        d1 = mapMaybe (lookupPkg db) (lookupDependants db n)
        -- d2 = map (\ p -> (pkgName p, getDependencyOn n p)) d1
        d2 = map (pkgName &&& getDependencyOn n ) d1
        fails = filter (not . V.withinRange v . Util.Dist.depVersionRange . fromJust . snd) d2
    in fails

checkAgainstDb :: CblDB -> String -> P.Dependency -> Bool
checkAgainstDb db name dep = let
        dN = Util.Dist.depName dep
        dVR = Util.Dist.depVersionRange dep
    in (dN == name) ||
            (case lookupPkg db dN of
                Nothing -> False
                Just p -> V.withinRange (pkgVersion p) dVR)

readDb :: FilePath -> IO CblDB
readDb fp = handle
    (\ e -> if isDoesNotExistError e
        then return emptyPkgDB
        else throwIO e)
    $ do
        r <- (mapM decode . C.lines) `liftM` C.readFile fp
        case r of
            Just a -> return a
            Nothing -> fail "JSON parsing failed"

saveDb :: CblDB -> FilePath -> IO ()
saveDb db fp = C.writeFile fp s
    where
        s = C.unlines $ map encode $ sort db

-- {{{1 JSON instances
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''V.Version)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''V.VersionRange)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''FlagName)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''Pkg)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''GhcPkgD)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''DistroPkgD)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''RepoPkgD)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''CblPkg)

instance ToJSON P.Dependency where
    toJSON (P.Dependency pn vr) = toJSON (P.unPackageName pn, vr)

instance FromJSON P.Dependency where
    parseJSON v = do
        (pn, vr) <- parseJSON v
        return $ P.Dependency (P.PackageName pn) vr
