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

{-# LANGUAGE TemplateHaskell #-}

module PkgDB where

-- {{{1 imports
import Control.Arrow
import Control.Exception as CE
import Control.Monad
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import System.IO.Error
import qualified Distribution.Package as P
import qualified Distribution.Version as V

import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..), SumEncoding(..))
import qualified Data.ByteString.Lazy.Char8 as C

-- {{{ temporary
_depName (P.Dependency (P.PackageName n) _) = n
_depVersionRange (P.Dependency _ vr) = vr

-- {{{1 types
data Pkg
    = GhcPkg { version :: V.Version }
    | DistroPkg { version :: V.Version, release :: String }
    | RepoPkg { version :: V.Version, deps :: [P.Dependency], flags :: FlagAssignment, release :: String }
    deriving (Eq, Show)

data CblPkg = CP String Pkg
    deriving (Eq, Show)

type CblDB = [CblPkg]

instance Ord CblPkg where
    compare
        (CP n1 GhcPkg { version = v1 })
        (CP n2 GhcPkg { version = v2 }) =
            compare (n1, v1) (n2, v2)
    compare (CP _ GhcPkg {}) _ = LT
    compare _ (CP _ GhcPkg {}) = GT

    compare
        (CP n1 DistroPkg { version = v1, release = r1 })
        (CP n2 DistroPkg { version = v2, release = r2 }) =
            compare (n1, v1, r1) (n2, v2, r2)
    compare (CP _ DistroPkg {}) _ = LT
    compare _ (CP _ DistroPkg {}) = GT

    compare
        (CP n1 RepoPkg { version = v1, release = r1 })
        (CP n2 RepoPkg { version = v2, release = r2 }) =
            compare (n1, v1, r1) (n2, v2, r2)

-- {{{1 packages
pkgName :: CblPkg -> String
pkgName (CP n _) = n

pkgPkg :: CblPkg -> Pkg
pkgPkg (CP _ p) = p

pkgVersion :: CblPkg -> V.Version
pkgVersion (CP _ p) = version p

pkgDeps :: CblPkg -> [P.Dependency]
pkgDeps (CP _ RepoPkg { deps = d}) = d
pkgDeps _ = []

pkgFlags :: CblPkg -> FlagAssignment
pkgFlags (CP _ RepoPkg { flags = fa}) = fa
pkgFlags _ = []

pkgRelease :: CblPkg -> String
pkgRelease (CP _ GhcPkg {}) = "xx"
pkgRelease (CP _ DistroPkg { release = r }) = r
pkgRelease (CP _ RepoPkg { release = r }) = r

createGhcPkg n v = CP n (GhcPkg v)
createDistroPkg n v r = CP n (DistroPkg v r)
createRepoPkg n v d fa r = CP n (RepoPkg v d fa r)

createCblPkg :: PackageDescription -> FlagAssignment -> CblPkg
createCblPkg pd fa = createRepoPkg name version deps fa "1"
    where
        name = (\ (P.PackageName n) -> n) (P.pkgName $ package pd)
        version = P.pkgVersion $ package pd
        deps = buildDepends pd

getDependencyOn :: String -> CblPkg -> Maybe P.Dependency
getDependencyOn n p = find (\ d -> _depName d == n) (pkgDeps p)

isGhcPkg (CP _ GhcPkg {}) = True
isGhcPkg _ = False

isDistroPkg (CP _ DistroPkg {}) = True
isDistroPkg _ = False

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

addDistroPkg :: CblDB -> String -> V.Version -> String -> CblDB
addDistroPkg db n v r = addPkg2 db (createDistroPkg n v r)

delPkg :: CblDB -> String -> CblDB
delPkg db n = filter (\ p -> n /= pkgName p) db

bumpRelease :: CblDB -> String -> CblDB
bumpRelease db n = let
        doBump (CP n' p@RepoPkg { release = r }) = CP n' (p { release = nr })
            where
                nr = show $ read r + (1 :: Int)
        doBump p = p
    in maybe db (addPkg2 db . doBump) (lookupPkg db n)

lookupPkg :: CblDB -> String -> Maybe CblPkg
lookupPkg [] _ = Nothing
lookupPkg (p:db) n
    | n == pkgName p = Just p
    | otherwise = lookupPkg db n

lookupDependants db n = filter (/= n) $ map pkgName $ filter (`doesDependOn` n) db
    where
        doesDependOn p n = n `elem` map _depName (pkgDeps p)

transitiveDependants :: CblDB -> [String] -> [String]
transitiveDependants db names = keepLast $ concatMap transUsersOfOne names
    where
        transUsersOfOne n = n : transitiveDependants db (lookupDependants db n)
        keepLast = reverse . nub . reverse

-- Todo: test
checkDependants :: CblDB -> String -> V.Version -> [(String, Maybe P.Dependency)]
checkDependants db n v = let
        d1 = mapMaybe (lookupPkg db) (lookupDependants db n)
        -- d2 = map (\ p -> (pkgName p, getDependencyOn n p)) d1
        d2 = map (pkgName &&& getDependencyOn n ) d1
        fails = filter (not . V.withinRange v . _depVersionRange . fromJust . snd) d2
    in fails

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
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''P.Dependency)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''P.PackageName)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''FlagName)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''Pkg)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''CblPkg)
