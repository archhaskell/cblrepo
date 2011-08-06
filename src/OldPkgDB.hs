{-
 - Copyright 2011 Per Magnus Therning
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

module OldPkgDB where

import Util.Misc

import Control.Applicative
import Control.Exception as CE
import Data.List
import Data.Maybe
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Version
import System.IO.Error
import Text.JSON
import qualified Distribution.Package as P
import qualified Distribution.Version as V

type CblPkg = (String, (V.Version, [P.Dependency], Int))
type CblDB = [CblPkg]

pkgName :: CblPkg -> String
pkgName (n, _) = n

pkgVersion :: CblPkg -> V.Version
pkgVersion (_, (v, _, _)) = v

pkgDeps :: CblPkg -> [P.Dependency]
pkgDeps (_, (_, ds, _)) = ds

pkgRelease :: CblPkg -> Int
pkgRelease (_, (_, _, i)) = i

createCblPkg :: PackageDescription -> CblPkg
createCblPkg pd = (name, (version, deps, 1))
    where
        name = (\ (P.PackageName n) -> n) (P.pkgName $ package pd)
        version = P.pkgVersion $ package pd
        deps = buildDepends pd

getDependencyOn :: String -> CblPkg -> Maybe P.Dependency
getDependencyOn n p = find (\ d -> depName d == n) (pkgDeps p)

isBasePkg :: CblPkg -> Bool
isBasePkg (_, (_, ds, _)) = null ds

emptyPkgDB :: CblDB
emptyPkgDB = []

addPkg :: CblDB -> String -> V.Version -> [P.Dependency] -> Int -> CblDB
addPkg db n v ds r = nubBy cmp newdb
    where
        cmp (n1, _) (n2, _) = n1 == n2
        newdb = (n, (v, ds, r)) : db

addPkg2 db (n, (v, ds, r)) = addPkg db n v ds r

addBasePkg db n v = addPkg db n v [] 0

delPkg :: CblDB -> String -> CblDB
delPkg db n = filter (\ p -> n /= pkgName p) db

lookupPkg :: CblDB -> String -> Maybe CblPkg
lookupPkg db n = maybe Nothing (\ s -> Just (n, s)) (lookup n db)

lookupDependencies :: CblDB -> String -> Maybe [String]
lookupDependencies db n =
    case lookupPkg db n of
        Nothing -> Nothing
        Just p -> let
                ds = pkgDeps p
            in Just $ map depName ds

lookupDependants :: CblDB -> String -> [String]
lookupDependants db n = map pkgName $ filter (\ p -> doesDependOn p n) db
    where
        doesDependOn p n = n `elem` (map depName $ pkgDeps p)

checkDependants db n v = let
        d1 = catMaybes $ map (lookupPkg db) (lookupDependants db n)
        d2 = map (\ p -> (pkgName p, getDependencyOn n p)) d1
        fails = filter (not . withinRange v . depVersionRange . fromJust . snd) d2
    in fails

transitiveDependants db pkgs = keepLast $ concat $ map transUsersOfOne pkgs
    where
        transUsersOfOne pkg = pkg : (keepLast $ concat $ map (transUsersOfOne) (lookupDependants db pkg))
        keepLast = reverse . nub . reverse

lookupRelease :: CblDB -> String -> Maybe Int
lookupRelease db n = lookupPkg db n >>= return . pkgRelease

bumpRelease db n = let
        bump (n', (v', d', r')) = (n', (v', d', r' + 1))
    in maybe db (addPkg2 db . bump) (lookupPkg db n)
readDb :: FilePath -> IO CblDB
readDb fp = (flip CE.catch)
    (\ e -> if isDoesNotExistError e
        then return emptyPkgDB
        else throwIO e)
    $ do
        r <- readFile fp >>= return . sequence . map decode . lines
        case r of
            Ok a -> return a
            Error s -> fail s

saveDb :: CblDB -> FilePath -> IO ()
saveDb db fp = writeFile fp s
    where
        s = unlines $ map (encode . showJSON) db
