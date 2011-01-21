module PkgDB where

import Utils

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

type CblPkg = (String, (V.Version, [P.Dependency]))
type CblDB = [CblPkg]

pkgName :: CblPkg -> String
pkgName (n, _) = n

pkgVersion :: CblPkg -> V.Version
pkgVersion (_, (v, _)) = v

pkgDeps :: CblPkg -> [P.Dependency]
pkgDeps (_, (_, ds)) = ds

createCblPkg :: PackageDescription -> CblPkg
createCblPkg pd = (name, (version, deps))
    where
        name = (\ (P.PackageName n) -> n) (P.pkgName $ package pd)
        version = P.pkgVersion $ package pd
        deps = buildDepends pd

getDependencyOn :: String -> CblPkg -> Maybe P.Dependency
getDependencyOn n p = find (\ d -> depName d == n) (pkgDeps p)

emptyPkgDB :: CblDB
emptyPkgDB = []

addPkg :: CblDB -> String -> V.Version -> [P.Dependency] -> CblDB
addPkg db n v ds = nubBy cmp newdb
    where
        cmp (n1, _) (n2, _) = n1 == n2
        newdb = (n, (v, ds)) : db

addPkg2 db (n, (v, ds)) = addPkg db n v ds

addBasePkg db n v = addPkg db n v []

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

instance JSON V.Version where
    showJSON v = makeObj [ ("Version", showJSON $ display v) ]
    readJSON (JSObject o) = let
            jAssoc = fromJSObject o
            resultToMaybe (Ok a) = Just a
            resultToMaybe _ = Nothing
            version = lookup "Version" jAssoc >>= resultToMaybe . readJSON >>= simpleParse
        in
            maybe (fail "Not a version object") return version
    readJSON _ = fail "Not a version object"

instance JSON P.Dependency where
    showJSON d = makeObj [ ("Dependency", showJSON $ display d) ]
    readJSON (JSObject o) = let
            jAssoc = fromJSObject o
            resultToMaybe (Ok a) = Just a
            resultToMaybe _ = Nothing
            dependency = lookup "Dependency" jAssoc >>= resultToMaybe . readJSON >>= simpleParse
        in
            maybe (fail "Not a dependency object") return dependency
    readJSON _ = fail "Not a version object"
