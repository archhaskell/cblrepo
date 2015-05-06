{-# LANGUAGE PatternGuards #-}
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

module ListPkgs where

import Util.Misc
import PkgDB

import Control.Monad.Reader
import Distribution.Text
import Distribution.PackageDescription

listPkgs :: Command ()
listPkgs = do
    lG <- asks $ listGhc . optsCmd
    lD <- asks $ listDistro . optsCmd
    lR <- asks $ noListRepo . optsCmd
    lH <- asks $ hackageFmt . optsCmd
    ps <- asks $ pkgs . optsCmd
    db <- asks dbFile >>= liftIO . readDb
    let allPkgs = filter (pkgFilter lG lD lR) db
    let pkgsToList = if null ps
            then allPkgs
            else filter (\p -> pkgName p `elem` ps) allPkgs
    let printer = if lH
            then printCblPkgHackage
            else printCblPkgShort
    liftIO $ mapM_ printer pkgsToList

pkgFilter :: Bool -> Bool -> Bool -> CblPkg -> Bool
pkgFilter g d r p = (g && isGhcPkg p) || (d && isDistroPkg p) || (not r && isRepoPkg p)

printCblPkgShort :: CblPkg -> IO ()
printCblPkgShort p =
    putStrLn $ pkgName p ++ "  " ++ v ++ "-" ++ r ++ showFlagsIfPresent p
        where
            v = display (pkgVersion p) ++ if (not $ isGhcPkg p) then ("_" ++ show (pkgXRev p)) else ""
            r = if isGhcPkg p then "xx" else show (pkgRelease p)
            showFlagsIfPresent _p
                | [] <- pkgFlags _p = ""
                | fa <- pkgFlags _p = " (" ++ unwords (map showSingleFlag fa) ++ ")"
            showSingleFlag (FlagName n, True) = n
            showSingleFlag (FlagName n, False) = '-' : n

printCblPkgHackage :: CblPkg -> IO ()
printCblPkgHackage p =
    print (pkgName p, display $ pkgVersion p, Nothing :: Maybe String)
