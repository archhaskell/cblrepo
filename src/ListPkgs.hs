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
import Data.List
import Distribution.Text
import Distribution.PackageDescription

listPkgs :: Command ()
listPkgs = do
    lG <- cfgGet $ listGhc . optsCmd
    lD <- cfgGet $ listDistro . optsCmd
    lR <- cfgGet $ noListRepo . optsCmd
    lH <- cfgGet $ hackageFmt . optsCmd
    db <- cfgGet dbFile >>= liftIO . readDb
    let pkgs = filter (pkgFilter lG lD lR) db
    let printer = if lH
            then printCblPkgHackage
            else printCblPkgShort
    liftIO $ mapM_ printer pkgs

pkgFilter g d r p = (g && isGhcPkg p) || (d && isDistroPkg p) || (not r && isRepoPkg p)

printCblPkgShort :: CblPkg -> IO ()
printCblPkgShort p =
    putStrLn $ pkgName p ++ "  " ++ (display $ pkgVersion p) ++ "-" ++ pkgRelease p ++ showFlagsIfPresent p
        where
            showFlagsIfPresent p
                | [] <- pkgFlags p = ""
                | fa <- pkgFlags p = " (" ++ (intercalate " " $ map showSingleFlag fa) ++ ")"
            showSingleFlag (FlagName n, True) = n
            showSingleFlag (FlagName n, False) = '-' : n

printCblPkgHackage :: CblPkg -> IO ()
printCblPkgHackage p =
    print (pkgName p, (display $ pkgVersion p), Nothing :: Maybe String)
