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

module Versions where

import Util.HackageIndex
import Util.Misc

import Control.Applicative
import Control.Monad.Reader
import Data.Map as M
import Distribution.Text
import Distribution.Version

versions :: Command ()
versions = do
    aD <- asks (appDir . fst)
    l <- asks $ latest . optsCmd . fst
    pkgs <- asks $ pkgs . optsCmd . fst
    let printFunc = if l then printLatestVersion else printAllVersions
    liftIO $ do
        pkgsNVers <- buildPkgVersions <$> readIndexFile aD
        mapM_ (\ pkg -> printFunc (pkg, M.lookup pkg pkgsNVers)) pkgs

printAllVersions :: (String, Maybe [(Version, Int)]) -> IO ()
printAllVersions (p, Nothing) = putStrLn $ p ++ ": No such package"
printAllVersions (p, Just vs) = putStrLn $ p ++ ": " ++ versions
    where
        versions = unwords $ display . fst <$> vs

printLatestVersion :: (String, Maybe [(Version, Int)]) -> IO ()
printLatestVersion (p, Nothing) = putStrLn $ p ++ ": No such package"
printLatestVersion (p, Just vs) = putStrLn $ p ++ "," ++ display (fst $ head vs)
