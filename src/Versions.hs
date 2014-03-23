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

import Util.Misc

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Distribution.Text
import Distribution.Version
import System.FilePath

versions :: Command ()
versions = do
    aD <- optGet appDir
    l <- optGet $ latest . optsCmd
    pkgs <- optGet $ pkgs . optsCmd
    let printFunc = if l then printLatestVersion else printAllVersions
    liftIO $ do
        es <- liftM (Tar.read . GZip.decompress) (readIndexFile aD)
        mapM_ (printFunc . findVersions es) pkgs

findVersions es p = (p, findV p es [])
    where
        findV pkgName (Next e es) acc = let
                eP = entryPath e
                (ePkg:v:_) = splitDirectories eP
            in findV pkgName es
                (if ('/' `elem` eP) && (pkgName == ePkg) then v:acc else acc)

        findV _ Done acc = let
                vs :: [Version]
                vs = map (fromJust . simpleParse) acc
            in map display $ sort vs

        findV _ (Fail _) _ = error "Failure to read index file"

printAllVersions (p, vs) = do
    putStr $ p ++ " : "
    putStrLn $ unwords vs

printLatestVersion (p, vs) = do
    putStr $ p ++ ","
    putStrLn $ last vs
