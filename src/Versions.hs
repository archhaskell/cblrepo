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
import qualified Data.ByteString.Lazy.Char8 as BS

versions :: Command ()
versions = do
    aD <- cfgGet appDir
    pkgs <- cfgGet $ pkgs . optsCmd
    liftIO $ do
        es <- liftM (Tar.read . GZip.decompress) (BS.readFile $ aD </> "00-index.tar.gz")
        mapM_ (printVersions . findVersions es) pkgs

findVersions es p = (p, findV p es [])
    where
        findV pkgName (Next e es) acc = let
                eP = entryPath e
                (ePkg:v:_) = splitDirectories eP
            in if ('/' `elem` eP) && (pkgName == ePkg)
                then findV pkgName es (v:acc)
                else findV pkgName es acc

        findV _ Done acc = let
                vs :: [Version]
                vs = map (fromJust . simpleParse) acc
            in map display $ sort vs

printVersions (p, vs) = do
    putStr $ p ++ " : "
    putStrLn $ unwords vs
