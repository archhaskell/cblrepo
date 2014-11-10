{-
 - Copyright 2014 Per Magnus Therning
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

module Util.HackageIndex
    where

import Util.Misc

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Map as M
import Data.Maybe
import Data.Version
import Distribution.Text
import System.FilePath

readIndexFile :: FilePath -> IO BSL.ByteString
readIndexFile indexLocation = exitOnException
    "Cannot open index file, have you run the 'sync' command?"
    (BSL.readFile $ indexLocation </> indexFileName)

type PkgVersions = M.Map String [Version]

buildPkgVersions :: BSL.ByteString -- ^ the index
    -> PkgVersions
buildPkgVersions idx = createPkgVerMap M.empty entries
    where
        entries = Tar.read $ GZip.decompress idx

        createPkgVerMap acc (Tar.Next e es) = createPkgVerMap (insertWith (++) (parts !! 0) [ver] acc) es
            where
                parts = splitDirectories (Tar.entryPath e)
                ver = fromJust . simpleParse $ parts !! 1

        createPkgVerMap acc Tar.Done = acc
        createPkgVerMap _ (Tar.Fail _) = undefined

latestVersion :: PkgVersions
    -> String -- ^ package name
    -> Maybe Version
latestVersion pnv pkg = last . sort <$> M.lookup  pkg pnv

extractCabal :: BSL.ByteString  -- ^ the index
    -> String                   -- ^ package name
    -> Version                  -- ^ package version
    -> Maybe BSL.ByteString
extractCabal idx pkg ver = getContent entries
    where
        entries = Tar.read $ GZip.decompress idx
        pkgPath = pkg </> display ver </> pkg <.> "cabal"

        getContent (Tar.Next e es)
            | pkgPath == Tar.entryPath e =
                case Tar.entryContent e of
                    Tar.NormalFile c _ -> Just c
                    _ -> Nothing
            | otherwise = getContent es

        getContent _ = Nothing
