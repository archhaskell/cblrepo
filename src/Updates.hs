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

module Updates where

import PkgDB
import Util.Misc

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Distribution.Text
import Distribution.Version
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BS

updates :: ReaderT Cmds IO ()
updates = do
    db <- cfgGet dbFile >>= liftIO . readDb
    aD <- cfgGet appDir
    entries <- liftIO $ liftM (Tar.read . GZip.decompress)
        (BS.readFile $ aD </> "00-index.tar.gz")
    let nonBasePkgs = filter (\ (_, (_, ds, _)) -> not $ null ds) db
    let pkgsNVers = map (\ (p, (v, _, _)) -> (p, v)) nonBasePkgs
    let availPkgs = catMaybes $ eMap extractPkgVer entries
    let outdated = filter
            (\ (p, v) -> maybe False (> v) (latestVer p availPkgs))
            pkgsNVers
    liftIO $ mapM_ (flip printOutdated availPkgs) outdated

type PkgVer = (String, Version)

extractPkgVer :: Entry -> Maybe PkgVer
extractPkgVer e = let
        ep = entryPath e
        isCabal = '/' `elem` ep
        (pkg:ver':_) = map dropTrailingPathSeparator $ splitPath ep
        ver = simpleParse ver'
    in if isCabal && isJust ver
        then Just $ (pkg, (fromJust ver))
        else Nothing

latestVer p pvs = let
        vs = map snd $ filter ((== p) . fst) pvs
    in if null vs
        then Nothing
        else Just $ maximum vs

eMap _ Done = []
eMap f (Next e es) = (f e):(eMap f es)

printOutdated (p, v) avail = let
        l = fromJust $ latestVer p avail
    in
        putStrLn $ p ++ ": " ++ (display v) ++ " (" ++ (display l) ++ ")"
