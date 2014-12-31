{-
 - Copyright 2011-2014 Per Magnus Therning
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

module Updates
    ( updates
    ) where

import PkgDB
import Util.Misc
import Util.HackageIndex

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Data.Maybe
import Distribution.Text
import Distribution.Version

updates :: Command ()
updates = do
    db <- optGet dbFile >>= liftIO . readDb
    aD <- optGet appDir
    aCS <- optGet $ idxStyle .optsCmd
    availPkgsNVers <- liftIO $ buildPkgVersions <$> readIndexFile aD
    let nonBasePkgs = filter (not . isBasePkg) db
        pkgsNVers = map (pkgName &&& pkgVersion &&& pkgXRev) nonBasePkgs
        outdated = filter
            (\ (p, vx) -> maybe False (> vx) (latestVersion availPkgsNVers p))
            pkgsNVers
        printer = if aCS
            then printOutdatedShort
            else printOutdated
    liftIO $ mapM_ (printer availPkgsNVers) outdated

printOutdated :: PkgVersions -> (String, (Version, Int)) -> IO ()
printOutdated avail (p, (v, x)) = putStrLn $ p ++ ": " ++ display v ++ ":x" ++ show x ++ " (" ++ display lv ++ ":x" ++ show lx ++ ")"
    where
        (lv, lx) = fromJust $ latestVersion avail p

printOutdatedShort :: PkgVersions -> (String, (Version, Int)) -> IO ()
printOutdatedShort avail (p, _) = putStrLn $ p ++ "," ++ display l
    where
        l = fst $ fromJust $ latestVersion avail p
