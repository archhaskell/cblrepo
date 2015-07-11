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

module Upgrades
    ( upgrades
    ) where

import PkgDB
import Util.Misc
import Util.HackageIndex
import Util.Cfg

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Data.Maybe
import Distribution.Text
import Distribution.Version

upgrades :: Command ()
upgrades = do
    db <- asks (dbFile . fst) >>= liftIO . readDb
    aD <- asks $ appDir . fst
    aCS <- asks $ idxStyle .optsCmd . fst
    cfg <- asks snd
    availPkgsNVers <- liftIO $ buildPkgVersions <$> readIndexFile aD (getIndexFileName cfg)
    let nonBasePkgs = filter (not . isBasePkg) db
        pkgsNVers = map (pkgName &&& pkgVersion &&& pkgXRev) nonBasePkgs
        outdated = filter
            (\ (p, vx) -> maybe False (> vx) (latestVersion availPkgsNVers p))
            pkgsNVers
        printer = if aCS
            then printOldShort
            else printOld
    liftIO $ mapM_ (printer availPkgsNVers) outdated

printOld :: PkgVersions -> (String, (Version, Int)) -> IO ()
printOld avail (p, (v, x)) = putStrLn $ p ++ ": " ++ display v ++ ":x" ++ show x ++ " (" ++ display lv ++ ":x" ++ show lx ++ ")"
    where
        (lv, lx) = fromJust $ latestVersion avail p

printOldShort :: PkgVersions -> (String, (Version, Int)) -> IO ()
printOldShort avail (p, _) = putStrLn $ p ++ "," ++ display l
    where
        l = fst $ fromJust $ latestVersion avail p
