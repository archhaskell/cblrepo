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
import Distribution.Version hiding (thisVersion)

upgrades :: Command ()
upgrades = do
  db <- asks (dbFile . fst) >>= liftIO . readDb
  aD <- asks $ appDir . fst
  aCS <- asks $ idxStyle .optsCmd . fst
  xrevsOnly <- asks $ xrevs . optsCmd . fst
  cfg <- asks snd
  availPkgsNVers <- liftIO $ buildPkgVersions <$> readIndexFile aD (getIndexFileName cfg)
  let nonBasePkgs = filter (not . isBasePkg) db
      pkgsNVers = map (pkgName &&& pkgVersion &&& pkgXRev) nonBasePkgs
      filterFunc = if xrevsOnly
                   then (\ (p, (v, x)) -> maybe False (> x) (snd <$> thisVersion availPkgsNVers p v))
                   else (\ (p, vx) -> maybe False (> vx) (latestVersion availPkgsNVers p undefined))
      outdated = filter filterFunc pkgsNVers
      printer = if aCS
                then printNewShort
                else printUpgrades
  if xrevsOnly
    then liftIO $ mapM_ (printer thisVersion availPkgsNVers) outdated
    else liftIO $ mapM_ (printer latestVersion availPkgsNVers) outdated

printUpgrades verFunc avail (pkgName, (pkgVer, pkgXRev)) = putStrLn $
  pkgName ++ ": " ++ display pkgVer ++ ":x" ++ show pkgXRev ++ " (" ++ display lv ++ ":x" ++ show lx ++ ")"
  where
    (lv, lx) = fromJust $ verFunc avail pkgName pkgVer

printNewShort verFunc avail (pkgName, (pkgVer, _)) = putStrLn $
  pkgName ++ "," ++ display l
  where
    l = fst $ fromJust $ verFunc avail pkgName pkgVer
