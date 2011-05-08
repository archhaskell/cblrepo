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

module AddBase where

import PkgDB
import Util.Misc

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Distribution.Text
import Distribution.Version
import System.FilePath

addBase :: ReaderT Cmds IO ()
addBase = do
    pkgs <- cfgGet pkgVers
    dR <- cfgGet dryRun
    guard $ isJust $ (sequence $ map (simpleParse . snd) pkgs :: Maybe [Version])
    let ps = map (\ (n, v) -> (n, fromJust $ simpleParse v)) pkgs
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    case doAddBase db ps of
        Left brkOthrs -> liftIO $ mapM_ printBrksOth brkOthrs
        Right newDb -> liftIO $ unless dR $ saveDb newDb dbFn

doAddBase db pkgs = let
        (_, fails) = partition (\ (n, v) -> canBeAdded db n v) pkgs
        newDb = foldl (\ d (n, v) -> addBasePkg d n v) db pkgs
        brkOthrs = map (\ (n, v) -> ((n, v), checkDependants db n v)) fails
    in if null fails
        then Right newDb
        else Left brkOthrs

canBeAdded db n v = null $ checkDependants db n v
