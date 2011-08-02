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

module BumpPkgs where

import PkgDB
import Util.Misc

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import System.FilePath

bumpPkgs :: ReaderT Cmds IO ()
bumpPkgs = do
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    dR <- cfgGet dryRun
    pkgs <- cfgGet pkgs
    incl <- cfgGet inclusive
    let bpkgs = transDependants db incl pkgs
    let newDb = foldl (\ db p -> bumpRelease db p) db bpkgs
    if dR
        then liftIO $ putStrLn "Would bump:" >> mapM_ putStrLn bpkgs
        else liftIO $ saveDb newDb dbFn

transDependants db i pkgs = filter ((||) i . (not . flip elem pkgs)) $ transitiveDependants db pkgs
