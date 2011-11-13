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

module Remove where

-- {{{1 imports
-- {{{1 local
import PkgDB
import Util.Misc

-- {{{1 system
import Control.Monad.Error
import Control.Monad.Reader
import System.Exit

-- {{{1 remove
remove :: ReaderT Cmds IO ()
remove = do
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    pkgs <- cfgGet pkgs
    dR <- cfgGet dryRun
    liftIO $ either
        (\ s -> putStrLn s >> exitFailure)
        (\ newDb -> unless dR $ saveDb newDb dbFn)
        (foldM removeOne db pkgs)

removeOne :: CblDB -> String -> Either String CblDB
removeOne db pkg = let
        deps = lookupDependants db pkg
    in if null deps
        then return (delPkg db pkg)
        else throwError ("Can't delete package " ++ pkg ++ " (" ++ (show $ length deps) ++ " dependants)")
