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

module ConvertDB where

import Util.Misc
import qualified OldPkgDB as ODB
import qualified PkgDB as NDB

import Control.Monad.Reader

convertDb :: Command ()
convertDb = do
    inDbFn <- optGet $ inDbFile . optsCmd
    outDbFn <- optGet $ outDbFile . optsCmd
    newDb <- liftIO $ liftM (map doConvert) (ODB.readDb inDbFn)
    liftIO $ NDB.saveDb newDb outDbFn

doConvert :: ODB.CblPkg -> NDB.CblPkg
doConvert o
    | ODB.isGhcPkg o = NDB.createGhcPkg n v
    | ODB.isDistroPkg o = NDB.createDistroPkg n v r
    | ODB.isRepoPkg o = NDB.createRepoPkg n v x d f r
    | otherwise = error ""
    where
        n = ODB.pkgName o
        v = ODB.pkgVersion o
        x = 0
        d = ODB.pkgDeps o
        f = ODB.pkgFlags o
        r = ODB.pkgRelease o
