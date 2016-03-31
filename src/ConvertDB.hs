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
import System.Directory

convertDb :: Command ()
convertDb = do
    inDbFn <- asks $ inDbFile . optsCmd . fst
    outDbFn <- asks $ outDbFile . optsCmd . fst
    dbExist <- liftIO $ doesFileExist inDbFn
    when dbExist $ do
        newDb <- fmap doConvertDB (liftIO $ ODB.readDb inDbFn)
        liftIO $ NDB.saveDb newDb outDbFn

doConvertDB :: ODB.CblDB -> NDB.CblDB
doConvertDB = map doConvert
    where
        doConvert o
            | ODB.isGhcPkg o = NDB.createGhcPkg n v
            | ODB.isDistroPkg o = NDB.createDistroPkg n v x r
            | ODB.isRepoPkg o = NDB.createRepoPkg n v x d f r
            | otherwise = error ""
            where
                n = ODB.pkgName o
                v = ODB.pkgVersion o
                x = ODB.pkgXRev o
                d = ODB.pkgDeps o
                f = ODB.pkgFlags o
                r = ODB.pkgRelease o
