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
doConvert (ODB.CP n (ODB.GhcPkg v)) = NDB.CP n (NDB.GhcPkg v)
doConvert (ODB.CP n (ODB.DistroPkg v r)) = NDB.CP n (NDB.DistroPkg v r)
doConvert (ODB.CP n (ODB.RepoPkg v d f r)) = NDB.CP n (NDB.RepoPkg v 0 d f r)
