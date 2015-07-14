{-
 - Copyright 2015 Per Magnus Therning
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

{-# LANGUAGE TemplateHaskell #-}

module Util.Cfg
    ( Cfg(..)
    , readCfg
    , saveDefCfg
    , getIndexFileName
    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..), SumEncoding(..))
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import System.Posix.Files

data Cfg = Cfg { cfgIdxUrl :: String }
    deriving (Show)

defCfg = Cfg "http://hackage.fpcomplete.com/00-index.tar.gz"

readCfg :: FilePath -> IO Cfg
readCfg fn = do
    exists <- fileExist fn
    if exists
        then (fromMaybe defCfg . decode) `fmap` BSL.readFile fn
        else return defCfg

saveDefCfg :: FilePath -> IO ()
saveDefCfg fn = BSL.writeFile fn (encode defCfg)

getIndexFileName :: Cfg -> String
getIndexFileName cfg = map repSlash $ drop 7 $ cfgIdxUrl cfg
    where
        repSlash c = if c == '/' then '_' else c

$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''Cfg)
