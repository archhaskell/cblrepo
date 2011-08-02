{-# OPTIONS_GHC -XTemplateHaskell #-}
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

module TestPkgDB
    ( testGroup
    ) where

import PkgDB

import Data.Maybe
import Distribution.Text
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import Text.JSON
import qualified Distribution.Package as P
import qualified Distribution.Version as V

-- {{{1 transitiveDependants
theDb :: CblDB
theDb =
    [ createRepoPkg "pkgA" (fromJust $ simpleParse "1.0") [] "1" -- nothing depends on this pkg
    -- a chain of two
    , createRepoPkg "pkgB" (fromJust $ simpleParse "1.0") [] "1"
    , createRepoPkg "pkgC" (fromJust $ simpleParse "1.0")
        (map (fromJust . simpleParse) ["pkgB"]) "1"
    -- a chain of three
    , createRepoPkg "pkgD" (fromJust $ simpleParse "1.0") [] "1"
    , createRepoPkg "pkgE" (fromJust $ simpleParse "1.0")
        (map (fromJust . simpleParse) ["pkgD"]) "1"
    , createRepoPkg "pkgF" (fromJust $ simpleParse "1.0")
        (map (fromJust . simpleParse) ["pkgE"]) "1"
    ]

case_TD_one = do transitiveDependants theDb ["pkgA"] @=? ["pkgA"]
case_TD_two = do transitiveDependants theDb ["pkgB"] @=? ["pkgB", "pkgC"]
case_TD_three = do transitiveDependants theDb ["pkgD"] @=? ["pkgD", "pkgE", "pkgF"]
-- pass in packages in correct order
case_TD_ordering1 = do transitiveDependants theDb ["pkgD", "pkgF"] @=? ["pkgD", "pkgE", "pkgF"]
-- pass in packages in reverse order
case_TD_ordering2 = do transitiveDependants theDb ["pkgF", "pkgD"] @=? ["pkgD", "pkgE", "pkgF"]

-- {{{1 JSON instances
case_json_version = let
        v1_i = V.Version [1, 2, 3] []
        v1_s = "{\"Version\":\"1.2.3\"}"
        v2_i = V.Version [1, 2, 3] ["foo"]
    in do
        assertEqual "JSON Version showJSON 1" v1_s (encode $ showJSON v1_i)
        assertEqual "JSON Version showJSON 2" v1_s (encode $ showJSON v2_i)
        assertEqual "JSON Version readJSON 1" (Ok v1_i) (decode v1_s)

case_json_dependency = let
        (Just d1_i) = simpleParse "package -any" :: Maybe P.Dependency
        d1_s = "{\"Dependency\":\"package -any\"}"
        (Just d2_i) = simpleParse "ConfigFile >=1 && <1.1" :: Maybe P.Dependency
        d2_s = "{\"Dependency\":\"ConfigFile >=1 && <1.1\"}"
    in do
        assertEqual "JSON Dependency showJSON 1" d1_s (encode $ showJSON d1_i)
        assertEqual "JSON Dependency showJSON 2" d2_s (encode $ showJSON d2_i)
        assertEqual "JSON Dependency readJSON 1" (Ok d1_i) (decode d1_s)
        assertEqual "JSON Dependency readJSON 2" (Ok d2_i) (decode d2_s)

-- {{{1 testGroup
testGroup = $(testGroupGenerator)
