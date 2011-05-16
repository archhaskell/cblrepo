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

-- {{{1 transitiveDependants
theDb :: CblDB
theDb =
    [ ("pkgA", (fromJust $ simpleParse "1.0", [], 1)) -- nothing depends on this pkg
    -- a chain of two
    , ("pkgB", (fromJust $ simpleParse "1.0", [], 1))
    , ("pkgC", (fromJust $ simpleParse "1.0", map (fromJust . simpleParse) ["pkgB"], 1))
    -- a chain of three
    , ("pkgD", (fromJust $ simpleParse "1.0", [], 1))
    , ("pkgE", (fromJust $ simpleParse "1.0", map (fromJust . simpleParse) ["pkgD"], 1))
    , ("pkgF", (fromJust $ simpleParse "1.0", map (fromJust . simpleParse) ["pkgE"], 1))
    ]

case_TD_one = do transitiveDependants theDb ["pkgA"] @=? ["pkgA"]
case_TD_two = do transitiveDependants theDb ["pkgB"] @=? ["pkgB", "pkgC"]
case_TD_three = do transitiveDependants theDb ["pkgD"] @=? ["pkgD", "pkgE", "pkgF"]
-- pass in packages in correct order
case_TD_ordering1 = do transitiveDependants theDb ["pkgD", "pkgF"] @=? ["pkgD", "pkgE", "pkgF"]
-- pass in packages in reverse order
case_TD_ordering2 = do transitiveDependants theDb ["pkgF", "pkgD"] @=? ["pkgD", "pkgE", "pkgF"]

-- {{{1 testGroup
testGroup = $(testGroupGenerator)
