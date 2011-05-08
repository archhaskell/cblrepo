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

module TestSystem
    ( testGroup
    ) where

import Data.List
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

-- {{{1 nub works as expected
case_nub_order = do nub [1,2,1,3,1] @=? [1,2,3]

-- {{{1 testGroup
testGroup = $(testGroupGenerator)
