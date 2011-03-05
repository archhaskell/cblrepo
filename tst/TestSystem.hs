{-# OPTIONS_GHC -XTemplateHaskell #-}

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
