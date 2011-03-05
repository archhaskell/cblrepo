{-# OPTIONS_GHC -XTemplateHaskell #-}

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
    [ ("pkgA", (fromJust $ simpleParse "1.0", [])) -- nothing depends on this pkg
    -- a chain of two
    , ("pkgB", (fromJust $ simpleParse "1.0", []))
    , ("pkgC", (fromJust $ simpleParse "1.0", map (fromJust . simpleParse) ["pkgB"]))
    -- a chain of three
    , ("pkgD", (fromJust $ simpleParse "1.0", []))
    , ("pkgE", (fromJust $ simpleParse "1.0", map (fromJust . simpleParse) ["pkgD"]))
    , ("pkgF", (fromJust $ simpleParse "1.0", map (fromJust . simpleParse) ["pkgE"]))
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
