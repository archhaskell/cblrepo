module Main where

import qualified TestPkgDB
import qualified TestSystem

import Test.Framework

main = defaultMain
    [ TestPkgDB.testGroup
    , TestSystem.testGroup
    ]

