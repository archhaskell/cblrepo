module AddBase where

import PkgDB
import Utils

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Text
import Distribution.Version

addBase :: FilePath -> [(String, String)] -> IO ()
addBase dbFp pkgs = do
    guard $ isJust $ (sequence $ map (simpleParse . snd) pkgs :: Maybe [Version])
    let ps = map (\ (n, v) -> (n, fromJust $ simpleParse v)) pkgs
    db <- readDb dbFp
    case doAddBase db ps of
        Left brkOthrs -> do
            mapM_ printBrksOth brkOthrs
        Right newDb -> do
            putStrLn "Success"
            saveDb newDb dbFp

doAddBase db pkgs = let
        (_, fails) = partition (\ (n, v) -> canBeAdded db n v) pkgs
        newDb = foldl (\ d (n, v) -> addBasePkg d n v) db pkgs
        brkOthrs = map (\ (n, v) -> ((n, v), checkDependants db n v)) fails
    in if null fails
        then Right newDb
        else Left brkOthrs

canBeAdded db n v = null $ checkDependants db n v
