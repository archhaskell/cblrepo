module AddBase where

import PkgDB
import Util.Misc

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Distribution.Text
import Distribution.Version
import System.FilePath

addBase :: ReaderT Cmds IO ()
addBase = do
    pkgs <- cfgGet pkgVers
    dR <- cfgGet dryRun
    guard $ isJust $ (sequence $ map (simpleParse . snd) pkgs :: Maybe [Version])
    let ps = map (\ (n, v) -> (n, fromJust $ simpleParse v)) pkgs
    dbFn <- cfgGet dbFile
    db <- liftIO $ readDb dbFn
    case doAddBase db ps of
        Left brkOthrs -> liftIO $ mapM_ printBrksOth brkOthrs
        Right newDb -> liftIO $ unless dR $ saveDb newDb dbFn

doAddBase db pkgs = let
        (_, fails) = partition (\ (n, v) -> canBeAdded db n v) pkgs
        newDb = foldl (\ d (n, v) -> addBasePkg d n v) db pkgs
        brkOthrs = map (\ (n, v) -> ((n, v), checkDependants db n v)) fails
    in if null fails
        then Right newDb
        else Left brkOthrs

canBeAdded db n v = null $ checkDependants db n v
