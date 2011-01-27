module ListPkgs where

import Utils
import PkgDB

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Maybe
import Distribution.Text

listPkgs :: ReaderT Cmds IO ()
listPkgs = do
    iB <- liftM incBase ask
    db <- liftM (fromJust . dbLoc) ask >>= liftIO . readDb
    let pkgs = if not iB
            then filter (not . isBasePkg) db
            else db
    liftIO $ mapM_ printCblPkgShort pkgs

printCblPkgShort (p, (v, _)) =
    putStrLn $ p ++ " ==" ++ (display v)
