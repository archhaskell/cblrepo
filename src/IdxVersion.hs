module IdxVersion where

import Utils

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import Distribution.Text
import Distribution.Version
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BS

idxVersion :: ReaderT Cmds IO ()
idxVersion = do
    aD <- cfgGet appDir
    pkgs <- cfgGet pkgs
    liftIO $ do
        es <- liftM (Tar.read . GZip.decompress) (BS.readFile $ aD </> "00-index.tar.gz")
        mapM_ (printVersions . findVersions es) pkgs

findVersions es p = (p, findV p es [])
    where
        findV pkgName (Next e es) acc = let
                eP = entryPath e
                (ePkg:v:_) = splitDirectories eP
            in if ('/' `elem` eP) && (pkgName == ePkg)
                then findV pkgName es (v:acc)
                else findV pkgName es acc

        findV _ Done acc = let
                vs :: [Version]
                vs = map (fromJust . simpleParse) acc
            in map display $ sort vs

printVersions (p, vs) = do
    putStr $ p ++ " : "
    putStrLn $ unwords vs
