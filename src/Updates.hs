module Updates where

import PkgDB

import Codec.Archive.Tar as Tar
import Codec.Compression.GZip as GZip
import Control.Monad
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import Distribution.Text
import Distribution.Version

updates dn = do
    entries <- liftM (Tar.read . GZip.decompress)
        (BS.readFile $ dn </> "00-index.tar.gz")
    db <- readDb $ dn </> "cblrepo.db"
    let nonBasePkgs = filter (\ (_, (_, ds)) -> not $ null ds) db
    let pkgsNVers = map (\ (p, (v, _)) -> (p, v)) nonBasePkgs
    let availPkgs = catMaybes $ eMap extractPkgVer entries
    let outdated = filter
            (\ (p, v) -> maybe False (> v) (latestVer p availPkgs))
            pkgsNVers
    mapM_ (flip printOutdated availPkgs) outdated

type PkgVer = (String, Version)

extractPkgVer :: Entry -> Maybe PkgVer
extractPkgVer e = let
        ep = entryPath e
        isCabal = '/' `elem` ep
        (pkg:ver':_) = map dropTrailingPathSeparator $ splitPath ep
        ver = simpleParse ver'
    in if isCabal && isJust ver
        then Just $ (pkg, (fromJust ver))
        else Nothing

latestVer p pvs = let
        vs = map snd $ filter ((== p) . fst) pvs
    in if null vs
        then Nothing
        else Just $ maximum vs

eMap _ Done = []
eMap f (Next e es) = (f e):(eMap f es)

printOutdated (p, v) avail = let
        l = fromJust $ latestVer p avail
    in
        putStrLn $ p ++ ": " ++ (display v) ++ " (" ++ (display l) ++ ")"
