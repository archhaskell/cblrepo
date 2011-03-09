module PkgDB where

import qualified Distribution.Package as P
import qualified Distribution.Version as V

type CblPkg = (String, (V.Version, [P.Dependency]))
type CblDB = [CblPkg]

lookupPkg :: CblDB -> String -> Maybe CblPkg
