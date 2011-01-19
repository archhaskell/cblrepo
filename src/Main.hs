{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

data Cmds
    = AddBasePkg {pkgName :: String, pkgVer :: String}
    | AddPkg {cblLoc :: [FilePath]}
    deriving(Show, Data, Typeable)

cmdAddBasePkg = AddBasePkg
    { pkgName = def &= argPos 0 &= typ "NAME"
    , pkgVer = def &= argPos 1 &= typ "VERSION"
    }
cmdAddPkg = AddPkg {cblLoc = def &= args &= typFile}

cmds = modes
    [ cmdAddBasePkg &= name "addbasepkg"
    , cmdAddPkg &= name "add"
    ]
    &= program "cblrepo"
    &= summary "CblRepo v0.0"
    &= help "maintain a database of dependencies of CABAL packages"

main = cmdArgs cmds >>= \ c -> case c of
    AddBasePkg {} -> error "add base package not implemented"
    AddPkg {} -> error "add package not implemented"
