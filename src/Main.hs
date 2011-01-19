{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AddBase

import System.Console.CmdArgs
import Control.Monad
import System.Directory
import System.FilePath

progName = "cblrepo"

data Cmds
    = AddBasePkg {pkg :: [(String, String)]}
    | AddPkg {cblLoc :: [FilePath]}
    deriving(Show, Data, Typeable)

cmdAddBasePkg = AddBasePkg { pkg = def &= args &= typ "STRING,STRING" }
cmdAddPkg = AddPkg {cblLoc = def &= args &= typFile}

cmds = modes
    [ cmdAddBasePkg &= name "addbasepkg"
    , cmdAddPkg &= name "add"
    ]
    &= program progName
    &= summary "CblRepo v0.0"
    &= help "maintain a database of dependencies of CABAL packages"

main = do
    appDir <- getAppUserDataDirectory progName
    createDirectoryIfMissing True appDir
    let dbfp = appDir </> (progName ++ ".db")
    cmdArgs cmds >>= \ c -> case c of
        AddBasePkg {} -> addBase dbfp (pkg c)
        AddPkg {} -> print c >> error "add package not implemented"
