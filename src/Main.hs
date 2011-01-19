{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AddBase
import AddCabal
import PrintUsers

import Control.Monad
import System.Console.CmdArgs
import System.Directory
import System.FilePath

progName = "cblrepo"

data Cmds
    = AddBasePkg {dbLoc :: Maybe String, pkgVers :: [(String, String)]}
    | AddPkg {dbLoc :: Maybe String, cbls :: [FilePath]}
    | PrintUsers {dbLoc :: Maybe String, pkgs :: [String]}
    deriving(Show, Data, Typeable)

cmdAddBasePkg = AddBasePkg
    { dbLoc = Nothing &= help "DB location"
    , pkgVers = def &= args &= typ "STRING,STRING"
    }

cmdAddPkg = AddPkg
    { dbLoc = Nothing &= help "DB location"
    , cbls = def &= args &= typFile
    }

cmdPrintUsers = PrintUsers
    { dbLoc = Nothing &= help "DB location"
    , pkgs = def &= args &= typ "PKG"
    }

cmds = modes
    [ cmdAddBasePkg &= name "addbasepkg"
    , cmdAddPkg &= name "add"
    , cmdPrintUsers &= name "users"
    ]
    &= program progName
    &= summary "CblRepo v0.0"
    &= help "maintain a database of dependencies of CABAL packages"

main = do
    defDbfp <- liftM (</> (progName ++ ".db")) (getAppUserDataDirectory progName)
    cmdArgs cmds >>= \ c -> do
        let dbF = maybe defDbfp id (dbLoc c)
        createDirectoryIfMissing True (dropFileName dbF)
        case c of
            AddBasePkg {} -> addBase dbF (pkgVers c)
            AddPkg {} -> addCabal dbF (cbls c)
            PrintUsers {} -> print c >> printUsers dbF (pkgs c)
