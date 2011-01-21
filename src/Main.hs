{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AddBase
import AddCabal
import BumpPkgs
import BuildPkgs

import Control.Monad
import System.Console.CmdArgs
import System.Directory
import System.FilePath

progName = "cblrepo"

data Cmds
    = AddBasePkg {dbLoc :: Maybe String, pkgVers :: [(String, String)]}
    | AddPkg {dbLoc :: Maybe String, cbls :: [FilePath]}
    | BumpPkgs {dbLoc :: Maybe String, pkgs :: [String]}
    | BuildPkgs {dbLoc :: Maybe String, pkgs :: [String]}
    deriving(Show, Data, Typeable)

cmdAddBasePkg = AddBasePkg
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , pkgVers = def &= args &= typ "STRING,STRING"
    }

cmdAddPkg = AddPkg
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , cbls = def &= args &= typFile
    }

cmdBumpPkgs = BumpPkgs
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , pkgs = def &= args &= typ "PKG"
    }

cmdBuildPkgs = BuildPkgs
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , pkgs = def &= args &= typ "PKG"
    }

cmds = cmdArgsMode $ modes
    [ cmdAddBasePkg &= name "addbasepkg"
    , cmdAddPkg &= name "add"
    , cmdBumpPkgs &= name "bump"
    , cmdBuildPkgs &= name "build"
    ]
    &= program progName
    &= summary "CblRepo v0.0"
    &= help "maintain a database of dependencies of CABAL packages"

main = do
    defDbfp <- liftM (</> (progName ++ ".db")) (getAppUserDataDirectory progName)
    cmdArgsRun cmds >>= \ c -> do
        let dbF = maybe defDbfp id (dbLoc c)
        createDirectoryIfMissing True (dropFileName dbF)
        case c of
            AddBasePkg {} -> addBase dbF (pkgVers c)
            AddPkg {} -> addCabal dbF (cbls c)
            BumpPkgs {} -> bumpPkgs dbF (pkgs c)
            BuildPkgs {} -> buildPkgs dbF (pkgs c)
