module Main where

import AddBase
import AddCabal
import BuildPkgs
import BumpPkgs
import IdxUpdate
import ListPkgs
import Updates
import Utils

import Paths_cblrepo

import Control.Monad
import Control.Monad.Trans.Reader
import Distribution.Text
import System.Console.CmdArgs
import System.Directory
import System.FilePath

-- {{{1 command line arguments
cmdAddBasePkg = AddBasePkg
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , pkgVers = def &= args &= typ "STRING,STRING"
    } &= name "addbasepkg"

cmdAddPkg = AddPkg
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , cbls = def &= args &= typ "CABAL"
    } &= name "add"

cmdBumpPkgs = BumpPkgs
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , pkgs = def &= args &= typ "PKG"
    } &= name "bump"

cmdBuildPkgs = BuildPkgs
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , pkgs = def &= args &= typ "PKG"
    } &= name "build"

cmdIdxUpdate = IdxUpdate
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    }

cmdUpdates = Updates
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    } &= name "updates"

cmdListPkgs = ListPkgs
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , incBase = False &= help "include base packages in listing"
    } &= name "list"

cmds = cmdArgsMode $ modes
    [ cmdAddBasePkg
    , cmdAddPkg
    , cmdBumpPkgs
    , cmdBuildPkgs
    , cmdIdxUpdate
    , cmdUpdates
    , cmdListPkgs
    ]
    &= program progName
    &= summary ("CblRepo v" ++ (display version))
    &= help "maintain a database of dependencies of CABAL packages"

-- {{{1 main
main = do
    defAppDir <- getAppUserDataDirectory progName
    cmdArgsRun cmds >>= \ c -> do
        let aD = if null (appDir c) then defAppDir else (appDir c)
        let c' = c { appDir = aD }
        createDirectoryIfMissing True (aD)
        case c' of
            AddBasePkg {} -> runReaderT addBase c'
            AddPkg {} -> runReaderT addCabal c'
            BumpPkgs {} -> runReaderT bumpPkgs c'
            BuildPkgs {} -> runReaderT buildPkgs c'
            IdxUpdate {} -> runReaderT idxUpdate c'
            Updates {} -> runReaderT updates c'
            ListPkgs {} -> runReaderT listPkgs c'
