module Main where

import AddBase
import AddCabal
import BuildPkgs
import BumpPkgs
import IdxSync
import IdxVersion
import ListPkgs
import Updates
import Utils
import Urls

import Paths_cblrepo

import Control.Monad
import Control.Monad.Reader
import Distribution.Text
import System.Console.CmdArgs
import System.Directory
import System.FilePath

-- {{{1 command line arguments
cmdAddBasePkg = AddBasePkg
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , dbFile = "cblrepo.db" &= explicit &= name "db" &= help "package database" &= typFile
    , dryRun = False &= explicit &= name "n" &= help "dry run"
    , pkgVers = def &= args &= typ "STRING,STRING"
    } &= name "addbasepkg" &= help "add base packages" &= details
        [ "The format for a package is <name>,<version>." ]

cmdAddPkg = AddPkg
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , dbFile = "cblrepo.db" &= explicit &= name "db" &= help "package database" &= typFile
    , dryRun = False &= explicit &= name "n" &= help "dry run"
    , cbls = def &= args &= typ "CABAL"
    } &= name "add" &= help "add a package from a Cabal file" &= details
        [ "There are three ways to specify the location of the Cabal file:"
        , " 1. The filename of the Cabal file."
        , " 2. A URL where the Cabal can be found (for file:// URLs the full absolute path is required)."
        , " 3. A pair, <name>,<version>, will load the Cabal file out of an index file (see idxupdate)"
        , "All three format may be mixed on the command line."
        ]

cmdBumpPkgs = BumpPkgs
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , dbFile = "cblrepo.db" &= explicit &= name "db" &= help "package database" &= typFile
    , pkgs = def &= args &= typ "PKG"
    } &= name "bump" &= help "list packages that need bumping"

cmdBuildPkgs = BuildPkgs
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , dbFile = "cblrepo.db" &= explicit &= name "db" &= help "package database" &= typFile
    , pkgs = def &= args &= typ "PKG"
    } &= name "build" &= help "list packages that need rebuilding, in order"

cmdIdxSync = IdxSync
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    } &= help "update the index"

cmdIdxVersion = IdxVersion
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , pkgs = def &= args &= typ "PKG"
    } &= help "list available versions"

cmdUpdates = Updates
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , dbFile = "cblrepo.db" &= explicit &= name "db" &= help "package database" &= typFile
    } &= name "updates" &= help "check for availabale updates"

cmdListPkgs = ListPkgs
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , dbFile = "cblrepo.db" &= explicit &= name "db" &= help "package database" &= typFile
    , incBase = False &= help "include base packages in listing"
    } &= name "list" &= help "list packages in repo"

cmdUrls = Urls
    { appDir = def &= explicit &= name "appdir" &= help "application data directory" &= typDir
    , pkgVers = def &= args &= typ "STRING,STRING"
    } &= help "list urls of cabal files for the given packages" &= details
        [ "The format for a package is <name>,<version>." ]

cmds = cmdArgsMode $ modes
    [ cmdAddBasePkg
    , cmdAddPkg
    , cmdBuildPkgs
    , cmdBumpPkgs
    , cmdIdxSync
    , cmdIdxVersion
    , cmdListPkgs
    , cmdUpdates
    , cmdUrls
    ]
    &= program progName
    &= summary (progName ++ " v" ++ (display version))
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
            BuildPkgs {} -> runReaderT buildPkgs c'
            BumpPkgs {} -> runReaderT bumpPkgs c'
            IdxSync {} -> runReaderT idxSync c'
            IdxVersion {} -> runReaderT idxVersion c'
            ListPkgs {} -> runReaderT listPkgs c'
            Updates {} -> runReaderT updates c'
            Urls {} -> runReaderT urls c'
