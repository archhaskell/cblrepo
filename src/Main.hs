module Main where

import AddBase
import AddCabal
import BumpPkgs
import BuildPkgs
import IdxUpdate
import Updates
import Utils
import ListPkgs

import Control.Monad
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Control.Monad.Trans.Reader

cmdAddBasePkg = AddBasePkg
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , pkgVers = def &= args &= typ "STRING,STRING"
    } &= name "addbasepkg"

cmdAddPkg = AddPkg
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , cbls = def &= args &= typFile
    } &= name "add"

cmdBumpPkgs = BumpPkgs
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , pkgs = def &= args &= typ "PKG"
    } &= name "bump"

cmdBuildPkgs = BuildPkgs
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
    , pkgs = def &= args &= typ "PKG"
    } &= name "build"

cmdIdxUpdate = IdxUpdate
    { dbLoc = Nothing &= ignore
    }

cmdUpdates = Updates
    { dbLoc = Nothing &= ignore
    } &= name "updates"

cmdListPkgs = ListPkgs
    { dbLoc = Nothing &= explicit &= name "db" &= help "DB location" &= typFile
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
    &= summary "CblRepo v0.0"
    &= help "maintain a database of dependencies of CABAL packages"

main = do
    defDbfp <- liftM (</> (progName ++ ".db")) (getAppUserDataDirectory progName)
    cmdArgsRun cmds >>= \ c -> do
        let dbF = maybe defDbfp id (dbLoc c)
        let c' = c {dbLoc = Just dbF}
        createDirectoryIfMissing True (dropFileName dbF)
        case c' of
            AddBasePkg {} -> addBase dbF (pkgVers c)
            AddPkg {} -> addCabal dbF (cbls c)
            BumpPkgs {} -> bumpPkgs dbF (pkgs c)
            BuildPkgs {} -> buildPkgs dbF (pkgs c)
            IdxUpdate {} -> getAppUserDataDirectory progName >>= idxUpdate
            Updates {} -> getAppUserDataDirectory progName >>= updates
            ListPkgs {} -> runReaderT listPkgs c'
