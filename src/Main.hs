{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AddBase

import System.Console.CmdArgs
import Control.Monad
import System.Directory
import System.FilePath

progName = "cblrepo"

data Cmds
    = AddBasePkg {dbLoc :: Maybe String, pkg :: [(String, String)]}
    | AddPkg {dbLoc :: Maybe String, cblLoc :: [FilePath]}
    deriving(Show, Data, Typeable)

cmdAddBasePkg = AddBasePkg
    { dbLoc = Nothing &= help "DB location"
    , pkg = def &= args &= typ "STRING,STRING"
    }

cmdAddPkg = AddPkg
    { dbLoc = Nothing &= help "DB location"
    , cblLoc = def &= args &= typFile
    }

cmds = modes
    [ cmdAddBasePkg &= name "addbasepkg"
    , cmdAddPkg &= name "add"
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
            AddBasePkg {} -> addBase dbF (pkg c)
            AddPkg {} -> print c >> error "add package not implemented"
