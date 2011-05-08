{-
 - Copyright 2011 Per Magnus Therning
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module Main where

import AddBase
import AddCabal
import BuildPkgs
import BumpPkgs
import IdxSync
import IdxVersion
import ListPkgs
import Updates
import Util.Misc
import Urls
import PkgBuild

import Paths_cblrepo

import Control.Monad
import Control.Monad.Reader
import Distribution.Text
import System.Console.CmdArgs
import System.Directory
import System.FilePath

-- {{{1 command line arguments
argAppDir = appDir := def += explicit += name "appdir" += help "application data directory" += typDir
argDbFile = dbFile := "cblrepo.db" += explicit += name "db" += help "package database" += typFile
argDryRun = dryRun := False += explicit += name "n" += help "dry run"

cmdAddBasePkg = record defAddBasePkg
    [ argAppDir, argDbFile, argDryRun
    , pkgVers := def += args += typ "STRING,STRING"
    ] += name "addbasepkg" += help "add base packages" += details
        [ "The format for a package is <name>,<version>." ]

cmdAddPkg = record defAddPkg
    [ argAppDir, argDbFile
    , patchDir := "patches" += explicit += name "patchdir" += help "location of patches" += typDir
    , argDryRun
    , cbls := def += args += typ "CABAL"
    ] += name "add" += help "add a package from a Cabal file" += details
        [ "There are three ways to specify the location of the Cabal file:"
        , " 1. The filename of the Cabal file."
        , " 2. A URL where the Cabal can be found (for file:// URLs the full absolute path is required)."
        , " 3. A pair, <name>,<version>, will load the Cabal file out of an index file (see idxupdate)"
        , "All three format may be mixed on the command line."
        ]

cmdBumpPkgs = record defBumpPkgs
    [ argAppDir, argDbFile
    , argDryRun
    , inclusive := False += explicit += name "inclusive" += help "include listed packages"
    , pkgs := def += args += typ "PKG"
    ] += name "bump" += help "bump packages that need it after updating the named packages"

cmdBuildPkgs = record defBuildPkgs
    [ argAppDir, argDbFile
    , pkgs := def += args += typ "PKG"
    ] += name "build" += help "list packages that need rebuilding, in order"

cmdIdxSync = record defIdxSync [ argAppDir ] += help "update the index"

cmdIdxVersion = record defIdxVersion
    [ argAppDir
    , pkgs := def += args += typ "PKG"
    ] += help "list available versions"

cmdUpdates = record defUpdates [ argAppDir , argDbFile ] += name "updates" += help "check for availabale updates"

cmdListPkgs = record defListPkgs
    [ argAppDir, argDbFile
    , incBase := False += help "include base packages in listing"
    ] += name "list" += help "list packages in repo"

cmdUrls = record defUrls
    [ argAppDir
    , pkgVers := def += args += typ "STRING,STRING"
    ] += help "list urls of cabal files for the given packages" += details
        [ "The format for a package is <name>,<version>." ]

cmdPkgBuild = record defPkgBuild
    [ argAppDir, argDbFile
    , patchDir := "patches" += explicit += name "patchdir" += help "location of patches (patches)" += typDir
    , pkgs := def += args += typ "PKG"
    ] += help "create a PKGBUILD, and other files necessary for an Arch package"

cmds = cmdArgsMode_ $ modes_
    [ cmdAddBasePkg
    , cmdAddPkg
    , cmdBuildPkgs
    , cmdBumpPkgs
    , cmdIdxSync
    , cmdIdxVersion
    , cmdListPkgs
    , cmdUpdates
    , cmdUrls
    , cmdPkgBuild
    ]
    += program progName
    += summary (progName ++ " v" ++ (display version))
    += help "maintain a database of dependencies of CABAL packages"

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
            PkgBuild {} -> runReaderT pkgBuild c'
