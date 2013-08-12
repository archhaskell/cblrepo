{-
 - Copyright 2011-2013 Per Magnus Therning
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

import Add
import BuildPkgs
import BumpPkgs
import Sync
import Versions
import ListPkgs
import Updates
import Util.Misc
import Urls
import PkgBuild
import ConvertDB
import Remove

import Paths_cblrepo

import Control.Monad
import Control.Monad.Reader
import Distribution.Text
import System.Directory
import System.FilePath
import Options.Applicative as OA

-- -- {{{1 command line arguments
argAppDir = strOption (long "appdir" <> value "" <> help "application data directory")
argDbFile = strOption (long "db" <> value "cblrepo.db" <> help "package database")
argDryRun = switch (short 'n' <> help "dry run")

cmdAddPkgOpts = CmdAdd
    <$> argAppDir <*> argDbFile
    <*> strOption (long "patchdir" <> value "patches" <> help "location of patches (patches)")
    <*> argDryRun
    <*> many (nullOption (short 'g' <> long "ghc-pkg" <> OA.reader strPairArg <> metavar "PKG,VER" <> help "GHC base package (multiple)"))
    <*> many (nullOption (short 'd' <> long "distro-pkg" <> OA.reader strTripleArg <> metavar "PKG,VER,REL" <> help "distro package (multiple)"))
    <*> many (strOption (short 'u' <> long "cbl-url" <> metavar "URL" <> help "url of CABAL file (multiple)"))
    <*> many (strOption (short 'f' <> long "cbl-file" <> metavar "FILE" <> help "CABAL file (multiple)"))
    <*> arguments strPairArg (metavar "PKGNAME,VERSION ...")
cmdAddPkgCmd = command "add" (info (helper <*> cmdAddPkgOpts)
    (fullDesc <> progDesc "add a package to the database"))

cmdBumpPkgsOpts = CmdBumpPkgs
    <$> argAppDir <*> argDbFile <*> argDryRun
    <*> switch (long "inclusive" <> help "include the listed packages")
    <*> arguments1 Just (metavar "PKGNAME ...")
cmdBumpPkgsCmd = command "bump" (info (helper <*> cmdBumpPkgsOpts)
    (fullDesc <> progDesc "bump packages that need it after updating the named packages"))

cmdBuildPkgsOpts = CmdBuildPkgs
    <$> argAppDir <*> argDbFile
    <*> arguments1 Just (metavar "PKGNAME ...")
cmdBuildPkgsCmd = command "build" (info (helper <*> cmdBuildPkgsOpts)
    (fullDesc <> progDesc "re-order packages into a good build order"))

cmdSyncOpts = CmdSync <$> argAppDir
cmdSyncCmd = command "sync" (info (helper <*> cmdSyncOpts)
    (fullDesc <> progDesc "update the index"))

cmdVersionsOpts = CmdVersions
    <$> argAppDir
    <*> arguments1 Just (metavar "PKGNAME ...")
cmdVersionsCmd = command "versions" (info (helper <*> cmdVersionsOpts)
    (fullDesc <> progDesc "list available versions of packages"))

cmdUpdatesOpts = CmdUpdates
    <$> argAppDir <*> argDbFile
    <*> switch (short 's' <> help "a shorter output suitable for scripting")
cmdUpdatesCmd = command "updates" (info (helper <*> cmdUpdatesOpts)
    (fullDesc <> progDesc "check for available updates"))

cmdListPkgsOpts = CmdListPkgs
    <$> argAppDir <*> argDbFile
    <*> switch (short 'g' <> long "ghc" <> help "list ghc packages")
    <*> switch (short 'd' <> long "distro" <> help "list distro packages")
    <*> switch (long "no-repo" <> help "do not list repo packages")
    <*> switch (long "hackage" <> help "list in hackage format")
cmdListPkgsCmd = command "list" (info (helper <*> cmdListPkgsOpts)
    (fullDesc <> progDesc "list packages in repo"))

cmdUrlsOpts = CmdUrls
    <$> argAppDir
    <*> arguments1 strPairArg (metavar "PKGNAME,VERSION ...")
cmdUrlsCmd = command "urls" (info (helper <*> cmdUrlsOpts)
    (fullDesc <> progDesc "list urls of CABAL files for the given packages"))

cmdPkgBuildOpts = CmdPkgBuild
    <$> argAppDir <*> argDbFile
    <*> strOption (long "patchdir" <> value "patches" <> help "location of patches (patches)")
    <*> arguments1 Just (metavar "PKGNAME ...")
cmdPkgBuildCmd = command "pkgbuild" (info (helper <*> cmdPkgBuildOpts)
    (fullDesc <> progDesc "create PKGBUILD other files necessary for an Arch package"))

cmdConvertDbOpts = CmdConvertDb
    <$> argAppDir
    <*> strOption (short 'i' <> long "indb" <> value "cblrepo.db" <> help "old database")
    <*> strOption (short 'o' <> long "outdb" <> value "new-cblrepo.db" <> help "new database")
cmdConvertDbCmd = command "convertdb" (info (helper <*> cmdConvertDbOpts)
    (fullDesc <> progDesc "convert and old database to the new format"))

cmdRemovePkgOpts = CmdRemovePkg
    <$> argAppDir <*> argDbFile <*> argDryRun
    <*> arguments1 Just (metavar "PKGNAME ...")
cmdRemovePkgCmd = command "rm" (info (helper <*> cmdRemovePkgOpts)
    (fullDesc <> progDesc "remove packages"))

argParser = info (helper <*> opts) (fullDesc <> header (progName ++ " v" ++ (display version)) <> progDesc "maintain a datatbase of dependencies of CABAL packages")
    where
        opts = Opts <$> subparser (
            cmdAddPkgCmd <> cmdBumpPkgsCmd <> cmdBuildPkgsCmd <> cmdSyncCmd <> cmdVersionsCmd <>
            cmdUpdatesCmd <> cmdListPkgsCmd <> cmdUrlsCmd <> cmdPkgBuildCmd <> cmdConvertDbCmd <> cmdRemovePkgCmd)

-- {{{1 main
main = do
    defAppDir <- getAppUserDataDirectory progName
    execParser argParser >>= \ (Opts c) -> do
        let aD = if null (appDir c) then defAppDir else (appDir c)
        let c' = c { appDir = aD }
        createDirectoryIfMissing True (aD)
        case c' of
            CmdAdd {} -> runCommand c' add
            CmdBuildPkgs {} -> runCommand c' buildPkgs
            CmdBumpPkgs {} -> runCommand c' bumpPkgs
            CmdSync {} -> runCommand c' sync
            CmdVersions {} -> runCommand c' versions
            CmdListPkgs {} -> runCommand c' listPkgs
            CmdUpdates {} -> runCommand c' updates
            CmdUrls {} -> runCommand c' urls
            CmdPkgBuild {} -> runCommand c' pkgBuild
            CmdConvertDb {} -> runCommand c' convertDb
            CmdRemovePkg {} -> runCommand c' remove
