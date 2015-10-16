{-
 - Copyright 2011-2014 Per Magnus Therning
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

-- {{{1 imports
import Add
import BuildPkgs
import BumpPkgs
import Update
import Versions
import ListPkgs
import Upgrades
import Util.Misc
import PkgBuild
import ConvertDB
import Remove
import Extract
import CreateConfig
import Util.Cfg

import Paths_cblrepo

import Distribution.Text
import Options.Applicative as OA
import System.Directory

-- -- {{{1 command line arguments
argAppDir, argDbFile :: Parser String
argAppDir = strOption (long "appdir" <> value "" <> showDefault  <> help "Path to application data directory")
argDbFile = strOption (long "db" <> value "cblrepo.db" <> showDefault  <> help "Path to package database")

argDryRun :: Parser Bool
argDryRun = switch (short 'n' <> help "Make no changes, (dry run)")

cmdAddPkgOpts :: Parser Cmds
cmdAddPkgOpts = CmdAdd
                <$> strOption (long "patchdir" <> value "patches" <> showDefault <> help "Location of patches")
                <*> option ghcVersionArgReader (long "ghc-version" <> value ghcDefVersion <> showDefault <> help "GHC version to use")
                <*> many (option ghcPkgArgReader (short 'g' <> long "ghc-pkg" <> metavar "PKG,VER" <> help "GHC base package (multiple)"))
                <*> many (option distroPkgArgReader (short 'd' <> long "distro-pkg" <> metavar "PKG,VER,REL" <> help "Distro package (multiple)"))
                <*> many (option strCblFileArgReader (short 'f' <> long "cbl-file" <> metavar "FILE[:flag,-flag]" <> help "CABAL file (multiple)"))
                <*> many (argument strCblPkgArgReader (metavar "PKGNAME,VERSION[:flag,-flag] ..."))

cmdAddPkgCmd = command "add" (info (helper <*> cmdAddPkgOpts) (fullDesc <> progDesc "Add a package to the database"))

cmdBumpPkgsCmd = command "bump" (info (helper <*> cmdBumpPkgsOpts) (fullDesc <> progDesc "Bump packages that need it after updating the named packages"))
  where
    cmdBumpPkgsOpts = CmdBumpPkgs
                      <$> switch (long "inclusive" <> help "Include the listed packages")
                      <*> some (strArgument (metavar "PKGNAME ..."))

cmdBuildPkgsCmd = command "build" (info (helper <*> cmdBuildPkgsOpts)
                                   (fullDesc <> progDesc "Re-order packages into a good build order"))
  where
    cmdBuildPkgsOpts = CmdBuildPkgs <$> some (strArgument (metavar "PKGNAME ..."))

cmdUpdateCmd = command "update" (info (helper <*> cmdUpdateOpts) (fullDesc <> progDesc "Update the index"))
  where
    cmdUpdateOpts = CmdUpdate <$> switch (internal <> hidden)

cmdVersionsCmd = command "versions" (info (helper <*> cmdVersionsOpts) (fullDesc <> progDesc "List available versions of packages"))
  where
    cmdVersionsOpts = CmdVersions
                      <$> switch (short 'l' <> long "latest" <> help "List only the latest version of packages")
                      <*> some (strArgument (metavar "PKGNAME ..."))

cmdUpgradesCmd = command "upgrades" (info (helper <*> cmdUpgradesOpts) (fullDesc <> progDesc "Check for packages that can be upgraded"))
  where
    cmdUpgradesOpts = CmdUpgrades <$> switch (short 's' <> help "A shorter output suitable for scripting")

cmdListPkgsCmd = command "list" (info (helper <*> cmdListPkgsOpts) (fullDesc <> progDesc "List packages in repo"))
  where
    cmdListPkgsOpts = CmdListPkgs
                      <$> switch (short 'g' <> long "ghc" <> help "List ghc packages")
                      <*> switch (short 'd' <> long "distro" <> help "List distro packages")
                      <*> switch (long "no-repo" <> help "Do not list repo packages")
                      <*> option listFormatReader (short 'f' <> long "format" <> value CmdListNormalFmt <> help "Output format: short, normal, hackage (default: normal)")
                      <*> many (argument str (metavar "PKGNAME ..."))

cmdPkgBuildCmd = command "pkgbuild" (info (helper <*> cmdPkgBuildOpts) (fullDesc <> progDesc "Create PKGBUILD other files necessary for an Arch package"))
  where
    cmdPkgBuildOpts = CmdPkgBuild
                      <$> option ghcVersionArgReader (long "ghc-version" <> value ghcDefVersion <> help "GHC version to use in PKGBUILD (default: 7.10.2)")
                      <*> option auto (long "ghc-release" <> value ghcDefRelease <> showDefault <> help "GHC release to use in PKGBUILD")
                      <*> strOption (long "patchdir" <> value "patches" <> showDefault  <> help "Location of patches")
                      <*> some (strArgument (metavar "PKGNAME ..."))

cmdConvertDbCmd = command "convertdb" (info (helper <*> cmdConvertDbOpts) (fullDesc <> progDesc "Convert an old database to the new format"))
  where
    cmdConvertDbOpts = CmdConvertDb
                       <$> strOption (short 'i' <> long "indb" <> value "cblrepo.db" <> showDefault  <> help "Old database")
                       <*> strOption (short 'o' <> long "outdb" <> value "new-cblrepo.db" <> showDefault  <> help "New database")

cmdRemovePkgCmd = command "rm" (info (helper <*> cmdRemovePkgOpts) (fullDesc <> progDesc "Remove packages"))
  where
    cmdRemovePkgOpts = CmdRemovePkg <$> some (strArgument (metavar "PKGNAME ..."))

cmdExtractCmd = command "extract" (info (helper <*> cmdExtractOpts) (fullDesc <> progDesc "Extract Cabal file from index"))
  where
    cmdExtractOpts = CmdExtract <$> many (argument pkgNVersionArgReader (metavar "PKGNAME,VERSION"))

cmdCreateConfigCmd = command "create-config" (info (helper <*> cmdCreateConfigOpts) (fullDesc <> progDesc "Create configuration file with defaults"))
  where
    cmdCreateConfigOpts = pure CmdCreateConfig

argParser = info (helper <*> opts) (fullDesc <> header (progName ++ " v" ++ display version) <> progDesc "Maintain a database of dependencies of CABAL packages")
  where
    opts = Opts
           <$> argAppDir <*> argDbFile <*> argDryRun
           <*> subparser (cmdAddPkgCmd <> cmdBumpPkgsCmd <> cmdBuildPkgsCmd <> cmdUpdateCmd <> cmdVersionsCmd <> cmdUpgradesCmd <>
                          cmdListPkgsCmd <> cmdPkgBuildCmd <> cmdConvertDbCmd <> cmdRemovePkgCmd <> cmdExtractCmd <> cmdCreateConfigCmd)

-- {{{1 main
main :: IO ()
main = do
  defAppDir <- getAppUserDataDirectory progName
  execParser argParser >>= \ o -> do
    let aD = if null (appDir o) then defAppDir else appDir o
    createDirectoryIfMissing True aD
    cfg <- readCfg "cblrepo.cfg"
    let e = (o { appDir = aD }, cfg)
    case optsCmd o of
      CmdAdd {} -> runCommand e add
      CmdBuildPkgs {} -> runCommand e buildPkgs
      CmdBumpPkgs {} -> runCommand e bumpPkgs
      CmdUpdate {} -> runCommand e update
      CmdVersions {} -> runCommand e versions
      CmdListPkgs {} -> runCommand e listPkgs
      CmdUpgrades {} -> runCommand e upgrades
      CmdPkgBuild {} -> runCommand e pkgBuild
      CmdConvertDb {} -> runCommand e convertDb
      CmdRemovePkg {} -> runCommand e remove
      CmdExtract {} -> runCommand e extract
      CmdCreateConfig -> runCommand e createConfig
