# What is cblrepo?

The goal of `cblrepo` is to aid in maintaining a consistent set of Haskell packages, e.g. for a Linux distribution.  Currently it's heavily influenced by the work required to maintain Haskell packages for [ArchLinux](http://www.archlinux.org/), but it's proven useful also for other distributions.

# Building it

It uses CABAL, so as soon as its dependencies are satisfied it builds and installs with the familiar 3 steps:

    $ ./Setup.hs configure
    $ ./Setup.hs build
    $ ./Setup.hs install

Alternatively one can use the `cabal` tool:

   $ cabal install

# Using it

The following sections cover some of the more used commands, but it's not an exhaustive description of the tool.  One can list all commands can be accessed using

    $ cblrepo --help

and the help for an individual command is accessed using

    $ cblrepo <command> --help

## Syncing with Hackage

The program can maintain a cache of all the packages on Hackage, this cache is used in some of the commands.  The cache is updated using

    $ cblrepo sync

By default the cache is stored in `~/.cblrepo/`, the location can be controlled using `--appdir=`.

## Adding packages

All added packages are kept in a database, named `cblrepo.db`, which should be in the current directory (unless the `--db=` argument is used).  The database will be created on first add, if it doesn't exist already.

To `cblrepo` there are three types of packages:

*GHC package* -- A package provided by GHC, e.g. `base`.  Added using the `-g` (or `--ghc-pkg=`) flag.  Multiple package can be added at the same time by using the flag multiple times.  For each package only the package name and version is recorded.  Ex:

    $ cblrepo add -g base,4.3.1.0 -g array,0.3.0.2

*Distro package* -- A package provided by the distribution.  Added using the `-d` (or `--distro-pkg=`) flag.  Multiple package can be added at the same time using the flag multiple times.  For each package package name, version and release is recorded.  Ex:

    $ cblrepo add -d xmonad-contrib,0.10,1 -d zlib,0.5.3.1,2.1

*Repo package* -- A package maintained using `cblrepo`.  Added using either `-u`/`--cbl-url` (adding by URL), `-f`/`--cbl-file` (adding by file name), or by just giving the package name and version on the command line (the details are then extracted from the cache).  Ex:

    $ cblrepo add -u http://hackage.haskell.org/packages/archive/dataenc/0.14.0.3/dataenc.cabal
    $ cblrepo add -f my-cbl-files/dataenc.cabal
    $ cblrepo add dataenc,0.14.0.3

The release number after adding is set to 1.

If there are any unsatisfiable dependencies they will be reported by `cblrepo` and no changes will be made to the database.

## Updating packages

The *add* command is used to update packages as well.

## Adding patches for packages

TBD

## Generating PKGBUILDs for packages

Use the *pkgbuild* command to generate the source ArchLinux package.

    $ cblrepo pkgbuild yesod

# Contact

Please report bugs and suggestions for improvements at [github](https://github.com/magthe/cblrepo).
