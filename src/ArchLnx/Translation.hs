module ArchLnx.Translation where

--
import AddCabal
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
--

import PkgDB as DB
import Utils

import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen hiding((</>))
import Data.Version
import Distribution.Text
import Distribution.Package as P
import Data.Char
import Distribution.PackageDescription
import Data.Maybe
import System.FilePath
import System.Unix.Directory
import System.Exit
import System.IO
import System.Process
import Data.List

-- {{{1 ShQuotedString
newtype ShQuotedString = ShQuotedString String
    deriving (Eq, Show)

instance Pretty ShQuotedString where
    pretty (ShQuotedString s) = char '"' <> text s <> char '"'

-- {{{1 ShArray
newtype ShArray = ShArray [String]
    deriving (Eq, Show)

instance Monoid ShArray where
    mempty = ShArray []
    (ShArray a) `mappend` (ShArray b) = ShArray $ a ++ b

instance Pretty ShArray where
    pretty (ShArray a) = encloseSep (char '(') (char ')') (char ' ') (map (\ t -> char '"' <> text t <> char '"') a)

-- {{{1 ShVar
data ShVar a = ShVar String a
    deriving (Eq, Show)

shVarNewValue (ShVar n _) v = ShVar n v

instance Pretty a => Pretty (ShVar a) where
    pretty (ShVar n v) = text n <> char '=' <> pretty v

-- {{{1 ArchPkg
data ArchPkg = ArchPkg
    { apHkgName :: ShVar String
    , apPkgName :: ShVar String
    , apPkgVer :: ShVar Version
    , apPkgRel :: ShVar Int
    , apPkgDesc :: ShVar ShQuotedString
    , apUrl :: ShVar ShQuotedString
    , apLicence :: ShVar ShArray
    , apMakeDepends :: ShVar ShArray
    , apDepends :: ShVar ShArray
    , apSource :: ShVar ShArray
    , apInstall :: Maybe (ShVar ShQuotedString)
    , apSha256Sums :: ShVar ShArray
    } deriving (Eq, Show)

-- {{{2 baseArchPkg
baseArchPkg = ArchPkg
    { apHkgName = ShVar "_hkgname" ""
    , apPkgName = ShVar "pkgname" ""
    , apPkgVer = ShVar "pkgver" (Version [] [])
    , apPkgRel = ShVar "pkgrel" 0
    , apPkgDesc = ShVar "pkgdesc" (ShQuotedString "")
    , apUrl = ShVar "url" (ShQuotedString "http://hackage.haskell.org/package/${_hkgname}")
    , apLicence = ShVar "license" (ShArray [])
    , apMakeDepends = ShVar "makedepends" (ShArray [])
    , apDepends = ShVar "depends" (ShArray [])
    , apSource = ShVar "source" (ShArray [])
    , apInstall = Just $ ShVar "install" (ShQuotedString "${pkgname}.install")
    -- this is a trick to make sure that the user's setting for integrity
    -- checking in makepkg.conf isn't used, as long as this array contains
    -- something non-empty it will overrule
    , apSha256Sums = ShVar "sha256sums" (ShArray ["0"])
    }

-- {{{2 Pretty instance
instance Pretty ArchPkg where
    pretty (ArchPkg
        { apHkgName = hgkName
        , apPkgName = pkgName
        , apPkgVer = pkgVer
        , apPkgRel = pkgRel
        , apPkgDesc = pkgDesc
        , apUrl = url
        , apLicence = pkgLicense
        , apMakeDepends = makeDepends
        , apDepends = depends
        , apSource = source
        , apInstall = install
        , apSha256Sums = sha256sums
        }) = vsep
            [ text "# custom variables"
            , pretty hgkName
            , empty, text "# PKGBUILD options/directives"
            , pretty pkgName
            , pretty pkgVer
            , pretty pkgRel
            , pretty pkgDesc
            , pretty url
            , pretty pkgLicense
            , text "arch=('i686' 'x86_64')"
            , pretty makeDepends
            , pretty depends
            , text "options=('strip')"
            , pretty source
            , maybe empty pretty install
            , pretty sha256sums
            , empty, text "# PKGBUILD functions"
            , buildFunction
            , empty
            , packageFunction
            , empty
            ]
            where
                buildFunction = text "build() {" <>
                    nest 4 (empty <$>
                        text "cd ${srcdir}/${_hkgname}-${pkgver}" <$>
                        nest 4 (text "runhaskell Setup configure -O --enable-split-objs --enable-shared \\" <$>
                            text "--prefix=/usr --docdir=/usr/share/doc/${pkgname} \\" <$>
                            text "--libsubdir=\\$compiler/site-local/\\$pkgid") <$>
                        text "runhaskell Setup build" <$>
                        text "runhaskell Setup haddock" <$>
                        text "runhaskell Setup register --gen-script" <$>
                        text "runhaskell Setup unregister --gen-script" <$>
                        text "sed -i -r -e \"s|ghc-pkg.*unregister[^ ]* |&'--force' |\" unregister.sh"
                        ) <$>
                    char '}'

                packageFunction = text "package() {" <>
                    nest 4 (empty <$>
                        text "cd ${srcdir}/${_hkgname}-${pkgver}" <$>
                        text "install -D -m744 register.sh   ${pkgdir}/usr/share/haskell/${pkgname}/register.sh" <$>
                        text "install    -m744 unregister.sh ${pkgdir}/usr/share/haskell/${pkgname}/unregister.sh" <$>
                        text "install -d -m755 ${pkgdir}/usr/share/doc/ghc/html/libraries" <$>
                        text "ln -s /usr/share/doc/${pkgname}/html ${pkgdir}/usr/share/doc/ghc/html/libraries/${_hkgname}" <$>
                        text "runhaskell Setup copy --destdir=${pkgdir}" <$>
                        text "install -D -m644 LICENSE ${pkgdir}/usr/share/licenses/${pkgname}/LICENSE" <$>
                        text "rm -f ${pkgdir}/usr/share/doc/${pkgname}/LICENSE"
                        ) <$>
                    char '}'

-- {{{1 extra instances
instance Pretty Version where
    pretty (Version b _) = encloseSep empty empty (char '.') (map pretty b)

-- {{{1 translate
-- TODO:
--  • add flags
--  • add patches to sources
translate pd db = let
        ap = baseArchPkg
        (PackageName hkgName) = packageName pd
        pkgVer = packageVersion pd
        pkgRel = 1 -- this should be grabbed out of DB
        hasLib = maybe False (const True) (library pd)
        archName = (if hasLib then "haskell-" else "") ++ (map toLower hkgName)
        pkgDesc = synopsis pd
        url = if null (homepage pd) then "http://hackage.haskell.org/package/${_hkgname}" else (homepage pd)
        lic = display (license pd)
        makeDepends = if hasLib then [] else ["ghc=6.12.3"] ++ calcExactDeps db pd
        depends = if hasLib then ["ghc=6.12.3"] ++ calcExactDeps db pd else []
        src = "http://hackage.haskell.org/packages/archive/" ++ hkgName ++ "/" ++ display pkgVer ++ "/" ++ hkgName ++ ".cabal"
        install = if hasLib then (apInstall ap) else Nothing
    in ap
        { apHkgName = shVarNewValue (apHkgName ap) hkgName
        , apPkgName = shVarNewValue (apPkgName ap) archName
        , apPkgVer = shVarNewValue (apPkgVer ap) pkgVer
        , apPkgRel = shVarNewValue (apPkgRel ap) pkgRel
        , apPkgDesc = shVarNewValue (apPkgDesc ap) (ShQuotedString pkgDesc)
        , apUrl = shVarNewValue (apUrl ap) (ShQuotedString url)
        , apLicence = shVarNewValue (apLicence ap) (ShArray [lic])
        , apMakeDepends = shVarNewValue (apMakeDepends ap) (ShArray makeDepends)
        , apDepends = shVarNewValue (apDepends ap) (ShArray depends)
        , apSource = shVarNewValue (apSource ap) (ShArray [src])
        , apInstall = install
        }

-- Calculate exact dependencies based on the package in a CblDB.  We assume the
-- same db has been used to finalise the package so it's all right to use
-- fromJust.
-- TODO:
--  • this is most likely too simplistic to create the Arch package names
--  correctly for all possible dependencies
calcExactDeps db pd = let
        deps = filter (not . flip elem ghcPkgs) (map depName (buildDepends pd))
        lookupPkgVer = display . DB.pkgVersion . fromJust . lookupPkg db
    in map (\ n -> "haskell-" ++ n ++ "=" ++ (lookupPkgVer n)) deps

-- {{{2 ghcPkgs
-- libraries included in GHC, but not marked as provided by the Arch package
ghcPkgs = ["base", "bin-package-db", "dph-base", "dph-par", "dph-prim-interface", "dph-prim-par", "dph-prim-seq", "dph-seq", "ffi", "ghc", "ghc-binary", "ghc-prim", "integer-gmp", "rts"]

-- {{{1 addHashes
-- TODO:
--  • add PKGBUILD patch support (applied before running makepkg -g)
--  • add patch support (copy the cabal and build patch into the dir)
--  • deal with errors better
addHashes ap tmpDir = let
        hashes = map (filter (`elem` "1234567890abcdef")) . lines . drop 11
    in do
        writeFile (tmpDir </> "PKGBUILD") (show $ pretty ap)
        (ec, out, er) <- withWorkingDirectory tmpDir (readProcessWithExitCode "makepkg" ["-g"] "")
        case ec of
            ExitFailure _ -> do
                hPutStrLn stderr er
                hPutStrLn stderr "makepkg: error while calculating the source hashes"
                return ap
            ExitSuccess -> do
                if "sha256sums=(" `isPrefixOf` out
                    then return ap { apSha256Sums = shVarNewValue (apSha256Sums ap) (ShArray $ hashes out) }
                    else return ap
