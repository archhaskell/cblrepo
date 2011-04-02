module Util.Translation where

--
import AddCabal
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
--

import PkgDB as DB
import Util.Misc

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
-- TODO: license file, patches, flags
data ArchPkg = ArchPkg
    { apPkgName :: String
    , apHasLibrary :: Bool
    , apLicenseFile :: Maybe FilePath
    -- shell bits
    , apShHkgName :: ShVar String
    , apShPkgName :: ShVar String
    , apShPkgVer :: ShVar Version
    , apShPkgRel :: ShVar Int
    , apShPkgDesc :: ShVar ShQuotedString
    , apShUrl :: ShVar ShQuotedString
    , apShLicence :: ShVar ShArray
    , apShMakeDepends :: ShVar ShArray
    , apShDepends :: ShVar ShArray
    , apShSource :: ShVar ShArray
    , apShInstall :: Maybe (ShVar ShQuotedString)
    , apShSha256Sums :: ShVar ShArray
    } deriving (Eq, Show)

-- {{{2 baseArchPkg
baseArchPkg = ArchPkg
    { apPkgName = ""
    , apHasLibrary = False
    , apLicenseFile = Nothing
    , apShHkgName = ShVar "_hkgname" ""
    , apShPkgName = ShVar "pkgname" ""
    , apShPkgVer = ShVar "pkgver" (Version [] [])
    , apShPkgRel = ShVar "pkgrel" 0
    , apShPkgDesc = ShVar "pkgdesc" (ShQuotedString "")
    , apShUrl = ShVar "url" (ShQuotedString "http://hackage.haskell.org/package/${_hkgname}")
    , apShLicence = ShVar "license" (ShArray [])
    , apShMakeDepends = ShVar "makedepends" (ShArray [])
    , apShDepends = ShVar "depends" (ShArray [])
    , apShSource = ShVar "source" (ShArray ["http://hackage.haskell.org/packages/archive/${_hkgname}/${pkgver}/${_hkgname}-${pkgver}.tar.gz"])
    , apShInstall = Just $ ShVar "install" (ShQuotedString "${pkgname}.install")
    -- this is a trick to make sure that the user's setting for integrity
    -- checking in makepkg.conf isn't used, as long as this array contains
    -- something non-empty it will overrule
    , apShSha256Sums = ShVar "sha256sums" (ShArray ["0"])
    }

-- {{{2 Pretty instance
instance Pretty ArchPkg where
    pretty (ArchPkg
        { apHasLibrary = hasLib
        , apLicenseFile = licenseFile
        , apShHkgName = hgkName
        , apShPkgName = pkgName
        , apShPkgVer = pkgVer
        , apShPkgRel = pkgRel
        , apShPkgDesc = pkgDesc
        , apShUrl = url
        , apShLicence = pkgLicense
        , apShMakeDepends = makeDepends
        , apShDepends = depends
        , apShSource = source
        , apShInstall = install
        , apShSha256Sums = sha256sums
        }) = vsep
            [ text "# custom variables"
            , pretty hgkName
            , maybe empty (\ fn -> pretty $ ShVar "_licensefile" fn) licenseFile
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
            , if hasLib then libBuildFunction else exeBuildFunction
            , empty
            , if hasLib then libPackageFunction else exePackageFunction
            , empty
            ]
            where
                libBuildFunction = text "build() {" <>
                    nest 4 (empty <$>
                        text "cd ${srcdir}/${_hkgname}-${pkgver}" <$>
                        nest 4 (text "runhaskell Setup configure -O -p --enable-split-objs --enable-shared \\" <$>
                            text "--prefix=/usr --docdir=/usr/share/doc/${pkgname} \\" <$>
                            text "--libsubdir=\\$compiler/site-local/\\$pkgid") <$>
                        text "runhaskell Setup build" <$>
                        text "runhaskell Setup haddock" <$>
                        text "runhaskell Setup register --gen-script" <$>
                        text "runhaskell Setup unregister --gen-script" <$>
                        text "sed -i -r -e \"s|ghc-pkg.*unregister[^ ]* |&'--force' |\" unregister.sh"
                        ) <$>
                    char '}'

                exeBuildFunction = text "build() {" <>
                    nest 4 (empty <$> text "cd ${srcdir}/${_hkgname}-${pkgver}" <$>
                        text "runhaskell Setup configure -O --prefix=/usr --docdir=/usr/share/doc/${pkgname}" <$>
                        text "runhaskell Setup build"
                        ) <$>
                    char '}'

                libPackageFunction = text "package() {" <>
                    nest 4 (empty <$>
                        text "cd ${srcdir}/${_hkgname}-${pkgver}" <$>
                        text "install -D -m744 register.sh   ${pkgdir}/usr/share/haskell/${pkgname}/register.sh" <$>
                        text "install    -m744 unregister.sh ${pkgdir}/usr/share/haskell/${pkgname}/unregister.sh" <$>
                        text "install -d -m755 ${pkgdir}/usr/share/doc/ghc/html/libraries" <$>
                        text "ln -s /usr/share/doc/${pkgname}/html ${pkgdir}/usr/share/doc/ghc/html/libraries/${_hkgname}" <$>
                        text "runhaskell Setup copy --destdir=${pkgdir}" <$>
                        (maybe empty (\ _ -> text "install -D -m644 ${_licensefile} ${pkgdir}/usr/share/licenses/${pkgname}/LICENSE" <$>
                            text "rm -f ${pkgdir}/usr/share/doc/${pkgname}/${_licensefile}") licenseFile)
                        ) <$>
                    char '}'

                exePackageFunction = text "package() {" <>
                    nest 4 (empty <$> text "cd ${srcdir}/${_hkgname}-${pkgver}" <$>
                        text "runhaskell Setup copy --destdir=${pkgdir}"
                        ) <$>
                    char '}'

-- {{{1 ArchInstall
data ArchInstall = ArchInstall
    { aiShPkgName :: ShVar String
    } deriving (Eq, Show)

-- {{{2 baseArchInstall
baseArchInstall = ArchInstall
    { aiShPkgName = ShVar "pkgname" ""
    }

-- {{{2 ArchInstall from ArchPackage
aiFromAP (ArchPkg { apShPkgName = pkgName }) =
    ArchInstall { aiShPkgName = pkgName }

-- {{{2 Pretty instance
instance Pretty ArchInstall where
    pretty (ArchInstall
        { aiShPkgName = pkgName
        }) = vsep
            [ text "# custom variables"
            , pretty pkgName
            , pretty (ShVar "HS_DIR" "usr/share/haskell/${pkgname}")
            , empty, text "# functions"
            , postInstallFunction
            , empty, preUpgradeFunction
            , empty, postUpgradeFunction
            , empty, preRemoveFunction
            , empty, postRemoveFunction
            , empty
            ]
            where
                postInstallFunction = text "post_install() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/register.sh" <$>
                        text "(cd usr/share/doc/ghc/html/libraries; ./gen_contents_index)") <$>
                    char '}'
                preUpgradeFunction = text "pre_upgrade() {" <>
                    nest 4 (empty <$> text "${HS_DIR}/register.sh") <$>
                    char '}'
                postUpgradeFunction = postInstallFunction
                preRemoveFunction = preUpgradeFunction
                postRemoveFunction = postInstallFunction

-- {{{1 extra instances
instance Pretty Version where
    pretty (Version b _) = encloseSep empty empty (char '.') (map pretty b)

-- {{{1 translate
-- TODO:
--  • add flags
--  • add patches to sources
--  • translation of extraLibDepends-libs to Arch packages
translate db pd = let
        ap = baseArchPkg
        (PackageName hkgName) = packageName pd
        pkgVer = packageVersion pd
        pkgRel = maybe 1 pkgRelease (lookupPkg db hkgName)
        hasLib = maybe False (const True) (library pd)
        licFn = let l = licenseFile pd in if null l then Nothing else Just l
        archName = (if hasLib then "haskell-" else "") ++ (map toLower hkgName)
        pkgDesc = synopsis pd
        url = if null (homepage pd) then "http://hackage.haskell.org/package/${_hkgname}" else (homepage pd)
        lic = display (license pd)
        makeDepends = if hasLib then [] else ["ghc=7.0.2"] ++ calcExactDeps db pd
        depends = if hasLib then ["ghc=7.0.2"] ++ calcExactDeps db pd else []
        extraLibDepends = maybe [] (extraLibs . libBuildInfo) (library pd)
        install = if hasLib then (apShInstall ap) else Nothing
    in ap
        { apPkgName = archName
        , apHasLibrary = hasLib
        , apLicenseFile = licFn
        , apShHkgName = shVarNewValue (apShHkgName ap) hkgName
        , apShPkgName = shVarNewValue (apShPkgName ap) archName
        , apShPkgVer = shVarNewValue (apShPkgVer ap) pkgVer
        , apShPkgRel = shVarNewValue (apShPkgRel ap) pkgRel
        , apShPkgDesc = shVarNewValue (apShPkgDesc ap) (ShQuotedString pkgDesc)
        , apShUrl = shVarNewValue (apShUrl ap) (ShQuotedString url)
        , apShLicence = shVarNewValue (apShLicence ap) (ShArray [lic])
        , apShMakeDepends = shVarNewValue (apShMakeDepends ap) (ShArray makeDepends)
        , apShDepends = shVarNewValue (apShDepends ap) (ShArray $ depends ++ extraLibDepends)
        , apShInstall = install
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
    in map (\ n -> "haskell-" ++ (map toLower n) ++ "=" ++ (lookupPkgVer n)) deps

-- {{{2 ghcPkgs
-- libraries included in GHC, but not marked as provided by the Arch package
ghcPkgs = ["base", "bin-package-db", "ffi", "ghc", "ghc-binary", "ghc-prim", "haskell2010", "integer-gmp", "rts"]

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
                    then return ap { apShSha256Sums = shVarNewValue (apShSha256Sums ap) (ShArray $ hashes out) }
                    else return ap
