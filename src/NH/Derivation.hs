{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module NH.Derivation
  ( getDerivation
  , internDerivation
  -- *
  , drvFieldsPkgSet
  , drvFieldType
  , drvFieldNixName
  )
where

import           Control.Monad                       (forM_, unless)
import           Control.Monad.Plus                  (partial)
import           Control.Monad.IO.Class

import qualified Data.Aeson                       as AE
import           Data.Char
import qualified Data.Default.Class               as DD
import           Data.Foldable
import qualified Data.List                        as L
import           Data.Maybe
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Data.Set.Lens
import           Data.String
import           Data.Text                           (pack, unpack)
import qualified Data.Text                        as T
import           Data.Text.Format             hiding (print)

import           Language.Nix.PrettyPrinting  hiding ((<>), empty)
import qualified Language.Nix.PrettyPrinting      as Nix

import qualified Network.HTTP.Req                 as HTTP
import           Network.HTTP.Req                    (Url, Scheme(..), (/:))

import qualified Nix.Parser                       as Nix
import qualified Nix.Pretty                       as Nix
import qualified Nix.Expr                         as Nix

import qualified Options.Applicative              as O
import           Options.Applicative

import           Prelude.Unicode

import qualified System.Environment               as Sys
import qualified System.IO                        as Sys
import qualified System.IO.Temp                   as Sys

import qualified Text.PrettyPrint.ANSI.Leijen     as PP
import           Text.Printf

import           Data.Hourglass

-- import Control.Exception ( bracket )
import Control.Lens                           hiding (argument)
-- import Control.Monad ( when )
-- import Data.Maybe ( fromMaybe, isJust )
-- import Data.Monoid ( (<>) )
-- import qualified Data.Set as Set
-- import Data.String
-- import Data.Time
import qualified Distribution.Compat.ReadP as P
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import qualified Distribution.Nixpkgs.Haskell.FromCabal.PostProcess as PP (pkg)
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Distribution.Nixpkgs.Haskell.PackageSourceSpec as Nixpkgs
import Distribution.Nixpkgs.Haskell.PackageSourceSpec hiding (Package)
import Distribution.Nixpkgs.Meta
import Distribution.PackageDescription ( mkFlagName, FlagAssignment, FlagName, unFlagName, unFlagAssignment, mkFlagAssignment )
import Distribution.Package ( packageId, packageName, packageVersion )
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Text
import Language.Nix
-- import Paths_cabal2nix ( version )
-- import System.Environment ( getArgs )
-- import System.IO ( hFlush, hPutStrLn, stdout, stderr )
import qualified Text.PrettyPrint.ANSI.Leijen as P2
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(..), text, vcat, hcat, semi )

-- #if MIN_VERSION_base(4,11,0)
-- import Distribution.PackageDescription ( unFlagAssignment, mkFlagAssignment )

import           NH.Config                             (Config(..))
import qualified NH.Config                          as CFG
import           NH.Misc
import           NH.Nix
import           NH.Types

import           NH.MRecord



drvFieldType ∷ DrvField → NixType
drvFieldType DFdrvparams = NTList NTVar
drvFieldType DFpname = NTStr
drvFieldType DFversion = NTStr
drvFieldType DFsrcUrl = NTStr
drvFieldType DFsrcSha256 = NTStr -- nixhash
drvFieldType DFsrcRev = NTStr
drvFieldType DFsubpath = NTStr
drvFieldType DFrevision = NTInt
drvFieldType DFeditedCabalFile = NTStr -- nixhash
drvFieldType DFconfigureFlags = NTList NTStr
drvFieldType DFisLibrary = NTBool
drvFieldType DFisExecutable = NTBool
drvFieldType DFenableSeparateDataOutput = NTBool
drvFieldType _ = NTList NTVar



internDerivation ∷ Derivation → SrcSpec → Package
internDerivation drv sspec =
  let SSGithub{..} = case sspec of
                       x@SSGithub{..} → x
                       _ → error "Non-Github imports not supported."

      meAttrName         = Nothing -- only applies to versioned Nixpkgs attrs
      meChdir            = if drvFieldIsNondefault drv DFsubpath
                           then Just (T.pack $ drv^.subpath) else Nothing
      meDisable          = KeepOverride
      meErdeps           = mempty
      meEssentialRevDeps = []      -- initially not tracked
      meExplanation      = mempty
      meLocal            = ToLocal
      meRepoName         = if fromRepoName ssRepoName ≢ fromAttr ssAttr
                           then Just ssRepoName
                           else Nothing
      meTargets          = ToLocal
      pkMeta             = Meta{..}

      upRepoName         = ssRepoName
      upUser             = ssUser
      upIssue            = Nothing
      upPr               = Nothing
      upTimestamp        = Nothing -- XXX: loss
      pkUpstream         = Upstream{..}

      ghRev              = GitRef $ pack $ derivRevision $ drv^.src
      srNixHash          = NixHash $ pack $ derivHash $ drv^.src
      ghRepoName         = ssRepoName
      ghUser             = ssUser -- XXX: assumption
      pkSrc              = Github{..}

      nonDefaultDrvFields = piecewiseDerivation drv
      ovDrvFields        = Map.fromList $ flip filter nonDefaultDrvFields $
        \(k, _)→ Set.member k drvFieldsForOverrides
      ovDoCheck          = DoCheck
      ovDoHaddock        = DoHaddock 
      ovInputs           = mempty
      ovJailbreak        = DontJailbreak
      ovRevision         = KeepRevision
      ovSrc              = Just pkSrc
      ovPatches          = mempty
      pkOver             = Overrides{..}

      dmDescription      = showDocOneLine ∘ flip pprintField DFmetaSectionDescription <$>
                           partial (flip drvFieldIsNondefault DFmetaSectionDescription) drv
      dmLicense          = showDocOneLine $ pprintField drv DFmetaSectionLicense
      dmHomepage         = Nothing
      dmPlatforms        = Nothing
      dmMaintainers      = Nothing
      pkDrvMeta          = DrvMeta{..}

      pkAttr             = ssAttr
      -- * Pass-through fields.
      pkDrvFields        = Map.fromList $ flip filter nonDefaultDrvFields $
        \(k, _)→ not $ any (Set.member k) drvPkgExclusionSets
  in Package{..}


-- * Cabal2nix URLs
(//) ∷ T.Text → T.Text → T.Text
x // y = x<>"/"<>y

attrC2NUrl ∷ SrcSpec → URL
attrC2NUrl  (SSGithub attr user repo ref msub) = URL $ "https://github.com"//fromUser user//fromRepoName repo
attrC2NUrl (SSHackage attr msub)               = URL $ "cabal://"<>fromAttr attr


getDerivation ∷ CompilerId → Platform → SrcSpec → IO Derivation
getDerivation oCompiler oSystem sspec = do
  let optHpack           = False
      optHackageDb       = Nothing
      optHackageSnapshot = Nothing
      optUrl             = T.unpack $ fromURL $ attrC2NUrl sspec
      optRevision        = Nothing
      optSha256          = Nothing
      optSubpath         = T.unpack ∘ fromDir <$> ssDir sspec
      optSystem          = oSystem
      optCompiler        = oCompiler
      optExtraArgs       = []
      opts               = ImportOptions{..}
  package ← getPackage optHpack optHackageDb optHackageSnapshot $
            Source optUrl (fromMaybe "" optRevision) (maybe UnknownHash Guess optSha256) (fromMaybe "" optSubpath)
  pure $ packageDerivation opts package


data ImportOptions = ImportOptions
  { optCompiler    ∷ CompilerId
  , optSystem      ∷ Platform
  , optSubpath     ∷ Maybe FilePath
  , optExtraArgs   ∷ [String]
  }

packageDerivation ∷ ImportOptions → Nixpkgs.Package → Derivation
packageDerivation ImportOptions{..} pkg = do
  let
      withHpackOverrides :: Derivation -> Derivation
      withHpackOverrides = if pkgRanHpack pkg then hpackOverrides else id

      hpackOverrides :: Derivation -> Derivation
      hpackOverrides = over phaseOverrides (<> "preConfigure = \"hpack\";")
                       . set (libraryDepends . tool . contains (PP.pkg "hpack")) True

      flags :: FlagAssignment
      flags = configureCabalFlags (packageId (pkgCabal pkg))

      deriv :: Derivation
      deriv = withHpackOverrides $ fromGenericPackageDescription (const True)
                                            (\i -> Just (binding # (i, path # [i])))
                                            optSystem
                                            (unknownCompilerInfo optCompiler NoAbiTag)
                                            flags
                                            []
                                            (pkgCabal pkg)
              & src .~ pkgSource pkg
              & subpath .~ fromMaybe "." optSubpath
              & extraFunctionArgs %~ Set.union (Set.fromList ("inherit stdenv":map (fromString . ("inherit " ++)) optExtraArgs))
  deriv



--
-- * Field-wise 'Derivation'
--
nestedPrefixes ∷ [(T.Text, Field)]
nestedPrefixes =
  [("src",         "src")
  ,("metaSection", "meta")
  ]

drvFieldNestPrefix ∷ DrvField → Maybe (T.Text, Field)
drvFieldNestPrefix df =
  let s = pack $ drop 2 $ show df
  in flip find nestedPrefixes (flip T.isPrefixOf s ∘ fst)

drvFieldNixName ∷ DrvField → T.Text
drvFieldNixName df =
  let s         = pack $ drop 2 $ show df
      detitle x = T.toLower (T.take 1 x) <> T.drop 1 x
  in case drvFieldNestPrefix df of
       Nothing       → s
       Just (pfx, _) → detitle $ T.drop (T.length pfx) s

isHackagePackage ∷ Derivation → Bool
isHackagePackage drv = "mirror://hackage/" `L.isPrefixOf` derivUrl (drv^.src)

piecewiseDerivation ∷ Derivation → [(DrvField, DFValue)]
piecewiseDerivation drv =
  [ (,) field $ DFValue field (pprintField drv field)
  | field ← every
  , drvFieldIsNondefault drv field ]

drvFieldsPkgSet = Set.difference (Set.fromList every) $ foldl (<>) mempty drvPkgExclusionSets
drvFieldsPkgSet, drvFieldsForDrvMeta, drvFieldsForOverrides, drvFieldsForSrc  ∷ Set.Set DrvField
drvPkgExclusionSets@[drvFieldsForDrvMeta, drvFieldsForOverrides, drvFieldsForSrc] = Set.fromList <$> [
  [ DFmetaSectionHomepage
  , DFmetaSectionDescription
  , DFmetaSectionLicense
  , DFmetaSectionPlatforms
  , DFmetaSectionMaintainers
  ],
  [ DFsubpath

  , DFrevision
  , DFeditedCabalFile

  , DFdoHaddock
  , DFjailbreak
  , DFdoCheck
  ],
  [ DFsrcUrl
  , DFsrcSha256
  , DFsrcRev
  ]
  ]


-- | Whether a 'Derivation's field needs to be a part of Nix derivation.
drvFieldIsNondefault ∷ Derivation → DrvField → Bool
drvFieldIsNondefault drv DFdrvparams                   = False
drvFieldIsNondefault drv DFpname                       = True
drvFieldIsNondefault drv DFversion                     = True
drvFieldIsNondefault drv DFsrcUrl                      = True
drvFieldIsNondefault drv DFsrcSha256                   = True
drvFieldIsNondefault drv DFsrcRev                      = True
drvFieldIsNondefault drv DFsubpath                     = drv^.subpath /= "."

drvFieldIsNondefault drv DFrevision                    = drv^.revision > 0
drvFieldIsNondefault drv DFeditedCabalFile             = not (null (drv^.editedCabalFile)) && drv^.revision > 0

drvFieldIsNondefault drv DFconfigureFlags              = not (Set.null (drv^.configureFlags)) ∧ not (null (unFlagAssignment (drv^.cabalFlags)))

drvFieldIsNondefault drv DFisLibrary                   = not (drv^.isLibrary) || drv^.isExecutable
drvFieldIsNondefault drv DFisExecutable                = not (drv^.isLibrary) || drv^.isExecutable
drvFieldIsNondefault drv DFenableSeparateDataOutput    = drv^.enableSeparateDataOutput

drvFieldIsNondefault drv DFsetupHaskellDepends         = drv^.setupDepends.haskell      /= mempty
drvFieldIsNondefault drv DFlibraryHaskellDepends       = drv^.libraryDepends.haskell    /= mempty
drvFieldIsNondefault drv DFexecutableHaskellDepends    = drv^.executableDepends.haskell /= mempty
drvFieldIsNondefault drv DFtestHaskellDepends          = drv^.testDepends.haskell       /= mempty
drvFieldIsNondefault drv DFbenchmarkHaskellDepends     = drv^.benchmarkDepends.haskell  /= mempty

drvFieldIsNondefault drv DFsetupSystemDepends          = drv^.setupDepends.system      /= mempty
drvFieldIsNondefault drv DFlibrarySystemDepends        = drv^.libraryDepends.system    /= mempty
drvFieldIsNondefault drv DFexecutableSystemDepends     = drv^.executableDepends.system /= mempty
drvFieldIsNondefault drv DFtestSystemDepends           = drv^.testDepends.system       /= mempty
drvFieldIsNondefault drv DFbenchmarkSystemDepends      = drv^.benchmarkDepends.system  /= mempty

drvFieldIsNondefault drv DFsetupPkgconfigDepends       = drv^.setupDepends.pkgconfig      /= mempty
drvFieldIsNondefault drv DFlibraryPkgconfigDepends     = drv^.libraryDepends.pkgconfig    /= mempty
drvFieldIsNondefault drv DFexecutablePkgconfigDepends  = drv^.executableDepends.pkgconfig /= mempty
drvFieldIsNondefault drv DFtestPkgconfigDepends        = drv^.testDepends.pkgconfig       /= mempty
drvFieldIsNondefault drv DFbenchmarkPkgconfigDepends   = drv^.benchmarkDepends.pkgconfig  /= mempty

drvFieldIsNondefault drv DFsetupToolDepends            = drv^.setupDepends.tool      /= mempty
drvFieldIsNondefault drv DFlibraryToolDepends          = drv^.libraryDepends.tool    /= mempty
drvFieldIsNondefault drv DFexecutableToolDepends       = drv^.executableDepends.tool /= mempty
drvFieldIsNondefault drv DFtestToolDepends             = drv^.testDepends.tool       /= mempty
drvFieldIsNondefault drv DFbenchmarkToolDepends        = drv^.benchmarkDepends.tool  /= mempty

drvFieldIsNondefault drv DFenableLibraryProfiling      = drv^.enableLibraryProfiling
drvFieldIsNondefault drv DFenableExecutableProfiling   = drv^.enableExecutableProfiling
drvFieldIsNondefault drv DFenableSplitObjs             = not (drv^.enableSplitObjs)

drvFieldIsNondefault drv DFdoHaddock                   = not (drv^.runHaddock)
drvFieldIsNondefault drv DFjailbreak                   = drv^.jailbreak
drvFieldIsNondefault drv DFdoCheck                     = not (drv^.doCheck)

drvFieldIsNondefault drv DFtestTarget                  = not (null (drv^.testTarget))
drvFieldIsNondefault drv DFhyperlinkSource             = not (drv^.hyperlinkSource)
drvFieldIsNondefault drv DFphaseOverrides              = not (null (drv^.phaseOverrides))

drvFieldIsNondefault drv DFmetaSectionHomepage         = not (null (drv^.metaSection.homepage))
drvFieldIsNondefault drv DFmetaSectionDescription      = not (null (drv^.metaSection.description))
drvFieldIsNondefault drv DFmetaSectionLicense          = True
drvFieldIsNondefault drv DFmetaSectionPlatforms        = drv^.metaSection.platforms /= allKnownPlatforms
drvFieldIsNondefault drv DFmetaSectionMaintainers      = False -- not (Set.null (drv^.metaSection.maintainers))


-- | Field-wise pretty-printing of 'Derivation'.
pprintField ∷ Derivation → DrvField → Doc
pprintField drv DFdrvparams                   = funargs (map text ("mkDerivation" : Set.toAscList inputs))
  where inputs ∷ Set.Set String
        inputs = Set.unions [ Set.map (view (localName . ident)) (drv^.extraFunctionArgs)
                            , setOf (dependencies . each . folded . localName . ident) drv
                            , Set.fromList ["fetch" ++ derivKind (drv^.src) | derivKind (drv^.src) /= "" && not (isHackagePackage drv)]
                            ]
pprintField drv DFpname                       = doubleQuotes $ disp $ packageName    $ drv^.pkgid
pprintField drv DFversion                     = doubleQuotes $ disp $ packageVersion $ drv^.pkgid
-- XXX: the src attribute handling is butchered, beware
pprintField drv DFsrcUrl                      = pPrint $ derivUrl $ drv^.src
pprintField drv DFsrcSha256                   = pPrint $ derivHash $ drv^.src
pprintField drv DFsrcRev                      = pPrint $ derivRevision $ drv^.src
pprintField drv DFsubpath                     = postUnpack
  where postUnpack = string $ "sourceRoot+=/" ++ (drv^.subpath) ++ "; echo source root reset to $sourceRoot"
pprintField drv DFrevision                    = doubleQuotes $ int $ drv^.revision
pprintField drv DFeditedCabalFile             = string $ drv^.editedCabalFile
pprintField drv DFconfigureFlags              = listattr "configureFlags" Nix.empty $ map (show . show) renderedFlags
  where renderedFlags = [ text "-f" <> (if enable then Nix.empty else char '-') <> text (unFlagName f)
#if MIN_VERSION_base(4,11,0)
                        | (f, enable) <- unFlagAssignment $ drv^.cabalFlags ]
#else
                        | (f, enable) <- _cabalFlags ]
#endif
                        ++ map text (toAscList $ drv^.configureFlags)
pprintField drv DFisLibrary                   = bool $ drv^.isLibrary
pprintField drv DFisExecutable                = bool $ drv^.isExecutable
pprintField drv DFenableSeparateDataOutput    = bool $ drv^.enableSeparateDataOutput

pprintField drv DFsetupHaskellDepends         = pprintSet $ setOf (haskell.folded.localName.ident) (drv^.setupDepends)
pprintField drv DFlibraryHaskellDepends       = pprintSet $ setOf (haskell.folded.localName.ident) (drv^.libraryDepends)
pprintField drv DFexecutableHaskellDepends    = pprintSet $ setOf (haskell.folded.localName.ident) (drv^.executableDepends)
pprintField drv DFtestHaskellDepends          = pprintSet $ setOf (haskell.folded.localName.ident) (drv^.testDepends)
pprintField drv DFbenchmarkHaskellDepends     = pprintSet $ setOf (haskell.folded.localName.ident) (drv^.benchmarkDepends)

pprintField drv DFsetupSystemDepends          = pprintSet $ setOf (system.folded.localName.ident) (drv^.setupDepends)
pprintField drv DFlibrarySystemDepends        = pprintSet $ setOf (system.folded.localName.ident) (drv^.libraryDepends)
pprintField drv DFexecutableSystemDepends     = pprintSet $ setOf (system.folded.localName.ident) (drv^.executableDepends)
pprintField drv DFtestSystemDepends           = pprintSet $ setOf (system.folded.localName.ident) (drv^.testDepends)
pprintField drv DFbenchmarkSystemDepends      = pprintSet $ setOf (system.folded.localName.ident) (drv^.benchmarkDepends)

pprintField drv DFsetupPkgconfigDepends       = pprintSet $ setOf (pkgconfig.folded.localName.ident) (drv^.setupDepends)
pprintField drv DFlibraryPkgconfigDepends     = pprintSet $ setOf (pkgconfig.folded.localName.ident) (drv^.libraryDepends)
pprintField drv DFexecutablePkgconfigDepends  = pprintSet $ setOf (pkgconfig.folded.localName.ident) (drv^.executableDepends)
pprintField drv DFtestPkgconfigDepends        = pprintSet $ setOf (pkgconfig.folded.localName.ident) (drv^.testDepends)
pprintField drv DFbenchmarkPkgconfigDepends   = pprintSet $ setOf (pkgconfig.folded.localName.ident) (drv^.benchmarkDepends)

pprintField drv DFsetupToolDepends            = pprintSet $ setOf (tool.folded.localName.ident) (drv^.setupDepends)
pprintField drv DFlibraryToolDepends          = pprintSet $ setOf (tool.folded.localName.ident) (drv^.libraryDepends)
pprintField drv DFexecutableToolDepends       = pprintSet $ setOf (tool.folded.localName.ident) (drv^.executableDepends)
pprintField drv DFtestToolDepends             = pprintSet $ setOf (tool.folded.localName.ident) (drv^.testDepends)
pprintField drv DFbenchmarkToolDepends        = pprintSet $ setOf (tool.folded.localName.ident) (drv^.benchmarkDepends)

pprintField drv DFenableLibraryProfiling      = bool $ drv^.enableLibraryProfiling
pprintField drv DFenableExecutableProfiling   = bool $ drv^.enableExecutableProfiling
pprintField drv DFenableSplitObjs             = bool $ drv^.enableSplitObjs
pprintField drv DFdoHaddock                   = bool $ drv^.runHaddock
pprintField drv DFjailbreak                   = bool $ drv^.jailbreak
pprintField drv DFdoCheck                     = bool $ drv^.doCheck
pprintField drv DFtestTarget                  = string $ drv^.testTarget
pprintField drv DFhyperlinkSource             = bool $ drv^.hyperlinkSource
pprintField drv DFphaseOverrides              = vcat $ (map text . lines) (drv^.phaseOverrides)

pprintField drv DFmetaSectionHomepage         = pPrint $ drv^.metaSection.homepage
pprintField drv DFmetaSectionDescription      = pPrint $ drv^.metaSection.description
pprintField drv DFmetaSectionLicense          = pPrint $ drv^.metaSection.license
pprintField drv DFmetaSectionPlatforms        = renderPlatforms "platforms" $ drv^.metaSection.platforms
  where
    -- Stolen from distribution-nixpkgs/src/Distribution/Nixpkgs/Meta.hs
    renderPlatforms ∷ String → Set.Set Platform → Doc
    renderPlatforms field ps
      | Set.null ps = sep [ text field <+> equals <+> text "stdenv.lib.platforms.none" Nix.<> semi ]
      | otherwise   = sep [ text field <+> equals <+> lbrack
                          , nest 2 $ fsep $ map text (toAscList (Set.map fromCabalPlatform ps))
                          , rbrack Nix.<> semi
                          ]
    -- Stolen from distribution-nixpkgs/src/Distribution/Nixpkgs/Meta.hs
    fromCabalPlatform ∷ Platform → String
    fromCabalPlatform (Platform I386 Linux)   = "\"i686-linux\""
    fromCabalPlatform (Platform X86_64 Linux) = "\"x86_64-linux\""
    fromCabalPlatform (Platform X86_64 OSX)   = "\"x86_64-darwin\""
    fromCabalPlatform p                       = error ("fromCabalPlatform: invalid Nix platform" ++ show p)
pprintField drv DFmetaSectionMaintainers      = (⊥)

pprintSet ∷ Set.Set String → Doc
pprintSet xs = fsep $ map text $ toAscList xs

bool :: Bool -> Doc
bool True  = text "true"
bool False = text "false"
