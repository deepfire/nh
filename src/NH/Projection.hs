{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.Projection
  ( piecewiseDerivation
  , fieldActive
  , drvFieldNixName
  , drvPassthroughFields
  , drvNonPassthroughFields
  )
where

import           Control.Lens                 hiding (argument)
import           Data.Foldable                       (find)
import qualified Data.List                        as L
import qualified Data.Set                         as Set
import           Data.Set.Lens                       (setOf)
import qualified Data.Text                        as T
import           Data.Text                           (pack, unpack)
import           Prelude.Unicode

-- Cabal
import           Distribution.PackageDescription     (mkFlagName, FlagAssignment, FlagName, unFlagName, unFlagAssignment, mkFlagAssignment)
import           Distribution.Package                (packageId, packageName, packageVersion)
import           Distribution.System

-- cabal2nix
import           Distribution.Nixpkgs.Fetch
import           Distribution.Nixpkgs.Haskell
-- distribution-nixpkgs
import           Distribution.Nixpkgs.Meta

-- language-nix
import           Language.Nix
import           Language.Nix.PrettyPrinting  hiding ((<>), empty)
import qualified Language.Nix.PrettyPrinting      as Nix

-- pretty
import           Text.PrettyPrint.HughesPJClass      (Doc, Pretty(..), text, vcat, hcat, semi)



import           NH.Misc
import           NH.Types


--
-- * Field-wise 'Derivation'
--
nestedPrefixes ∷ [(T.Text, Field)]
nestedPrefixes =
  [("src",         "src")
  ,("metaSection", "meta")
  ]

drvFieldNestPrefix ∷ DerivationField → Maybe (T.Text, Field)
drvFieldNestPrefix df =
  let s = pack $ drop 2 $ show df
  in flip find nestedPrefixes (flip T.isPrefixOf s ∘ fst)

drvFieldNixName ∷ DerivationField → T.Text
drvFieldNixName df =
  let s         = pack $ drop 2 $ show df
      detitle x = T.toLower (T.take 1 x) <> T.drop 1 x
  in case drvFieldNestPrefix df of
       Nothing       → s
       Just (pfx, _) → detitle $ T.drop (T.length pfx) s

isHackagePackage ∷ Derivation → Bool
isHackagePackage drv = "mirror://hackage/" `L.isPrefixOf` derivUrl (drv^.src)

piecewiseDerivation ∷ Derivation → [(DerivationField, Doc)]
piecewiseDerivation drv =
  [ (,) field (pprintField drv field)
  | field ← every
  , fieldActive drv field ]

drvNonPassthroughFields ∷ Set.Set DerivationField
drvNonPassthroughFields = Set.fromList
  [ DFinputs
  , DFpname
  , DFsrcUrl
  , DFsrcSha256
  , DFsrcRev
  , DFsubpath
  , DFrevision
  , DFeditedCabalFile
  , DFdoHaddock
  , DFjailbreak
  , DFdoCheck
  ]

drvPassthroughFields ∷ Set.Set DerivationField
drvPassthroughFields = Set.fromList every `Set.difference` drvNonPassthroughFields


-- | Whether a 'Derivation's field needs to be a part of Nix derivation.
fieldActive ∷ Derivation → DerivationField → Bool
fieldActive drv DFinputs                      = True
fieldActive drv DFpname                       = True
fieldActive drv DFversion                     = True
fieldActive drv DFsrcUrl                      = True
fieldActive drv DFsrcSha256                   = True
fieldActive drv DFsrcRev                      = True
fieldActive drv DFsubpath                     = drv^.subpath /= "."

fieldActive drv DFrevision                    = drv^.revision > 0
fieldActive drv DFeditedCabalFile             = not (null (drv^.editedCabalFile)) && drv^.revision > 0

fieldActive drv DFconfigureFlags              = not (Set.null (drv^.configureFlags)) ∧ not (null (unFlagAssignment (drv^.cabalFlags)))

fieldActive drv DFisLibrary                   = not (drv^.isLibrary) || drv^.isExecutable
fieldActive drv DFisExecutable                = not (drv^.isLibrary) || drv^.isExecutable
fieldActive drv DFenableSeparateDataOutput    = drv^.enableSeparateDataOutput

fieldActive drv DFsetupHaskellDepends         = drv^.setupDepends.haskell      /= mempty
fieldActive drv DFlibraryHaskellDepends       = drv^.libraryDepends.haskell    /= mempty
fieldActive drv DFexecutableHaskellDepends    = drv^.executableDepends.haskell /= mempty
fieldActive drv DFtestHaskellDepends          = drv^.testDepends.haskell       /= mempty
fieldActive drv DFbenchmarkHaskellDepends     = drv^.benchmarkDepends.haskell  /= mempty

fieldActive drv DFsetupSystemDepends          = drv^.setupDepends.system      /= mempty
fieldActive drv DFlibrarySystemDepends        = drv^.libraryDepends.system    /= mempty
fieldActive drv DFexecutableSystemDepends     = drv^.executableDepends.system /= mempty
fieldActive drv DFtestSystemDepends           = drv^.testDepends.system       /= mempty
fieldActive drv DFbenchmarkSystemDepends      = drv^.benchmarkDepends.system  /= mempty

fieldActive drv DFsetupPkgconfigDepends       = drv^.setupDepends.pkgconfig      /= mempty
fieldActive drv DFlibraryPkgconfigDepends     = drv^.libraryDepends.pkgconfig    /= mempty
fieldActive drv DFexecutablePkgconfigDepends  = drv^.executableDepends.pkgconfig /= mempty
fieldActive drv DFtestPkgconfigDepends        = drv^.testDepends.pkgconfig       /= mempty
fieldActive drv DFbenchmarkPkgconfigDepends   = drv^.benchmarkDepends.pkgconfig  /= mempty

fieldActive drv DFsetupToolDepends            = drv^.setupDepends.tool      /= mempty
fieldActive drv DFlibraryToolDepends          = drv^.libraryDepends.tool    /= mempty
fieldActive drv DFexecutableToolDepends       = drv^.executableDepends.tool /= mempty
fieldActive drv DFtestToolDepends             = drv^.testDepends.tool       /= mempty
fieldActive drv DFbenchmarkToolDepends        = drv^.benchmarkDepends.tool  /= mempty

fieldActive drv DFenableLibraryProfiling      = drv^.enableLibraryProfiling
fieldActive drv DFenableExecutableProfiling   = drv^.enableExecutableProfiling
fieldActive drv DFenableSplitObjs             = not (drv^.enableSplitObjs)
                                              
fieldActive drv DFdoHaddock                   = not (drv^.runHaddock)
fieldActive drv DFjailbreak                   = drv^.jailbreak
fieldActive drv DFdoCheck                     = not (drv^.doCheck)
                                              
fieldActive drv DFtestTarget                  = not (null (drv^.testTarget))
fieldActive drv DFhyperlinkSource             = not (drv^.hyperlinkSource)
fieldActive drv DFphaseOverrides              = not (null (drv^.phaseOverrides))

fieldActive drv DFmetaSectionHomepage         = not (null (drv^.metaSection.homepage))
fieldActive drv DFmetaSectionDescription      = not (null (drv^.metaSection.description))
fieldActive drv DFmetaSectionLicense          = True
fieldActive drv DFmetaSectionPlatforms        = drv^.metaSection.platforms /= allKnownPlatforms
fieldActive drv DFmetaSectionMaintainers      = False -- not (Set.null (drv^.metaSection.maintainers))


-- | Field-wise pretty-printing of 'Derivation'.
pprintField ∷ Derivation → DerivationField → Doc
pprintField drv DFinputs                      = funargs (map text ("mkDerivation" : Set.toAscList inputs))
  where inputs ∷ Set.Set String
        inputs = Set.unions [ Set.map (view (localName . ident)) (drv^.extraFunctionArgs)
                            , setOf (dependencies . each . folded . localName . ident) drv
                            , Set.fromList ["fetch" ++ derivKind (drv^.src) | derivKind (drv^.src) /= "" && not (isHackagePackage drv)]
                            ]
pprintField drv DFpname                       = doubleQuotes $ disp $ packageName $ drv^.pkgid
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
