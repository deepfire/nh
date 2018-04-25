{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
module NH.Emission
where

import           Control.Exception
import           Control.Lens                        ((<&>))
import           Control.Monad                       (foldM, forM, forM_, join, liftM, when)
import           Data.Coerce                         (Coercible, coerce)
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Function                       ((&))
import           Data.Hourglass                      (Seconds(..))
import           Data.Hourglass.Epoch
import qualified Data.List                        as L
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                         as Set
import           Data.Text                           (Text, pack, unpack, toLower, toUpper, drop, take, length, isSuffixOf)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as Sys
import qualified GHC.Types                        as Type
import           Prelude                      hiding (take, drop, length)
import qualified Prelude                          as P
import           Prelude.Unicode
import qualified System.Directory                 as Sys
import qualified System.IO.Temp                   as Sys
import qualified System.FilePath                  as Sys
import           Text.Printf

import           Data.Proxy
import           GHC.Generics                        (Generic)
import qualified GHC.Generics                     as GHC
import           Generics.SOP                        (Rep, NS(..), NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), FieldName(..), FieldInfo(..), ConstructorInfo(..), SListI
                                                     ,from, to, hcollapse, hcliftA2, hliftA, hcliftA, unI, hsequence, hcpure, hpure)
import qualified Generics.SOP                     as SOP

import           Language.Nix.PrettyPrinting  hiding ((<>), empty, Text)
import           Text.PrettyPrint.HughesPJClass      ( Doc, Pretty(..), Style(..), Mode(..)
                                                     , renderStyle, fsep, text, sep, fsep, lbrack, rbrack, lbrace, rbrace, lparen, rparen, empty
                                                     , vcat, nest, doubleQuotes, semi, (<+>), ($+$), maybeParens)

import qualified Language.Nix.PrettyPrinting      as Nix

import           NH.Types
import           NH.Config
import           NH.FS
import           NH.Logic
import           NH.Misc
import           NH.MRecord
import           NH.Nix



withTarget ∷ Flag Local → Doc → Doc
withTarget ToNixpkgs   body = funargs (text <$> ["pkgs", "haskellLib", "super", "self"]) $+$ "" $+$
  text "with haskellLib;" $+$ "" $+$
  text "self: super:" <+> lbrace $+$
  body $+$
  rbrace
withTarget ToLocal body = funargs (text <$> ["pkgs", "haskellLib"]) $+$ "" $+$
  text "with haskellLib; with self;" <+> lbrace $+$
  body $+$
  rbrace

instance Pretty OverPackage where
  pPrint o@OverPackage{opAttr
                      ,opMeta=opMeta@Meta{..}
                      ,opOver=opOver@Overrides{..}
                      ,opNixpkgs
                      } =
    let attrStr           = unpack ∘ fromAttr
        attrDoc           = text ∘ attrStr
        ovEmpty     = opOver ≡ mempty
        ovJustSrc   = not ovEmpty ∧ opOver { ovSrc=Nothing  } ≡ mempty
        (shadowed, baseAttr) = case (isJust ovSrc, overShadowed o) of
                                 (True,  Just shadow) → (,) True  shadow
                                 _                    → (,) False opAttr
        status      = overStatus o
        generalExpl = statusExplanation status
        srcExpl     = mempty -- XXX "emit_explanation src ${attr} | prefix_lines \"  ## \""
        inputOver   = if ovInputs ≡ mempty then mempty else
                        vcat
                        [ text ".override" <+> lbrace
                        , nest 2 $ vcat $ Map.toList ovInputs <&>
                          \(Attr from, Attr to) → attr (unpack from) (text $ unpack to)
                        , rbrace <> semi]

    in if meDisable ≡ DisableOverride ∨ ovEmpty then mempty else
       if opOver ≡ mempty ∨ (ovJustSrc ∧ not shadowed)
       then (if ovJustSrc then srcExpl else mempty)
            $+$
            attr (attrStr opAttr) ("super." <> attrDoc baseAttr <> inputOver)
       -- else attr (attrStr opAttr) $ (maybeParens (ovInputs ≢ mempty) $ sep $
       else vcat $ -- XXX inputs need parens:
              [ attr (attrStr opAttr) (("overrideCabal super."<>attrDoc baseAttr) <+> "(drv: {")
              , nest 2 ∘ vcat $
                mapFields @Override -- for p in "src doCheck doHaddock jailbreak editedCabalFile revision postPatch ${EXTRA_PROPS_HANDLED_EXTRAED} patches"
                  (\fieldName fieldVal→ vcat ∘ concat $ flip mapOverride fieldVal $
                    \leaf→ if overrideEnabled leaf
                           then emitOverride (Field fieldName) mempty leaf
                           else [])
                opOver
              , "})" <> inputOver
              ]

ovAssign ∷ Field → Doc → Doc
ovAssign  (Field fi) = attr (unpack ∘ fromField $ toField (Proxy @OverPackage) fi)

ovAssign1 ∷ Field → Doc → [Doc]
ovAssign1 f = (:[]) ∘ ovAssign f

class Override a where
  overrideEnabled ∷               a → Bool
  emitOverride    ∷ Field → Doc → a → [Doc]

instance                       Override Src where
  overrideEnabled _              = True
  emitOverride fi p Github{..} = ovAssign1 fi $ emitBlock' (p <+> "fetchFromGithub")
    [ attr  "owner"   ∘ string ∘ unpack $ fromUser       ghUser
    , attr  "repo"    ∘ string ∘ unpack $ fromRepoName   ghRepoName
    , attr  "rev"     ∘ string ∘ unpack $ fromRef        ghRev
    , attr  "sha256"  ∘ string ∘ unpack $ fromNixHash    srNixHash
    ]
  emitOverride _ p Hackage{..}   =
    [ (attr "version" ∘ string ∘ unpack $ fromRelease    haRelease)
    , (attr "sha256"  ∘ string ∘ unpack $ fromNixHash    srNixHash)
    ]
    
instance          Override a ⇒ Override (Maybe a) where
  overrideEnabled                  = isJust
  emitOverride _ _ Nothing         = []
  emitOverride f p (Just x)        = emitOverride f p x
instance   CFlag (a ∷ Flags) ⇒ Override (Flag a)  where
  overrideEnabled                  = toBool
  emitOverride f _ (toBool → True) = ovAssign1 f "true"
  emitOverride _ _ _               = []
instance                       Override DFValue   where
  overrideEnabled                  = (≢ mempty) ∘ dfDoc
  emitOverride f _ x               = ovAssign1 f $ dfDoc x
instance                       Override [Patch]       where
  overrideEnabled                  = (≢ mempty)
  emitOverride f p ps              = ovAssign1 f $ 
    emitList' (p <+> "(drv.patches or []) ++") $
    [ lparen <> emitBlock' "pkgs.fetchpatch"
      [ attr "url"    $ text $ unpack paUrl
      , attr "sha256" $ text $ unpack paSha256
      ] <> rparen
    | Patch{..} ← ps ]
instance (Eq k, Eq v, Ord k) ⇒ Override (Map k v) where
  overrideEnabled                  = (≢ mempty)
  emitOverride "ovDrvFields"   _ m = []
  emitOverride "ovInputs"      p m = []

class MapOverride a where
  type OverElem a ∷ Type.Type
  mapOverride     ∷ (OverElem a → b) → a → [b]

instance {-# OVERLAPPABLE #-} MapOverride a where
  type OverElem a = a
  mapOverride f a = [f a]

emitNest ∷ Doc → Doc → Doc → [Doc] → Doc
emitNest l r pre body = foldl ($+$) mempty
                        [ pre <+> l
                        , nest 2 $ vcat $ body
                        , r ]

emitBlock', emitList' ∷ Doc → [Doc] → Doc
emitBlock' = emitNest lbrace rbrace
emitList'  = emitNest lbrack rbrack

emitBlock, emitList ∷ [Doc] → Doc
emitBlock  = emitBlock' mempty
emitList   = emitList'  mempty



instance Pretty Package where
  pPrint fd@Package{pkAttr
                   ,pkUpstream=Upstream{..}
                   ,pkMeta=pkMeta@Meta{..}
                   ,pkOver=Overrides{ovSrc=movSrc@(Just Github{..}),ovDrvFields}
                   ,pkDrvMeta=DrvMeta{..}
                   ,pkDrvFields} =
    vcat
    [ text "with pkgs;" <+> text "with self;" <+> text "mkDerivation" <+> lbrace
    , text " "
    , nest 2 $ vcat $
      [ attr "pname"   ∘ string ∘ unpack $ fromAttr pkAttr
      , attr "version" $ pkgDrvFieldMand fd DFversion ]
      <> emitOverride "" ("src" <+> equals) (fromJust movSrc) <>
      [ maybeAttr "postUnpack" (meChdir <&> (\cd→ string ("sourceRoot+=/" <> unpack cd <> "; echo source root reset to $sourceRoot")))
      , maybeAttr "configureFlags"               $ pkgDrvField fd DFconfigureFlags
      , maybeAttr "isLibrary"                    $ pkgDrvField fd DFisLibrary
      , maybeAttr "isExecutable"                 $ pkgDrvField fd DFisExecutable
      , maybeAttr "enableSeparateDataOutput"     $ pkgDrvField fd DFenableSeparateDataOutput

      , maybeAttr' "setupHaskellDepends"         $ pkgDrvField fd DFsetupHaskellDepends
      , maybeAttr' "libraryHaskellDepends"       $ pkgDrvField fd DFlibraryHaskellDepends
      , maybeAttr' "executableHaskellDepends"    $ pkgDrvField fd DFexecutableHaskellDepends
      , maybeAttr' "testHaskellDepends"          $ pkgDrvField fd DFtestHaskellDepends
      , maybeAttr' "benchmarkHaskellDepends"     $ pkgDrvField fd DFbenchmarkHaskellDepends

      , maybeAttr' "setupSystemDepends"          $ pkgDrvField fd DFsetupSystemDepends
      , maybeAttr' "librarySystemDepends"        $ pkgDrvField fd DFlibrarySystemDepends
      , maybeAttr' "executableSystemDepends"     $ pkgDrvField fd DFexecutableSystemDepends
      , maybeAttr' "testSystemDepends"           $ pkgDrvField fd DFtestSystemDepends
      , maybeAttr' "benchmarkSystemDepends"      $ pkgDrvField fd DFbenchmarkSystemDepends

      , maybeAttr' "setupPkgconfigDepends"       $ pkgDrvField fd DFsetupPkgconfigDepends
      , maybeAttr' "libraryPkgconfigDepends"     $ pkgDrvField fd DFlibraryPkgconfigDepends
      , maybeAttr' "executablePkgconfigDepends"  $ pkgDrvField fd DFexecutablePkgconfigDepends
      , maybeAttr' "testPkgconfigDepends"        $ pkgDrvField fd DFtestPkgconfigDepends
      , maybeAttr' "benchmarkPkgconfigDepends"   $ pkgDrvField fd DFbenchmarkPkgconfigDepends

      , maybeAttr' "setupToolDepends"            $ pkgDrvField fd DFsetupToolDepends
      , maybeAttr' "libraryToolDepends"          $ pkgDrvField fd DFlibraryToolDepends
      , maybeAttr' "executableToolDepends"       $ pkgDrvField fd DFexecutableToolDepends
      , maybeAttr' "testToolDepends"             $ pkgDrvField fd DFtestToolDepends
      , maybeAttr' "benchmarkToolDepends"        $ pkgDrvField fd DFbenchmarkToolDepends

      , maybeAttr "enableLibraryProfiling"       $ pkgDrvField fd DFenableLibraryProfiling
      , maybeAttr "enableExecutableProfiling"    $ pkgDrvField fd DFenableExecutableProfiling
      , maybeAttr "enableSplitObjs"              $ pkgDrvField fd DFenableSplitObjs
      , maybeAttr "doHaddock"                    $ Nothing -- Over{..}
      , maybeAttr "jailbreak"                    $ Nothing -- Over{..}
      , maybeAttr "doCheck"                      $ Nothing -- Over{..}
      , maybeAttr "testTarget"                   $ pkgDrvField fd DFtestTarget
      , maybeAttr "hyperlinkSource"              $ pkgDrvField fd DFhyperlinkSource
      -- XXX: not really sure how to handle this
      -- , maybeAttr "phaseOverrides"              $ (vcat ∘ (map text . lines) <$> pkgDrvField ed DFphaseOverrides)
      , maybeAttr "homepage"    $ text ∘ unpack <$> dmHomepage
      , maybeAttr "description" $ text ∘ unpack <$> dmDescription
      , attr      "license"     $ text $ unpack dmLicense
      , maybeAttr "platforms"   $ text ∘ unpack <$> dmPlatforms
      , maybeAttr "maintainers" $ text ∘ unpack <$> dmMaintainers
      ]
    , rbrace
    ]
  pPrint _ = error "Cannot pretty-print a package with no Github specified."


--
-- * Aux code for emission
--
pkgDrvField ∷ Package → DrvField → Maybe Doc
pkgDrvField Package{..} fname = dfDoc <$> Map.lookup fname pkDrvFields

pkgDrvFieldOpt ∷ Package → DrvField → Doc
pkgDrvFieldOpt fd fname = pkgDrvField fd fname &
  fromMaybe empty

pkgDrvFieldMand ∷ Package → DrvField → Doc
pkgDrvFieldMand fd fname = pkgDrvField fd fname &
  (errNothing $ printf "Missing definition passfield '%s'." $ show fname)

maybeAttr ∷ Field → Maybe Doc → Doc
maybeAttr _ Nothing = empty
maybeAttr (Field field) (Just doc)  = attr (unpack field) doc

maybeAttr' ∷ Field → Maybe Doc → Doc
maybeAttr' _ Nothing = empty
maybeAttr' (Field field) (Just doc) = vcat
  [ text (unpack field) <+> equals <+> lbrack
  , nest 2 doc
  , rbrack <> semi ]
