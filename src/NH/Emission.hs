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
                                                     , renderStyle, fsep, text, sep, fsep, lbrack, rbrack, lbrace, rbrace, empty
                                                     , vcat, nest, doubleQuotes, (<+>), semi)

import           NH.Types
import           NH.Config
import           NH.FS
import           NH.Misc
import           NH.MRecord
import           NH.Nix



instance Pretty Package where
  pPrint fd@Package{pkAttr
                   ,pkRepo=Just GithubRepo{..}
                   ,pkMeta=pkMeta@Meta{..}
                   ,pkOver=Overrides{ovSrc=Just Github{..},ovDrvFields}
                   ,pkDrvMeta=DrvMeta{..}
                   ,pkDrvFields} =
    vcat
    [ text "with pkgs;" <+> text "with self;" <+> text "mkDerivation" <+> lbrace
    , text " "
    , nest 2 $ vcat
      [ attr "pname"   ∘ string ∘ unpack $ fromAttr pkAttr
      , attr "version" $ pkgDrvFieldMand fd DFversion
      , text "src" <+> equals <+> text "fetchFromGitHub" <+> lbrace
           , nest 2 $ vcat
             [ attr "owner"  ∘ string ∘ unpack $ fromUser       grUpstream
             , attr "repo"   ∘ string ∘ unpack $ fromRepoName   grRepoName
             , attr "rev"    ∘ string ∘ unpack $ fromRef        ghRev
             , attr "sha256" ∘ string ∘ unpack $ fromNixHash    srNixHash
             ]
      , rbrace <> semi
      , maybeAttr "postUnpack" (meChdir <&> (\cd→ string ("sourceRoot+=/" <> unpack cd <> "; echo source root reset to $sourceRoot")))
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
  pPrint _ = error "Cannot pretty-print a package with no GithubRepo."


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
