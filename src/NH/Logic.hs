{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module NH.Logic
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
import           Data.Text                           (Text, pack, unpack, toLower, toUpper, drop, take, length, isSuffixOf, isPrefixOf)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as Sys
import qualified GHC.Types                        as Type
import           Prelude                      hiding (read, take, drop, length)
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
import qualified Text.Read                        as R
import qualified Debug.Trace                      as DBG

import           NH.Types
import           NH.Config
import           NH.Derivation                    as Drv
import qualified NH.FS                            as FS
import           NH.FS                        hiding (open, init)
import           NH.Misc
import           NH.MRecord
import           NH.Nix
import           NH.PKGDB



attrRepoName ∷ Attr → Meta → RepoName
attrRepoName (Attr name) Meta{..} = RepoName name



overShadowed ∷ OverPackage → Maybe Attr
overShadowed OverPackage{opOver=Overrides{ovSrc=Just Hackage{..}}, ..} =
  attrShadowedAt opAttr haRelease opNixpkgs
overShadowed _ = Nothing

overStatus ∷ OverPackage → Status
overStatus op@OverPackage{opOver=opOver@Overrides{..}, ..} = do
  case (ovSrc, opUpstream) of
    (Just Hackage{..}, _) →
      if isJust $ overShadowed op
      then StShadowed
      else StHackaged
    (Just Github{..}, Just Upstream{..}) →
      case (opOver ≡ mempty, upUser ≡ ghUser) of
        (True, _)  → StConfig
        (_, True)  → StUpstreamed
        (_, False) → StUnmerged
    (Just Github{..}, Nothing) →
      error $ printf "Malformed package '%s': source overridden, but no upstream associated." (unpack $ fromAttr opAttr)
    (Nothing, _) →
      if opOver ≡ mempty
      then StConfig
      else StDefault

statusExplanation ∷ Status → Text
statusExplanation StShadowed   = "Needs bump to a versioned attribute"
statusExplanation StHackaged   = "On Hackage, awaiting for import"
statusExplanation StUpstreamed = "Upstreamed, awaiting a Hackage release"
statusExplanation StUnmerged   = "Unmerged.  PR: $(url upstream-pull-request ${attr})"
statusExplanation StConfig     = "Non-source change"
