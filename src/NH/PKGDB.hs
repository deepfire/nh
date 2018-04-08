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
module NH.PKGDB
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



cnDrvMeta,        cnGRepo, cnGithub, cnHackage, cnMeta, cnOver,      cnPkg ∷ CName
allCNames@[cnDrvMeta, cnGRepo, cnGithub, cnHackage, cnMeta, cnOver, cnPkg] = CName <$>
  ["DrvMeta", "GithubRepo", "Github", "Hackage", "Meta", "Overrides", "Package"]

deriving instance MapKey Field

instance RecordCtx Ctx where
  errCtxDesc  (_,  en) cn (Field fi) = T.pack $
    printf "%s:%s:%s" (unpack en) (unpack fi) (unpack $ fromCName cn)
  listFields  (db, en) cn            = (Field <$>) <$> listCtx cn (CtxName en) db
  dropField   (db, en) cn (Field fi) =
    rm cn (CtxName en) (Field fi) db
  nameMap = const
    [("nixHash", "hash")]

instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ Record Ctx a where
  prefixChars _ _ = 2; consCtx _ _ n _ = CName n
instance Record Ctx Src        where
  prefixChars _ _ = 2
  consCtx _ _ n _ = CName n
  saveChoice (db, en)  Github{..} = write' db  cnOver (CtxName en) "src" (Just "github")
  saveChoice (db, en) Hackage{..} = write' db  cnOver (CtxName en) "src" (Just "hackage")
  restoreChoice ctx _ = do
    msrc ∷ Maybe Text ← restoreField ctx cnOver "src"
    pure $ case msrc of
      Just "hackage" → 0 --     Z ∘ K $ ()
      Just "github"  → 1 -- S ∘ Z ∘ K $ ()
      Nothing → fieldError ctx cnOver "src" "'src' field ⊥: cannot choose between alternatives"
instance ReadField  Ctx DrvMeta
instance ReadField  Ctx GithubRepo
instance ReadField  Ctx Meta
instance ReadField  Ctx Overrides
instance ReadField  Ctx Package
instance ReadField  Ctx Src
instance WriteField Ctx DrvMeta
instance WriteField Ctx GithubRepo
instance WriteField Ctx Meta
instance WriteField Ctx Overrides
instance WriteField Ctx Package
instance WriteField Ctx Src

instance ReadField  Ctx (ElapsedSince UnixEpoch) where readField  d c f =  readField d c f <&> (P.read ∘ unpack <$>)
instance WriteField Ctx (ElapsedSince UnixEpoch) where writeField d c f = writeField d c f ∘ pack ∘ show

instance WriteField Ctx DFValue where writeField d c f = writeField d c f ∘ showDocOneLine ∘ dfDoc


-- * Basis for DB access:  writes are fake, reads are real

writeText ∷ (PKGDB, EName) → CName → Field → Text → IO ()
writeText (db, en) cn fi x = write' db cn (CtxName en) fi $ Just x

instance WriteField Ctx Text where
  writeField = writeText
instance  ReadField Ctx Text where
  readField (db,en) cn = read' db cn (CtxName en)



writeTextly ∷ Coercible a Text ⇒ Ctx → CName → Field → a → IO ()
writeTextly ctx cn fi = writeText ctx cn fi ∘ coerce

readTextly  ∷ Coercible a Text ⇒ Ctx → CName → Field → IO (Maybe a)
readTextly (db,en) cn = coerce <$> read' db cn (CtxName en)

instance {-# OVERLAPPABLE #-} Coercible a Text ⇒ WriteField Ctx a where
  writeField = writeTextly

instance {-# OVERLAPPABLE #-} Coercible a Text ⇒  ReadField Ctx a where
  readField  = readTextly



instance (Coercible a Text) ⇒ WriteField Ctx [a] where
  writeField ctx cn fi [] = dropField ctx cn fi
  writeField ctx cn fi xs = writeText ctx cn fi $ T.intercalate " " $ coerce <$> xs

instance (Coercible a Text) ⇒  ReadField Ctx [a] where
  readField (db, en) cn fi = read' db cn (CtxName en) fi <&>
    (defineMaybe [] ∘ (<&> (<&> coerce) ∘ readNames))



instance CFlag a ⇒ WriteField Ctx (Flag a) where
  writeField ctx cn fi x = storeField ctx cn fi $
    if x ≡ enabled
    then Just ("true" ∷ Text)
    else Nothing

instance CFlag a ⇒  ReadField Ctx (Flag a) where
  readField (db, en) cn fi = read' db cn (CtxName en) fi
    <&> \case
      Nothing → Just disabled
      Just _  → Just enabled



instance (MapKey k, Ord k, ReadField Ctx v) ⇒ RestoreField Ctx (Map k v) where
  restoreField ctx@(db, en) cn (Field f) = do
    -- XXX: this is an abstraction leak:
    keys ← (drop (length f + 1) <$>) ∘ filter (isPrefixOf (f <> ".")) <$> listCtx cn (CtxName en) db
    Map.fromList <$> (forM keys
                       (\k→ (fromKeyName k,) <$> restoreField ctx cn (Field $  f <> "." <> k)))
instance (MapKey k, Ord k, WriteField Ctx v) ⇒ StoreField Ctx (Map k v) where
  storeField ctx@(db, en) cn fi@(Field f) xs = do
    all ← filter (isPrefixOf (f <> ".")) <$> listCtx cn (CtxName en) db
    forM_ all $ \old → do
      removeFileIfExists old
    forM_ (Map.toList xs) $ \(k, v) → do
      writeField ctx cn (Field $ f<>"."<>toKeyName k) v

-- * XXX: This is an instance tailored to a single field of Package:
--   , pkDrvFields        ∷ Map DrvField DFValue -- ^ Non-overridable fields only
instance {-# OVERLAPS #-} RestoreField Ctx (Map DrvField DFValue) where
  restoreField (db, en) cn (Field fi) = Map.fromList ∘ catMaybes <$> mapM (readField db en) (Set.toList Drv.drvFieldsPkgSet)
    where
      readField ∷ PKGDB → EName → DrvField → IO (Maybe (DrvField, DFValue))
      -- XXX: this is an abstraction leak:
      readField db en df = ((df,) ∘ DFValue df ∘ parseFieldTyped (Drv.drvFieldType df) <$>) <$> read cn (CtxName en) (Field $ fi <> "." <> (Drv.drvFieldNixName df)) db
      parseAttributes ∷ Text → [Attr]
      parseAttributes raw = Attr <$> L.delete "" (T.splitOn " " raw)
      parseFieldTyped   (NTList NTVar) raw = sep [ fsep $ text ∘ unpack ∘ fromAttr <$> parseAttributes raw ]
      parseFieldTyped   (NTList NTStr) raw = sep [ lbrack
                                                 , fsep $ text <$> readSequence raw
                                                 , rbrack ]
      parseFieldTyped t@(NTList _)     _   = error $ printf "Unsupported list type: %s" (show t)
      parseFieldTyped _                raw = text $ unpack raw



init ∷ Text → IO PKGDB
init = FS.init allCNames

open ∷ Text → IO (Maybe PKGDB)
open = FS.open allCNames

read' ∷ PKGDB → CName → CtxName → Field → IO (Maybe Text)
read' db cn en fi = read cn en fi db

write' ∷ PKGDB → CName → CtxName → Field → Maybe Text → IO ()
write' db cn en fi mval = write cn en fi mval db



wtest = do
  Just db <- open "/home/deepfire/configuration-ghc84x/"
  store (db, "lol" ∷ Text) $ Meta (Just $ RepoName "lol") DisableOverride Nothing [] (Just $ Attr "lol") ToLocal mempty
rmeta = do
  Just db <- open "/home/deepfire/configuration-ghc84x/"
  recover (db, "hspec" ∷ Text) :: IO Meta
rsrc = do
  Just db <- open "/home/deepfire/configuration-ghc84x/"
  recover (db, "hspec" ∷ Text) :: IO Src
rover = do
  Just db <- open "/home/deepfire/configuration-ghc84x/"
  recover (db, "hspec" ∷ Text) :: IO Overrides



attrRepoName ∷ Attr → Meta → RepoName
attrRepoName (Attr name) Meta{..} = RepoName name



readFulldefnNames ∷ PKGDB → IO (Set Attr)
readFulldefnNames db = (Attr ∘ unCtxName <$>) <$> list cnPkg db

readPkNames ∷ PKGDB → IO (Set Attr)
readPkNames db = (Attr ∘ unCtxName <$>) <$> list cnOver db

readPks ∷ PKGDB → IO (Map Attr Package)
readPks db = readPkNames db
  <&> Set.toList >>= mapM (\pkAttr→ do
                             (pkAttr,) <$> (recover (db, fromAttr pkAttr)))
  <&> Map.fromList



pkShadowed ∷ Package → Nixpkgs → Bool
pkShadowed pk@Package{pkAttr, pkOver=pkOver@Overrides{ovSrc=Just Hackage{..}}} nixpkgs =
  attrShadowedAt pkAttr haRelease nixpkgs
pkShadowed _ _ = False

pkStatus ∷ Package → Nixpkgs → Status
pkStatus pk@Package{pkOver=over@Overrides{..}} nixpkgs = do
  case ovSrc of
    Just Hackage{..} → if pkShadowed pk nixpkgs
                       then StShadowed
                       else StHackaged
    Just Github{..}  → case (over ≡ mempty, Map.member DFmetaSectionLicense ovDrvFields) of
                         (True, _) → StConfig
                         (_, True) → StFulldefn
                         (_, _)    → StDefault
    Nothing          → if over ≡ mempty
                       then StConfig
                       else StDefault
