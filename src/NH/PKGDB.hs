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
{-# LANGUAGE RecursiveDo #-}
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
import           GHC.Stack
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



cnDrvMeta,            cnUpstream, cnGithub, cnHackage, cnMeta, cnOver, cnPkg, cnPatch, cnOverPack  ∷ CName
allCNames@[cnDrvMeta, cnUpstream, cnGithub, cnHackage, cnMeta, cnOver, cnPkg, cnPatch, cnOverPack] = CName <$>
  ["DrvMeta", "Upstream", "Github", "Hackage", "Meta", "Overrides", "Package", "Patch", "OverPackage"]

deriving instance MapKey Field

instance Ctx PKGCtx where
  errCtxDesc  (_,  en) cn (Field fi) = T.pack $
    printf "%s:%s:%s" (unpack en) (unpack fi) (unpack $ fromCName cn)
  listFields  (db, en) cn            = (Field <$>) <$> listCtx cn (CtxName en) db
  dropField   (db, en) cn (Field fi) =
    rm cn (CtxName en) (Field fi) db

instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ Record a where
  prefixChars = const 2
instance {-# OVERLAPPABLE #-} (SOP.Generic a, SOP.HasDatatypeInfo a) ⇒ CtxRecord PKGCtx a where
  consCtx _ _ n _ = CName n

restoreChoiceSrc ∷ PKGCtx → IO (Maybe Int)
restoreChoiceSrc ctx = do
  msrc ∷ Maybe Text ← restoreField ctx cnOver "src"
  pure $ case msrc of
    Just "hackage" → Just 0 --     Z ∘ K $ ()
    Just "github"  → Just 1 -- S ∘ Z ∘ K $ ()
    _ → Nothing

instance Record Src where
  prefixChars = const 2
  nameMap = const [("nixHash", "hash")]
instance CtxRecord PKGCtx Src        where
  consCtx _ _ n _ = CName n
  presence   (db, en) _ = has cnOver (CtxName en) "src" db
  saveChoice (db, en)  Github{..} = write' db  cnOver (CtxName en) "src" (Just "github")
  saveChoice (db, en) Hackage{..} = write' db  cnOver (CtxName en) "src" (Just "hackage")
  restoreChoice ctx _ = restoreChoiceSrc ctx <&> fromMaybe
    (fieldError ctx cnOver "src" "'src' field ⊥: cannot choose between alternatives")
  ctxSwitch to c@(db, _) = do
    horg ← restoreChoiceSrc c
    case horg of
      Just 1 → do
        mrepo ∷ Maybe Text ← restoreField c cnMeta "repoName"
        case mrepo of
          Just reponame → pure (db, reponame) 
          Nothing       → pure c
      _ → pure c
-- instance CtxRecord PKGCtx PKGDB      where

instance CtxRecord PKGCtx Upstream   where
  consCtx       _ _ n _ = CName n
  presence   (db, en) _ = has cnOver (CtxName en) "repoName" db
instance ReadField  PKGCtx DrvMeta
instance ReadField  PKGCtx Upstream
instance ReadField  PKGCtx Meta
instance ReadField  PKGCtx OverPackage
instance ReadField  PKGCtx Overrides
instance ReadField  PKGCtx Package
instance ReadField  PKGCtx Patch
instance ReadField  PKGCtx Src
instance WriteField PKGCtx DrvMeta
instance WriteField PKGCtx Upstream
instance WriteField PKGCtx Meta
instance WriteField PKGCtx OverPackage
instance WriteField PKGCtx Overrides
instance WriteField PKGCtx Package
instance WriteField PKGCtx Patch
instance WriteField PKGCtx Src

instance  RestoreField PKGCtx GHCConfStatic            where
  restoreField (PKGDB{..}, en) cn fi = error "restoreField GHCConfStatic"
    -- coerce ∘ fromJust <$> (error "read'" ∷ a → b → c → Field → IO (Maybe Text)) -- read'
    -- (error "db") --(PKGDB (error "a") (error "b") (error "c"))
    -- (error "cn")
    -- (error "cn2") --(CtxName (error "en"))
    -- fi
    -- coerce <$> read' db cn (CtxName en)
  --pure $ GHCConfStatic "non-fun" --error ("GHCConfStatic non lol: " <> unpack at)
-- instance  ReadField PKGCtx GHCConfStatic            where readField (_, at)   _ _ = pure $ Just $ GHCConfStatic "non-fun" --error ("GHCConfStatic non lol: " <> unpack at)
instance  ReadField PKGCtx PKGDBPath                where readField (_, path) _ _ = pure $ Just $ PKGDBPath      path

instance  ReadField PKGCtx Nixpkgs                  where
  readField _ _ _ = Just <$> NH.Nix.getNixpkgs
instance WriteField PKGCtx Nixpkgs                  where writeField _ _ _ _ = pure ()

instance  ReadField PKGCtx (ElapsedSince UnixEpoch) where readField  d c f =  readField d c f <&> (P.read ∘ unpack <$>)
instance WriteField PKGCtx (ElapsedSince UnixEpoch) where writeField d c f = writeField d c f ∘ pack ∘ show

instance WriteField PKGCtx DFValue where writeField d c f = writeField d c f ∘ showDocOneLine ∘ dfDoc


-- * Basis for DB access:  writes are fake, reads are real

writeText ∷ PKGCtx → CName → Field → Text → IO ()
writeText (db, en) cn fi x = write' db cn (CtxName en) fi $ Just x

instance WriteField PKGCtx Text where
  writeField = writeText
instance  ReadField PKGCtx Text where
  readField (db,en) cn = read' db cn (CtxName en)



writeTextly ∷ Coercible a Text ⇒ PKGCtx → CName → Field → a → IO ()
writeTextly ctx cn fi = writeText ctx cn fi ∘ coerce

readTextly  ∷ Coercible a Text ⇒ PKGCtx → CName → Field → IO (Maybe a)
readTextly (db,en) cn = coerce <$> read' db cn (CtxName en)

instance {-# OVERLAPPABLE #-} Coercible a Text ⇒ WriteField PKGCtx a where
  writeField = writeTextly

instance {-# OVERLAPPABLE #-} Coercible a Text ⇒  ReadField PKGCtx a where
  readField  = readTextly



instance (Coercible a Text) ⇒ WriteField PKGCtx [a] where
  writeField ctx cn fi [] = dropField ctx cn fi
  writeField ctx cn fi xs = writeText ctx cn fi $ T.intercalate " " $ coerce <$> xs

instance (Coercible a Text) ⇒  ReadField PKGCtx [a] where
  readField (db, en) cn fi = read' db cn (CtxName en) fi <&>
    (defineMaybe [] ∘ (<&> (<&> coerce) ∘ readNames))

instance                      StoreField PKGCtx [Patch] where
  storeField ctx cn fi [] = pure ()
  storeField ctx cn (Field f) xs = sequence_
    [ storeField ctx cn (Field $ f <> "." <> showT i) x
    | (i, x) ← zip [0..] xs ]

instance                    RestoreField PKGCtx [Patch] where
  restoreField ctx@(db, en) cn fi@(Field f) = do
    keys ← listField cn (CtxName en) fi db
    flip traverse keys $ restoreField ctx cn ∘ Field ∘ ((f<>".")<>)



instance CFlag a ⇒ WriteField PKGCtx (Flag a) where
  writeField ctx cn fi x = storeField ctx cn fi $
    if x ≡ enabled
    then Just ("true" ∷ Text)
    else Nothing

instance CFlag a ⇒  ReadField PKGCtx (Flag a) where
  readField (db, en) cn fi = read' db cn (CtxName en) fi
    <&> \case
      Nothing → Just disabled
      Just _  → Just enabled



instance (MapKey k, Ord k, ReadField PKGCtx v) ⇒ RestoreField PKGCtx (Map k v) where
  restoreField ctx@(db, en) cn fi@(Field f) = do
    keys ← listField cn (CtxName en) fi db
    Map.fromList <$> (forM keys
                       -- XXX: this is an abstraction leak:
                       (\k→ (fromKeyName k,) <$> restoreField ctx cn (Field $  f <> "." <> k)))
instance (MapKey k, Ord k, WriteField PKGCtx v) ⇒ StoreField PKGCtx (Map k v) where
  storeField ctx@(db, en) cn fi@(Field f) xs = do
    all ← listField' cn (CtxName en) fi db
    forM_ all $ \old → do
      removeFileIfExists old
    forM_ (Map.toList xs) $ \(k, v) → do
      writeField ctx cn (Field $ f<>"."<>toKeyName k) v

-- * XXX: This is an instance tailored to a single field of Package:
--   , pkDrvFields        ∷ Map DrvField DFValue -- ^ Non-overridable fields only
instance {-# OVERLAPS #-} RestoreField PKGCtx (Map DrvField DFValue) where
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

readDB ∷ HasCallStack ⇒ T.Text → IO PKGDB
readDB path = mdo
  db ← recover (db, path)
  pure db

open ∷ Text → IO (Maybe PKGDB)
open path = do
  valid ← FS.validate allCNames (FS.FSDBPath path)
  if valid
    then Just <$> readDB path
    else pure Nothing

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



listFulldefns, listOverPackages ∷ HasCallStack ⇒ PKGDB → IO [Attr]
listFulldefns    = ((Attr ∘ unCtxName <$>) ∘ Set.toList <$>) ∘ list cnPkg
listOverPackages = ((Attr ∘ unCtxName <$>) ∘ Set.toList <$>) ∘ list cnOver

readRecord ∷ (HasCallStack, Record a, All2 (RestoreField PKGCtx) (Code a), CtxRecord PKGCtx a) ⇒ PKGDB → Attr → IO (Attr, a)
readRecord db at@(Attr attr) = (at,) <$> recover (db, attr)

readPackages     ∷ HasCallStack ⇒ PKGDB → IO (Map Attr Package)
readPackages     db = listFulldefns    db >>= mapM (readRecord db) <&> Map.fromList

readOverPackages ∷ HasCallStack ⇒ PKGDB → IO (Map Attr OverPackage)
readOverPackages db = listOverPackages db >>= mapM (readRecord db) <&> Map.fromList
