{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
module NH.MRecord
where

import           Control.Exception
import           Control.Lens                        ((<&>))
import           Control.Monad                       (foldM, forM, forM_, join, liftM, when)
import           Data.Functor.Identity
import           Data.Function                       ((&))
import qualified Data.List                        as L
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                         as Set
import           Data.Text                           (Text, pack, unpack, toLower, toUpper, drop, take, length, isSuffixOf)
import qualified Data.Text                        as T
import qualified GHC.Types                        as Type
import           Prelude                      hiding (read, take, drop, length)
import           Prelude.Unicode
import           Text.Printf

import           Data.Proxy
import           GHC.Generics                        (Generic)
import qualified GHC.Generics                     as GHC
import           Generics.SOP                        (Rep, NS(..), NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), FieldName(..), FieldInfo(..), ConstructorInfo(..), SListI
                                                     ,from, to, hcollapse, hcliftA2, hliftA, hcliftA, unI, hsequence, hcpure, hpure)
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NS                  as SOP

import           NH.Types
import           NH.Misc

import           GHC.Stack
import           Debug.Trace (trace)



newtype CName = CName { fromCName ∷ Text } deriving (Eq, Ord, Show)

type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT
-- type ADTChoice   m xss = m (NS (K ()) xss)
type ADTChoiceIO   xss = ADTChoice IO xss

class RecordCtx    ctx where
  type ConsCtx     ctx ∷ Type.Type
  nameMap       ∷  ctx → [(Text, Text)]
  errCtxDesc    ∷  ctx → ConsCtx ctx → Field → Text
  dropField     ∷  ctx → ConsCtx ctx → Field → IO ()
  listFields    ∷  ctx → ConsCtx ctx         → IO [Field]
  -- *
  nameMap = const []
  errCtxDesc _ _ (Field f) = "field '"<>f<>"'"

class (SOP.Generic a, SOP.HasDatatypeInfo a, RecordCtx ctx) ⇒ Record ctx a where
  prefixChars   ∷  ctx → Proxy a → Int
  consCtx       ∷  ctx → Proxy a → CName → ADTChoiceT → ConsCtx ctx
  -- *
  restoreChoice ∷  ctx → Proxy a → ADTChoiceIO xss
  saveChoice    ∷  ctx → a → IO ()
  toField       ∷  ctx → Proxy a → Text → Field
  -- *
  restoreChoice _ _ = pure 0
  saveChoice    _ _ = pure ()
  toField c r x = --trace (T.unpack x <> "→" <> T.unpack (maybeRemap $ dropDetitle (prefixChars c r) x)) $
    Field $ maybeRemap $ dropDetitle (prefixChars c r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap c)

class Interpret a where
  fromText ∷ Text → a
  toText   ∷ a → Text

class ReadField    ctx a where
  readField            ∷ ctx → ConsCtx ctx → Field     → IO (Maybe a)
  default readField    ∷ (Record ctx a, Code a ~ xss, All2 (RestoreField ctx) xss)
                       ⇒ ctx → ConsCtx ctx → Field     → IO (Maybe a)
  readField ctx _ _    = Just <$> recover ctx

class WriteField   ctx a where
  writeField           ∷ ctx → ConsCtx ctx → Field → a → IO ()
  default writeField   ∷ (Record ctx a, Code a ~ xss, All2 (StoreField ctx) xss)
                       ⇒ ctx → ConsCtx ctx → Field → a → IO ()
  writeField ctx _ _ x = store   ctx x

class RecordCtx ctx ⇒
      RestoreField ctx a where restoreField  ∷ ctx → ConsCtx ctx → Field     → IO a
class StoreField   ctx a where   storeField  ∷ ctx → ConsCtx ctx → Field → a → IO ()



fieldError ∷ HasCallStack ⇒ RecordCtx ctx ⇒ ctx → ConsCtx ctx → Field → Text → b
fieldError ctx cc field mesg = errorT $ errCtxDesc ctx cc field <> ": " <> mesg



instance {-# OVERLAPPABLE #-} (RecordCtx ctx, WriteField ctx a) ⇒ StoreField   ctx a where
  storeField   ctx cc fi x = writeField ctx cc fi x

instance {-# OVERLAPPABLE #-} (RecordCtx ctx,  ReadField ctx a) ⇒ RestoreField ctx a where
  restoreField ctx cc fi   = readField  ctx cc fi
    <&> fromMaybe (fieldError ctx cc fi "mandatory field absent")

instance                      (RecordCtx ctx, WriteField ctx a) ⇒ StoreField   ctx (Maybe a) where
  storeField ctx cc fi Nothing  = dropField  ctx cc fi
  storeField ctx cc fi (Just x) = writeField ctx cc fi x

instance                      (RecordCtx ctx,  ReadField ctx a) ⇒ RestoreField ctx (Maybe a) where
  restoreField = readField



-- to        ∷ Generic a => SOP I (Code a) → a
-- SOP       ∷ NS (NP f) xss → SOP f xss
-- S         ∷ NS a xs → NS a (x : xs)
-- Z         ∷ a x → NS a (x : xs)
-- hcpure    ∷ (AllN h c xs, HPure h)
--           ⇒ proxy c → (forall a. c a ⇒ f a) → h f xs
-- hsequence ∷ (SListIN h xs, SListIN (Prod h) xs, HSequence h, Applicative f)
--           ⇒ h f xs → f (h I xs)
-- hcollapse ∷ (SListIN h xs, HCollapse h)
--           ⇒ h (K a) xs → CollapseTo h a
-- hcliftA2  ∷ (AllN (Prod h) c xs, HAp h, HAp (Prod h))
--           ⇒ proxy c → (forall a. c a ⇒ f a → f' a → f'' a)
--           → Prod h f xs → h f' xs → h f'' xs
-- hcliftA   ∷ (AllN (Prod h) c xs, HAp h)
--           ⇒ proxy c → (forall a. c a ⇒ f a → f' a) → h f xs → h f' xs

recover  ∷ ∀ a ctx xss. (Record ctx a, HasDatatypeInfo a, Code a ~ xss, All2 (RestoreField ctx) xss)
         ⇒ ctx → IO a
recover ctx = do
  to <$> (hsequence =<<
          (!!)        (SOP.apInjs_POP  $ recover' p ctx $ datatypeInfo p) <$> restoreChoice ctx p)
          -- indexNPbyNS (SOP.apInjs'_POP $ recover' p ctx $ datatypeInfo p) <$> (pure $ S(Z(K())))
  where
    p = Proxy ∷ Proxy a
    indexNPbyNS ∷ SListI xss ⇒ NP (K (SOP f yss)) xss → NS (K ()) xss → SOP f yss
    indexNPbyNS np ns = hcollapse $ SOP.hliftA2 (\x (K ()) → x) np ns

data NConstructorInfo xs where
  NC ∷ ConstructorInfo xs → Int →  NConstructorInfo xs

enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP NConstructorInfo xs
enumerate cs = SOP.hliftA2 (\c (K n)→ NC c n) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])

recover' ∷ ∀ a ctx xss. (Record ctx a, All2 (RestoreField ctx) xss, All SListI xss)
         ⇒ Proxy a → ctx → DatatypeInfo xss → POP IO xss
recover' proxy ctx (ADT _ _ cs) = POP $ hcliftA (pAllRFields (Proxy ∷ Proxy ctx)) (recoverFor proxy ctx) $ enumerate cs
recover' _ _ _ = error "Non-ADTs not supported."

recoverFor ∷ ∀ a ctx xs. (Record ctx a, All (RestoreField ctx) xs)
           ⇒ Proxy a → ctx → NConstructorInfo xs → NP IO xs
recoverFor proxy ctx (NC (Record consName fis) consNr) = withNames proxy ctx (pack consName) consNr $ hliftA (K ∘ pack ∘ SOP.fieldName) fis
recoverFor _ _ _ = error "Non-Record (plain Constructor, Infix) ADTs not supported."

withNames ∷ ∀ a ctx xs. (Record ctx a, All (RestoreField ctx) xs, SListI xs)
          ⇒ Proxy a → ctx → Text → Int → NP (K Text) xs → NP IO xs
withNames p ctx consName consNr (fs ∷ NP (K Text) xs) = hcliftA (pRField (Proxy ∷ Proxy ctx)) aux fs
  where
    aux ∷ RestoreField ctx f ⇒ K Text f → IO f
    aux (K "") = error "Empty field names not supported."
    aux (K fi) = restoreField ctx (consCtx ctx p (CName consName) consNr) (toField ctx p fi)

store   ∷ ∀ a ctx.     (Record ctx a, All2 (StoreField ctx) (Code a))            ⇒ ctx → a → IO ()
store   ctx x = do
  let di@(ADT _ _ cs) = case datatypeInfo (Proxy ∷ Proxy a) of
        x@ADT{} → x
        _ → error "Non-ADTs not supported."
  sequence_ $ store' ctx x (datatypeInfo (Proxy ∷ Proxy a)) (from x)
  when (SOP.lengthSList cs > 1) $
    saveChoice ctx x

store'  ∷              (Record ctx a, All2 (StoreField ctx) xss, All SListI xss) ⇒ ctx → a → DatatypeInfo xss → SOP I xss → [IO ()]
store'  ctx x (ADT _ _ cs) = store'' ctx x (enumerate cs)

store'' ∷ ∀ a ctx xss. (Record ctx a, All2 (StoreField ctx) xss, All SListI xss) ⇒ ctx → a → NP NConstructorInfo xss → SOP I xss → [IO ()]
store'' ctx x info (SOP sop) =
  hcollapse $ hcliftA2 (pAllSFields (Proxy ∷ Proxy ctx)) (storeCtor ctx x) info sop

storeCtor ∷ ∀ a ctx xs. (Record ctx a, All (StoreField ctx) xs) ⇒ ctx → a → NConstructorInfo xs → NP I xs → K [IO ()] xs
storeCtor ctx x (NC (Record consName fs) consNr) = K ∘ hcollapse ∘ hcliftA2 (pSField (Proxy ∷ Proxy ctx)) aux fs
  where
    p = Proxy ∷ Proxy a
    aux ∷ StoreField ctx f ⇒ FieldInfo f → I f → K (IO ()) f
    aux (FieldInfo fi) (I a) = K $ do
      storeField ctx (consCtx ctx p (CName $ pack consName) consNr) (toField ctx p $ pack fi) a

pRecord       ∷ Proxy ctx → Proxy (Record ctx)
pRecord     _ = Proxy
pRField       ∷ Proxy ctx → Proxy (RestoreField ctx)
pRField     _ = Proxy
pSField       ∷ Proxy ctx → Proxy (StoreField ctx)
pSField     _ = Proxy
pAllRFields   ∷ Proxy ctx → Proxy (All (RestoreField ctx))
pAllRFields _ = Proxy
pAllSFields   ∷ Proxy ctx → Proxy (All (StoreField ctx))
pAllSFields _ = Proxy
