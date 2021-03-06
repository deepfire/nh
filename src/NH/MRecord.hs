{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
module NH.MRecord
where

import           Control.Exception
import           Control.Lens                        ((<&>))
import           Control.Monad                       (foldM, forM, forM_, join, liftM, when)
import           Data.Functor.Identity
import           Data.Function                       ((&))
import           Data.Bool
import qualified Data.List                        as L
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                         as Set
import           Data.String
import           Data.Text                           (Text, pack, unpack, toLower, toUpper, drop, take, length, isSuffixOf)
import qualified Data.Text                        as T
import           Data.Typeable
import qualified GHC.Types                        as Type
import           Prelude                      hiding (read, take, drop, length)
import           Prelude.Unicode
import           Text.Printf

import           Data.Proxy
import           GHC.Generics                        (Generic)
import qualified GHC.Generics                     as GHC
import           GHC.Stack
import           Generics.SOP                        (Rep, NS(..), NP(..), SOP(..), POP(..), I(..), K(..), Code, All, All2
                                                     ,HasDatatypeInfo(..), DatatypeInfo(..), FieldName(..), FieldInfo(..), ConstructorInfo(..), SListI
                                                     ,from, to, hcollapse, hcliftA2, hliftA, hcliftA, unI, hsequence, hcpure, hpure)
import qualified Generics.SOP                     as SOP
import qualified Generics.SOP.NS                  as SOP

import           Debug.Trace (trace)



data NConstructorInfo xs where
  NC ∷ ConstructorInfo xs → Int →  NConstructorInfo xs

enumerate ∷ SListI xs ⇒ NP ConstructorInfo xs → NP NConstructorInfo xs
enumerate cs = SOP.hliftA2 (\c (K n)→ NC c n) cs (fromJust $ SOP.fromList $ L.take (SOP.lengthSList cs) [0..])

mapFields ∷ ∀ cst a c xs. (SOP.Generic a, SOP.HasDatatypeInfo a, Code a ~ '[xs], All cst xs)
          ⇒ (∀ b . cst b ⇒ Text → b → c) → a → [c]
mapFields f x = case datatypeInfo (Proxy ∷ Proxy a) of
                  (ADT _ _ ((Record _ fi) :* Nil)) →
                    hcollapse $ hcliftA2 (Proxy ∷ Proxy cst)
                                (\(FieldInfo fi) (I val)→
                                   K $ f (pack fi) val)
                                (fi ∷ NP FieldInfo xs)
                                (SOP.unZ ∘ SOP.unSOP $ from x)
                  _ → error "Non-ADTs/non-Records/sums not supported."

data A = A { a ∷ String, b ∷ Int } deriving (Show, GHC.Generic)
instance SOP.Generic A
instance SOP.HasDatatypeInfo A
x = mapFields @Show (\fi val→ fi<>": "<>pack (show val)) $ A "a" 1

-- mapFields ∷ ∀ cst a c xs. (SOP.Generic a, SOP.HasDatatypeInfo a, Code a ~ '[xs], All cst xs)
--           ⇒ (∀ b . cst b ⇒ b → c) → a → [c]
-- mapFields f x = case datatypeInfo (Proxy ∷ Proxy a) of
--                   info@(ADT _ _ ((Record _ _) :* Nil)) →
--                     hcollapse $ hcliftA (Proxy ∷ Proxy cst) (\(I x)→ K $ f x) (SOP.unZ ∘ SOP.unSOP $ from x)
--                   _ → error "Non-ADTs/non-Records/sums not supported."



type family ConsCtx ctx ∷ Type.Type

class Ctx ctx where
  errCtxDesc    ∷ ctx → ConsCtx ctx → Field → Text
  dropField     ∷ ctx → ConsCtx ctx → Field → IO ()
  --hasField      ∷  ctx → ConsCtx ctx → Field → IO Bool -- useless for presenceByField
  listFields    ∷ ctx → ConsCtx ctx         → IO [Field]
  -- *
  errCtxDesc _ _ (Field f) = "field '"<>f<>"'"

class Record a where
  prefixChars       ∷ Proxy a → Int
  nameMap           ∷ Proxy a → [(Text, Text)]
  toField           ∷ Proxy a → Text → Field
  -- *
  nameMap           = const []
  toField r x = --trace (T.unpack x <> "→" <> T.unpack (maybeRemap $ dropDetitle (prefixChars r) x)) $
    Field $ maybeRemap $ dropDetitle (prefixChars r) x
    where maybeRemap x = maybe x id (lookup x $ nameMap r)
          dropDetitle ∷ Int → Text → Text
          dropDetitle n (drop 2 → x) = toLower (take 1 x) <> drop 1 x



newtype Field = Field { fromField ∷ Text } deriving (Eq, IsString, Ord, Show)

type ADTChoiceT        = Int
type ADTChoice   m xss = m ADTChoiceT
-- type ADTChoice   m xss = m (NS (K ()) xss)
type ADTChoiceIO   xss = ADTChoice IO xss

class (SOP.Generic a, SOP.HasDatatypeInfo a, Ctx ctx, Record a) ⇒ CtxRecord ctx a where
  consCtx           ∷ ctx → Proxy a → Text → ADTChoiceT → ConsCtx ctx
  -- * Defaulted methods
  presence          ∷ ctx → Proxy a → IO Bool
  --presenceByField   ∷ ctx → Proxy a → IO (Maybe Field) -- not clear how to implement generically -- what constructor to look at?
  restoreChoice     ∷ HasCallStack
                    ⇒ ctx → Proxy a → ADTChoiceIO xss
  saveChoice        ∷ ctx → a → IO ()
  ctxSwitch         ∷ HasCallStack
                    ⇒ Proxy a → ctx → IO ctx
  -- * Method defaults
  presence      _ p = pure True
  restoreChoice _ _ = pure 0
  saveChoice    _ _ = pure ()
  ctxSwitch  to ctx = pure ctx



class Interpret a where
  -- XXX: sadly unused
  fromText ∷ Text → a
  toText   ∷ a → Text

class ReadField    ctx a where
  readField            ∷ HasCallStack ⇒ ctx → ConsCtx ctx → Field     → IO (Maybe a)
  default readField    ∷ (CtxRecord ctx a, Code a ~ xss, All2 (RestoreField ctx) xss, HasCallStack, Typeable a)
                       ⇒ ctx → ConsCtx ctx → Field     → IO (Maybe a)
  readField ctx _ _    = do
    let p = Proxy ∷ Proxy a
    newCtx ← ctxSwitch p ctx
    bool (pure Nothing) (Just <$> recover newCtx) =<< presence newCtx p

class WriteField   ctx a where
  writeField           ∷ HasCallStack ⇒ ctx → ConsCtx ctx → Field → a → IO ()
  default writeField   ∷ (CtxRecord ctx a, Code a ~ xss, All2 (StoreField ctx) xss, HasCallStack)
                       ⇒ ctx → ConsCtx ctx → Field → a → IO ()
  writeField ctx _ _ x = store   ctx x

class Ctx ctx ⇒
      RestoreField ctx a where
  restoreField         ∷ HasCallStack ⇒ ctx → ConsCtx ctx → Field     → IO a

class StoreField   ctx a where
  storeField           ∷ HasCallStack ⇒ ctx → ConsCtx ctx → Field → a → IO ()



fieldError ∷ HasCallStack ⇒ Ctx ctx ⇒ ctx → ConsCtx ctx → Field → Text → b
fieldError ctx cc field mesg = error $ unpack $ errCtxDesc ctx cc field <> ": " <> mesg



instance {-# OVERLAPPABLE #-} (Ctx ctx, WriteField ctx a) ⇒ StoreField   ctx a where
  storeField   ctx cc fi x = writeField ctx cc fi x

instance {-# OVERLAPPABLE #-} (Ctx ctx,  ReadField ctx a) ⇒ RestoreField ctx a where
  restoreField ctx cc fi   = trace ("restoreFi→readFi "<>unpack (fromField fi)) $ readField  ctx cc fi
    <&> fromMaybe (fieldError ctx cc fi "mandatory field absent")

instance                      (Ctx ctx, WriteField ctx a) ⇒ StoreField   ctx (Maybe a) where
  storeField ctx cc fi Nothing  = dropField  ctx cc fi
  storeField ctx cc fi (Just x) = writeField ctx cc fi x

instance                      (Ctx ctx,  ReadField ctx a) ⇒ RestoreField ctx (Maybe a) where
  restoreField a b fi = trace ("restoreFi Maybe→readFi "<>unpack (fromField fi)) $ readField a b fi



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

recover  ∷ ∀ a ctx xss. (CtxRecord ctx a, HasDatatypeInfo a, Code a ~ xss, All2 (RestoreField ctx) xss, HasCallStack)
         ⇒ ctx → IO a
recover ctx = do
  to <$> (hsequence =<<
         -- XXXXXXXXXXXXXXXXXXXX: so, here's the theory:
         -- a successful state loop needs an unobscured constructor to be returned,
         -- but this choice action does obscure it perfectly
         -- ...
          (!!)        (SOP.apInjs_POP  $ trace "SOP.apInjs_POP ← recover'" $ recover' p ctx (datatypeInfo p)) <$> pure 0
         -- XXXXXXXXXXXXXXXXXXXX: just pick 0'th: not much help
         --(trace "fmap ← restoreChoice" $ restoreChoice ctx p)
         )
          -- indexNPbyNS (SOP.apInjs'_POP $ recover' p ctx $ datatypeInfo p) <$> (pure $ S(Z(K())))
  where
    p = Proxy ∷ Proxy a
    indexNPbyNS ∷ SListI xss ⇒ NP (K (SOP f yss)) xss → NS (K ()) xss → SOP f yss
    indexNPbyNS np ns = hcollapse $ SOP.hliftA2 (\x (K ()) → x) np ns

recover' ∷ ∀ a ctx xss. (CtxRecord ctx a, All2 (RestoreField ctx) xss, All SListI xss, HasCallStack)
         ⇒ Proxy a → ctx → DatatypeInfo xss → POP IO xss
recover' proxy ctx (ADT _ name cs) = POP $ hcliftA (pAllRFields (Proxy ∷ Proxy ctx)) (recoverFor proxy ctx (pack name)) $ enumerate cs
recover' _ _ _ = error "Non-ADTs not supported."

recoverFor ∷ ∀ a ctx xs. (CtxRecord ctx a, All (RestoreField ctx) xs, HasCallStack)
           ⇒ Proxy a → ctx → Text → NConstructorInfo xs → NP IO xs
recoverFor proxy ctx _ (NC (Record consName fis) consNr) = withNames proxy ctx (pack consName) consNr $ hliftA (K ∘ pack ∘ SOP.fieldName) fis
recoverFor _ _ name _ = error $ printf "Non-Record (plain Constructor, Infix) ADTs not supported: type %s." (unpack name)

withNames ∷ ∀ a ctx xs. (CtxRecord ctx a, All (RestoreField ctx) xs, SListI xs, HasCallStack)
          ⇒ Proxy a → ctx → Text → Int → NP (K Text) xs → NP IO xs
withNames p ctx consName consNr (fs ∷ NP (K Text) xs) = hcliftA (pRField (Proxy ∷ Proxy ctx)) aux fs
  where
    aux ∷ RestoreField ctx f ⇒ K Text f → IO f
    aux (K "") = error "Empty field names not supported."
    aux (K fi) =
      trace ("withNames/aux ← restoreField "<>unpack fi<>"/"<>unpack consName) $
      restoreField (trace ("restoreField ← ctx fi="<>unpack fi) ctx)
      (trace ("restoreField ← consCtx fi="<>unpack fi) $ consCtx ctx p consName consNr)
      (trace ("restoreField ← toField fi="<>unpack fi) $ toField p fi)

store   ∷ ∀ a ctx.     (CtxRecord ctx a, All2 (StoreField ctx) (Code a), HasCallStack)
        ⇒ ctx → a → IO ()
store   ctx x = do
  let di@(ADT _ _ cs) = case datatypeInfo (Proxy ∷ Proxy a) of
        x@ADT{} → x
        _ → error "Non-ADTs not supported."
  sequence_ $ store' ctx x (datatypeInfo (Proxy ∷ Proxy a)) (from x)
  when (SOP.lengthSList cs > 1) $
    saveChoice ctx x

store'  ∷              (CtxRecord ctx a, All2 (StoreField ctx) xss, All SListI xss, HasCallStack)
        ⇒ ctx → a → DatatypeInfo xss → SOP I xss → [IO ()]
store'  ctx x (ADT _ _ cs) = store'' ctx x (enumerate cs)

store'' ∷ ∀ a ctx xss. (CtxRecord ctx a, All2 (StoreField ctx) xss, All SListI xss, HasCallStack)
        ⇒ ctx → a → NP NConstructorInfo xss → SOP I xss → [IO ()]
store'' ctx x info (SOP sop) =
  hcollapse $ hcliftA2 (pAllSFields (Proxy ∷ Proxy ctx)) (storeCtor ctx x) info sop

storeCtor ∷ ∀ a ctx xs. (CtxRecord ctx a, All (StoreField ctx) xs, HasCallStack)
          ⇒ ctx → a → NConstructorInfo xs → NP I xs → K [IO ()] xs
storeCtor ctx x (NC (Record consName fs) consNr) = K ∘ hcollapse ∘ hcliftA2 (pSField (Proxy ∷ Proxy ctx)) aux fs
  where
    p = Proxy ∷ Proxy a
    aux ∷ StoreField ctx f ⇒ FieldInfo f → I f → K (IO ()) f
    aux (FieldInfo fi) (I a) = K $ do
      storeField ctx (consCtx ctx p (pack consName) consNr) (toField p $ pack fi) a

pRecord       ∷ Proxy ctx → Proxy (CtxRecord ctx)
pRecord     _ = Proxy
pRField       ∷ Proxy ctx → Proxy (RestoreField ctx)
pRField     _ = Proxy
pSField       ∷ Proxy ctx → Proxy (StoreField ctx)
pSField     _ = Proxy
pAllRFields   ∷ Proxy ctx → Proxy (All (RestoreField ctx))
pAllRFields _ = Proxy
pAllSFields   ∷ Proxy ctx → Proxy (All (StoreField ctx))
pAllSFields _ = Proxy
