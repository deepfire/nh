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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module NH.FS
  ( init, open
  , list, listCtx
  , has
  , read
  , write, rm
  )
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
import           Prelude                      hiding (read, take, drop, init, length)
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

import           NH.MRecord
import           NH.Types
import           NH.Misc
import qualified NH.Nix                           as Nix



(</>) ∷ Text → Text → Text
l </> r = l <> T.singleton Sys.pathSeparator <> r

(<.>) ∷ Text → Text → Text
l <.> r = l <> T.singleton '.' <> r



init ∷ [CName] → Text → IO PKGDB
init allCNames pkgdbPath = Sys.withSystemTempDirectory "nh-temp" $
  \((</> "new") ∘ pack → assyPath) → do
    initDBat $ unpack pkgdbPath
    -- Unfortunately, the create-then-rename trick is useless,
    -- as /tmp is often on a different filesystem from the target.
    -- And so the move is neither atomic, neither supported by renamePath.
    --
    -- Sys.renamePath (unpack assyPath) (unpack pkgdbPath)
    pkgdbNixpkgs ← (Nix.internHaskellNixpkgs =<< Nix.locateNixpkgs)
    pure PKGDB{..}
      where
        initDBat dir = do
          -- Yes, the below check isn't atomic, so is an integrity risk.
          -- Unfortunately, see above message.
          Sys.doesPathExist dir >>= flip when
            (error$printf "Cannot init PKGDB at busy path:  %s" dir)
          Sys.createDirectory dir
          forM_ allCNames $ \(CName cn)→
            Sys.createDirectory $ dir Sys.</> unpack cn

open ∷ [CName] → Text → IO (Maybe PKGDB)
open allCNames path = do
  valid ← foldM (\acc sub→ (acc ∧) <$> Sys.doesDirectoryExist (unpack $ path </> sub))
    True (fromCName <$> allCNames)
  if valid
    then do -- printf "Found valid PKGDB at:  %s\n" $ unpack path
            nixpkgs ← Nix.internHaskellNixpkgs =<< Nix.locateNixpkgs
            pure $ Just $ PKGDB path nixpkgs
    else pure Nothing

cnPath ∷ PKGDB → CName → Text
cnPath PKGDB{..} (CName cn) = pkgdbPath </> cn

path ∷ CName → CtxName → Field → PKGDB → Text
path cn (CtxName en) (Field fi) db =
  cnPath db cn </> en <.> fi

read ∷ CName → CtxName → Field → PKGDB → IO (Maybe Text)
read cn en fi db = do
  let p = unpack $ path cn en fi db
  (∃) ← Sys.doesFileExist p
  if (∃)
    then Just <$> Sys.readFile p
    else do
    -- putStrLn $ "missing field: "<>show ty<>"/"<>unpack (fromField fi)<>" at " <>show p
    pure Nothing

parse ∷ SimpleToken a ⇒ CName → CtxName → Field → PKGDB → IO (Maybe a)
parse cn en fi db = do
  join ∘ (diagReadCaseInsensitive <$>) <$> read cn en fi db

has ∷ CName → CtxName → Field → PKGDB → IO Bool
has cn en fi db = do
  let p = unpack $ path cn en fi db
  Sys.doesFileExist p

rm ∷ CName → CtxName → Field → PKGDB → IO ()
rm cn en fi db = removeFileIfExists $ path cn en fi db

write ∷ CName → CtxName → Field → Maybe Text → PKGDB → IO ()
write cn en fi mval db = do
  let fpath   = unpack $ path cn en fi db
  fileExists ← Sys.doesFileExist fpath
  case (fileExists, mval) of
    (False, Nothing) → pure ()
    (True,  Nothing) → Sys.removeFile fpath
    (_,     Just v)  → do
      dirExists ← Sys.doesDirectoryExist $ unpack $ cnPath db cn
      if dirExists
        then Sys.writeFile fpath v
        else errorT $ "Malformed PKGDB: structural subdir doesn't exist: " <> cnPath db cn

listCName ∷ CName → PKGDB → IO [Text]
listCName cn db = do
  (T.pack <$>) <$> Sys.listDirectory (unpack $ cnPath db cn)

list ∷ CName → PKGDB → IO (Set CtxName)
list cn db = do
  fulls ← listCName cn db
  let split = T.splitOn "." <$> fulls
      names = (!! 0) <$> split
  pure $ CtxName <$> (Set.delete "" $ Set.fromList names)

listCtx ∷ CName → CtxName → PKGDB → IO [Text]
listCtx cn (CtxName en) db = listCName cn db <&>
  (drop (length en + 1) <$>) ∘ filter (T.isPrefixOf (en <> "."))



data MetaF
  = MSuppressShadow
  | MDisable
  | MChdir
  | MRepoName
  | MExplanation Field
  | MERDeps
  deriving (Eq, Show)

-- metaPath ∷ Attr → MetaF → PKGDB → Text
-- metaPath at mf db =
--   path cnMeta (attrCtx at) (metaField mf) db
--   where metaField ∷ MetaF → Field
--         metaField MRepoName                = Field "repoName"
--         metaField (MExplanation (Field x)) = Field $ x <> ".explanation"
--         metaField x                        = Field $ lowerShowT x
