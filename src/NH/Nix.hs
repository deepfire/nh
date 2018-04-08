{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.Nix
where

import           Control.Lens                 hiding (argument)
import           Data.Foldable                       (find)
import           Data.Fix                            (Fix(..))
import qualified Data.List                        as L
import qualified Data.Map                         as Map
import           Data.Set                            (Set)
import qualified Data.Set                         as Set
import           Data.Set.Lens                       (setOf)
import qualified Data.Text                        as T
import           Data.Text                           (Text, pack, unpack)
import           Prelude.Unicode

import           Nix.Eval
import           Nix.Expr
import           Nix.Parser
import           Nix.Pretty



import           NH.Misc
import           NH.Types



data NixType
  = NTStr
  | NTPath
  | NTBool
  | NTInt
  | NTVar
  | NTList NixType
  | NTAttrset (Map.Map Text NixType)
  | NTFunction [NixType] NixType
  deriving (Eq, Ord, Show)



internHaskellNixpkgs ∷ Text → IO Nixpkgs
internHaskellNixpkgs nixpkgsPath = do
  nixpkgsHackagePackages ← nixpkgsHackagePackagesTopAttrs nixpkgsPath
  pure Nixpkgs{..}

locateNixpkgs ∷ IO Text
locateNixpkgs = T.stripEnd <$> stdoutCall (Desc "locate <nixpkgs>")
  (Exec "nix-instantiate") ["--eval", "-E", "<nixpkgs>"]

nixpkgsHackagePackagesTopAttrs ∷ Text → IO (Set Attr)
nixpkgsHackagePackagesTopAttrs nixpkgs = do
  let file = nixpkgs <> "/pkgs/development/haskell-modules/hackage-packages.nix"
  stdoutCallSh (Desc "") (ShCmd $ "grep '\" = callPackage' "<> file <>" | sed 's/^.*\"\\(.*\\)\" = callPackage.*$/\\1/'")
  <&> flip Set.difference (Set.singleton "") ∘ Set.fromList ∘ (Attr <$>) ∘ T.lines

nixpkgsShadows ∷ Attr → Nixpkgs → Set Attr
nixpkgsShadows (Attr attr) Nixpkgs{..} =
  flip Set.filter nixpkgsHackagePackages $
  (\(Attr x)→ T.isPrefixOf attr x ∧ x /= attr)

attrDefined ∷ Attr → Nixpkgs → Bool
attrDefined attr = Set.member attr ∘ nixpkgsHackagePackages

attrShadowedAt ∷ Attr → Release → Nixpkgs → Bool
attrShadowedAt attr release = (attrShadow attr release `Set.member`) ∘ nixpkgsHackagePackages

attrShadow ∷ Attr → Release → Attr
attrShadow (Attr a) (Release r) = Attr $ a <> "_" <> T.map (charMap '.' '_')  r

attrHasShadows ∷ Attr → Nixpkgs → Bool
attrHasShadows attr = (≢ Set.empty) ∘ nixpkgsShadows attr

  -- Thatf's too slow: several seconds
  -- result ← parseNixFile (unpack file)
  -- let parse = case result of
  --               Success x   → x
  --               Failure err → errorT $ "Failed to parse " <> file <> ":\n" <> showT err
  -- pure $ case parse of
  --          Fix (NAbs
  --               (ParamSet (FixedParamSet _) Nothing)
  --               (Fix (NAbs _ (Fix (NSet xs)))))
  --            → Set.fromList [ Attr name
  --                           | NamedVar [DynamicKey (Plain (DoubleQuoted [Plain name]))] _ ← xs ]
  --          _ → errorT $ "Unexpected parsed structure in " <> file
