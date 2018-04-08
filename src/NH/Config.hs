{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.Config
where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad                       (when)
import           Data.Maybe
import           Data.Text                           (Text, pack, unpack)
import qualified Data.Text                        as T
import qualified Options.Applicative              as O
import           Options.Applicative          hiding (disabled)
import           Prelude.Unicode
import qualified System.Directory                 as Sys
import qualified System.FilePath                  as Sys
import qualified Text.Parser.Char                 as P
import qualified Text.Parser.Combinators          as P
import qualified Text.Parser.Token                as P
import qualified Text.Trifecta.Parser             as P

import           Debug.Trace

import           NH.Types
import           NH.FS
import           NH.Misc


-- * Constants
acmeAttr ∷ Attr
acmeAttr = "nh-acme-grand-total-attribute"

configName ∷ Text
configName = ".nh2"


data Config = Config
  { _cConfig             ∷ Text
  , _cGHCVer             ∷ GHCVer
  , _cGHCConfig          ∷ Text
  , _cGHCOverrides       ∷ Text
  , _cGHCPackages        ∷ Text
  , _cPKGDB              ∷ Text
  , _cGithubUser         ∷ GithubUser
  , _cTargetNixpkgs      ∷ Flag Local
  } deriving (Show)
makeLenses 'Config

instance Semigroup Config where
  _ <> r = r

instance Monoid Config where
  mempty = Config
    { _cConfig           = ".nh2"
    , _cGHCVer           = "841"
    , _cGHCConfig        = "configuration-ghc-8.4.x.nix"
    , _cGHCOverrides     = "overrides.nix"
    , _cGHCPackages      = "packages.nix"
    , _cPKGDB            = "pkgdb"
    , _cGithubUser       = "nobody"
    , _cTargetNixpkgs    = disabled
    }


-- This is for backward compat
readConfigOldStyle ∷ Text → IO Config
readConfigOldStyle path = do
  kvs ← P.parseFromFile readShellAssignments (unpack path) <&>
    fromMaybe (error "Failed to parse config file.")
  let setSingleField ∷ Text → Config → Text → Config
      setSingleField "TARGET_NIXPKGS"      x v = x & cTargetNixpkgs .~ fromBool (v ≢ "")
      setSingleField "PKGDB"               x v = x & cPKGDB         .~ v
      setSingleField "GHC"                 x v = x & cGHCVer        .~ GHCVer v
      setSingleField "GHC_CONFIG"          x v = x & cGHCConfig     .~ v
      setSingleField "GITHUB_USER"         x v = x & cGithubUser    .~ GithubUser v
      -- XXX: the following is a bit too silent
      setSingleField smth                  x v = x -- flip trace x $ "Ignoring config field: " <> T.unpack smth
  pure $ foldl (\cfg (k,v)→ setSingleField k cfg v) mempty kvs
       & cConfig       .~ path
       & cGHCOverrides .~ pack (Sys.takeDirectory $ unpack path) <> "/overrides.nix"
       & cGHCPackages  .~ pack (Sys.takeDirectory $ unpack path) <> "/packages.nix"

readShellAssignment ∷ (Monad p, P.TokenParsing p) ⇒ p (T.Text, T.Text)
readShellAssignment = do
  P.whiteSpace
  key ← P.some $ P.alphaNum <|> P.oneOf "_."
  P.char '='
  P.optional $ P.char '"'
  val ← P.many $ P.noneOf "\"\t\n\r"
  P.optional $ P.char '"'
  pure (T.pack key, T.pack val)

readShellAssignments ∷ (Monad p, P.TokenParsing p) ⇒ p [(T.Text, T.Text)]
readShellAssignments = do
  lines' ← P.sepEndBy readShellAssignment (P.newline)
  P.whiteSpace
  P.eof
  pure lines'


findConfig ∷ IO Text
findConfig = loop "."
  where
    loop cur = do
      let fullPath = cur <> "/" <> T.unpack configName
      (∃) ← Sys.doesFileExist fullPath
      if (∃)
        then pure $ T.pack fullPath
        else do
        when (cur ≡ "/") $
          errorConfigMissingAndSuggestAction
        -- putStrLn $ "Not found config at: " <> fullPath
        parent ← Sys.canonicalizePath $ cur <> "/.."
        loop parent

errorConfigMissingAndSuggestAction ∷ a
errorConfigMissingAndSuggestAction = error $ unlines
  [ "ERROR: the .nh configuration file is present neither in the working directory,"
  , "       nor in the containing hierarchy."
  , ""
  , "Consider the following:  cat > .nh"
  , ""
  , "$(emit_nh_config /home/user/configuration-ghc84x)"
  , ""
  , "Don't have packages.nix?  cat > packages.nix"
  , ""
  , "$(emit_packages_nix)"
  , ""
  ]
