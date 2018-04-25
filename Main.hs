{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import           GHC.Stack

import           Control.Monad                       (forM_, unless)
import           Control.Monad.Plus                  (partial)
import           Control.Monad.IO.Class

import qualified Data.Aeson                       as AE
import           Data.Char
import qualified Data.Default.Class               as DD
import           Data.Foldable
import qualified Data.List                        as L
import           Data.Maybe
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Data.Set.Lens
import           Data.String
import           Data.Text                           (pack, unpack)
import qualified Data.Text                        as T
import           Data.Text.Format             hiding (print)

import           Language.Nix.PrettyPrinting  hiding ((<>), empty)
import qualified Language.Nix.PrettyPrinting      as Nix

import qualified Network.HTTP.Req                 as HTTP
import           Network.HTTP.Req                    (Url, Scheme(..), (/:))

import qualified Nix.Parser                       as Nix
import qualified Nix.Pretty                       as Nix
import qualified Nix.Expr                         as Nix

import qualified Options.Applicative              as O
import           Options.Applicative

import           Prelude.Unicode

import qualified System.Environment               as Sys
import qualified System.IO                        as Sys
import qualified System.IO.Temp                   as Sys

import qualified Text.PrettyPrint.ANSI.Leijen     as PP
import           Text.Printf

import           Data.Hourglass

-- import Control.Exception ( bracket )
import Control.Lens                           hiding (argument)
-- import Control.Monad ( when )
-- import Data.Maybe ( fromMaybe, isJust )
-- import Data.Monoid ( (<>) )
-- import qualified Data.Set as Set
-- import Data.String
-- import Data.Time
import qualified Distribution.Compat.ReadP as P
import Distribution.Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.BuildInfo
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import qualified Distribution.Nixpkgs.Haskell.FromCabal.PostProcess as PP (pkg)
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Distribution.Nixpkgs.Haskell.PackageSourceSpec as Nixpkgs
import Distribution.Nixpkgs.Haskell.PackageSourceSpec hiding (Package)
import Distribution.Nixpkgs.Meta
import Distribution.PackageDescription ( mkFlagName, FlagAssignment, FlagName, unFlagName, unFlagAssignment, mkFlagAssignment )
import Distribution.Package ( packageId, packageName, packageVersion )
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Text
import Language.Nix
-- import Paths_cabal2nix ( version )
-- import System.Environment ( getArgs )
-- import System.IO ( hFlush, hPutStrLn, stdout, stderr )
import qualified Text.PrettyPrint.ANSI.Leijen as P2
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(..), text, vcat, hcat, semi )
import qualified Turtle as SH

-- #if MIN_VERSION_base(4,11,0)
-- import Distribution.PackageDescription ( unFlagAssignment, mkFlagAssignment )

import           NH.Config                             (Config(..))
import qualified NH.Config                          as CFG
import           NH.Derivation
import           NH.Emission
import           NH.Misc
import           NH.Nix
import qualified NH.FS                              as PKGDB hiding (open)
import qualified NH.PKGDB                           as PKGDB
import           NH.Types

import           NH.MRecord
import qualified NH.PKGDB                           as P
import           NH.PKGDB                       hiding (parse, path)


data Cmd
  = DumpConfig
  | InternDef      SrcSpec
  | EmitDef       (Maybe Attr)
  | EmitGHCConfig
  deriving (Show)

data Options = Options
  { oCompiler  ∷ CompilerId
  , oSystem    ∷ Platform
  } deriving Show

instance Semigroup Options where
  _ <> r = r
instance Monoid Options where
  mempty = Options { oCompiler = buildCompilerId
                   , oSystem   = buildPlatform }

optionsParser ∷ O.Parser Options
optionsParser = Options
  <$> (option (readP parse)
       (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith display))
  <*> (option (readP parsePlatform)
       (long "system" <> help "target system to use when evaluating the Cabal file" <> value buildPlatform <> showDefaultWith display))
  where
    readP :: P.ReadP a a -> ReadM a
    readP p = eitherReader $ \s -> case [ r' | (r',"") <- P.readP_to_S p s ] of
                                     (r:_) -> Right r
                                     _     -> Left ("invalid value " ++ show s)

parsePlatform :: P.ReadP r Platform
parsePlatform = do arch <- P.choice [P.string "i686" >> return I386, P.string "x86_64" >> return X86_64]
                   _    <- P.char '-'
                   os   <- P.choice [P.string "linux" >> return Linux, P.string "darwin" >> return OSX]
                   return (Platform arch os)

commandParser ∷ O.Parser Cmd
commandParser = subparser
  ( command "dump-config"
    (flip info (progDesc "Dump the configuration")
      (pure DumpConfig
       <**> helper))
 <> command "intern-definition"
    (flip info (progDesc "Intern a Cabal package from Hackage/Github")
      (InternDef
       <$> subparser
        (  command "hackage"
           (flip info (progDesc "Obtain properties of a Hackage package ATTR")
             (SSHackage
               <$> argument str (metavar "ATTR")
               <*> optional (argument str (metavar "SUBDIR"))
               <**> helper))
        <> command "github"
           (flip info (progDesc "Obtain properties of a Github package")
             (SSGithub
               <$> argument str (metavar "ATTR")
               <*> argument str (metavar "USER")
               <*> argument str (metavar "REPO")
               <*> optional (argument str (metavar "SUBDIR"))
               <*> (fromMaybe "master" <$> optional (argument str (metavar "GITREF")))
               <**> helper)))
       <**> helper))
 <> command "emit-definition"
    (flip info (progDesc "Emit a full package definition (as previously interned)")
      (EmitDef
       <$> optional (argument str (metavar "ATTR"))
       <**> helper))
 <> command "ghc-config"
    (flip info (progDesc "Emit a Nix GHC configuration from PKGDB")
      (pure EmitGHCConfig
       <**> helper))
  )



main ∷ IO ()
main = do
  (,) options command ← execParser $
    info ((,) <$> optionsParser <*> commandParser
          <**> helper) $
    fullDesc <> progDesc "Perform advanced queries for nh"
             <> header   "nh - Nix Haskell tooling"
  withFull $ execute options command

withFull ∷ (Config → PKGDB → IO a) → IO a
withFull action = do
  cfPath ← CFG.findConfig
  -- putStrLn $ unpack $ "Found config at:  " <> cfPath
  cfg@Config{..} ← CFG.readConfigOldStyle cfPath
  db@PKGDB{..} ← PKGDB.open _cPKGDB <&>
    fromMaybe (error $ printf "Config %s specifies malformed PKGDB at:  %s" cfPath _cPKGDB)
  action cfg db

getDB ∷ IO PKGDB
getDB = withFull (\_→pure)



run ∷ Cmd → IO ()
run = withFull ∘ execute mempty

with ∷ (PKGDB → IO a) → IO a
with action = withFull $ const action


type CmdRunner = Options → Cmd → Config → PKGDB → IO ()
execute ∷ CmdRunner
execute opts DumpConfig cfg@Config{..} PKGDB{..} = do
  print cfg
  print opts
  echoT $ "Config at: " <> _cConfig
  echoT $ "PKGDB at:  " <> fromPKGDBPath pkgdbPath
execute opts@Options{..} (InternDef sspec) cfg db = do
  drv ← getDerivation oCompiler oSystem sspec
  let pk = internDerivation drv sspec
  store (db, fromAttr $ ssAttr sspec) pk
  pk' ← recover (db, fromAttr $ ssAttr sspec)
  -- print pk
  -- putStrLn "-------------------------------"
  -- print pk'
  -- putStrLn "==============================="
  unless (pk ≡ pk') $ do
    putStrLn "FATAL: package roundtrip error:"
    let pp  = pPrint pk
        pp' = pPrint pk'
    if (show pp ≢ show pp')
    then do
      putStrLn "  --- 1. Just imported:"
      print $ pp
      putStrLn "  --- 2. After round-trip via PKGDB:"
      print $ pp'
    else do
      putStrLn "  --- 1. Just imported:"
      print $ pkMeta pk
      putStrLn "  --- 2. After round-trip via PKGDB:"
      print $ pkMeta pk'
execute opts (EmitDef mattr) cfg db = do
  attrs ← case mattr of
            Just attr → pure [attr]
            Nothing   → PKGDB.listFulldefns db -- XXX: switch to status-based set construction
  forM_ attrs $
    \attr→ do
      pk ∷ Package ← recover (db, fromAttr attr)
      print $ nest 2 $ vcat
        [ text (unpack $ fromAttr attr) <+> equals
        , pPrint pk <> semi ]
      -- print $ text (unpack $ fromAttr attr) <+> equals <+> pPrint fulldef
execute opts (EmitGHCConfig) cfg db = do
  opNames ← listOverPackages db
  overs ← readOverPackages db
  let doc  = withTarget ToNixpkgs $ nest 2 $ vcat $ filter (≢ mempty) $ ($+$ "") ∘ pPrint <$> Map.elems overs
      text = pack $ show doc
  print doc
  SH.writeTextFile "out" text
  SH.shell "diff -uN --color /home/deepfire/nixpkgs/pkgs/development/haskell-modules/configuration-ghc-8.4.x.nix out" mempty
  pure ()
