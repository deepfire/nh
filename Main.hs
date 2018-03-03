{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main
where

import           Control.Monad                       (forM_, unless)
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
import Distribution.Nixpkgs.Haskell.FromCabal.Normalize ( normalizeCabalFlags )
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import qualified Distribution.Nixpkgs.Haskell.FromCabal.PostProcess as PP (pkg)
import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
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

-- #if MIN_VERSION_base(4,11,0)
-- import Distribution.PackageDescription ( unFlagAssignment, mkFlagAssignment )

import           NH.Config                             (Config(..))
import qualified NH.Config                          as CFG
import qualified NH.PKGDB                           as PKGDB
import qualified NH.Projection                      as Proj
import           NH.Types


-- * Cabal2nix
(//) ∷ T.Text → T.Text → T.Text
x // y = x<>"/"<>y

attrC2NUrl ∷ SrcSpec → URL
attrC2NUrl  (SSGithub attr user repo ref msub) = URL $ "https://github.com"//fromUser user//fromRepo repo
attrC2NUrl (SSHackage attr msub)               = URL $ "cabal://"<>fromAttr attr


data Cmd
  = GetPackage        SrcSpec
  | EmitExtraDefn     (Maybe Attr)
  | Stub
  deriving (Show)

data Options = Options
  { oCompiler  ∷ CompilerId
  , oSystem    ∷ Platform
  }

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
  ( command "get-package"
    (flip info (progDesc "Obtain properties of Cabal package ATTR")
      (GetPackage
       <$> subparser
        (  command "hackage" (flip info (progDesc "Obtain properties of Cabal package ATTR")
                               (SSHackage
                                 <$> argument str (metavar "ATTR")
                                 <*> optional (argument str (metavar "SUBDIR"))))
        <> command "github"  (flip info (progDesc "Obtain properties of Cabal package ATTR")
                               (SSGithub
                                 <$> argument str (metavar "ATTR")
                                 <*> argument str (metavar "USER")
                                 <*> argument str (metavar "REPO")
                                 <*> optional (argument str (metavar "SUBDIR"))
                                 <*> (fromMaybe "master" <$> optional (argument str (metavar "GITREF"))))))))
 <> command "emit-extra-defn"
    (flip info (progDesc "Emit an extra definition")
      (EmitExtraDefn
       <$> optional (argument str (metavar "ATTR"))))
 <> command "commit"
    (flip info (progDesc "Record changes to the repository")
      (pure Stub))
  )


main ∷ IO ()
main = do
  cfPath ← CFG.findConfig
  -- putStrLn $ unpack $ "Found config at:  " <> cfPath
  cfg@Config{..} ← CFG.readConfigOldStyle cfPath
  db@PKGDB.PKGDB{..} ← PKGDB.open _cPKGDB <&>
    fromMaybe (error $ printf "Config %s specifies malformed PKGDB at:  %s" cfPath _cPKGDB)
  (,) options command ← execParser $
    info (((,) <$> optionsParser <*> commandParser) <**> helper)
    (fullDesc
     <> progDesc "Perform advanced queries for nh"
     <> header "nh - Nix Haskell tooling" )
  execute options db command


execute ∷ Options → PKGDB.PKGDB → Cmd → IO ()
execute opts db (GetPackage sspec) = do
  drv ← getDerivation opts sspec
  let extradef = internDerivation drv sspec
  -- print extradef
  PKGDB.writeExtraDefn db extradef
  extradef' ← PKGDB.readExtraDefn db (ssAttr sspec)
  unless (extradef ≡ extradef') $ do
    putStrLn "FATAL: extra definition roundtrip error:"
    putStrLn "  --- 1. Just imported:"
    print $ pPrint extradef
    putStrLn "  --- 2. After round-trip via PKGDB:"
    print $ pPrint extradef'
execute opts db (EmitExtraDefn mattr) = do
  attrs ← case mattr of
            Just attr → pure $ Set.singleton attr
            Nothing   → PKGDB.listExtraDefs db
  forM_ attrs $
    \attr→ do
      extradef ← PKGDB.readExtraDefn db attr
      print $ nest 2 $ vcat
        [ text (unpack $ fromAttr attr) <+> equals
        , pPrint extradef <> semi ]
      -- print $ text (unpack $ fromAttr attr) <+> equals <+> pPrint extradef


internDerivation ∷ Derivation → SrcSpec → ExtraDefn
internDerivation drv sspec =
  let SSGithub{..} = case sspec of
                       x@SSGithub{..} → x
                       _ → error "Non-Github imports not supported."
      drvActiveFields    = Proj.piecewiseDerivation drv
      edAttr             = ssAttr
      meRepoName         = if fromRepo ssRepo ≢ fromAttr ssAttr
                           then Just ssRepo
                           else Nothing
      meDisable          = KeepOverride
      meChdir            = if Proj.fieldActive drv DFsubpath
                           then Just (T.pack $ drv^.subpath) else Nothing
      meEssentialRevDeps = []      -- initially not tracked
      meAttrName         = Nothing -- only applies to versioned Nixpkgs attrs
      edMeta             = Meta{..}
      ghRepoName         = ssRepo
      ghUpstream         = ssUser
      ghUser             = Nothing -- XXX: assumption
      ghGitRef           = GitRef $ pack $ derivRevision $ drv^.src
      ghNixHash          = NixHash $ pack $ derivHash $ drv^.src
      ghPR               = Nothing
      ghIssue            = Nothing
      ghTimestamp        = Nothing -- XXX: loss
      edGithub           = Github{..}
      edDrvFields        = Map.fromList $ flip filter drvActiveFields $
        \(k, _)→ not $ Set.member k Proj.drvNonPassthroughFields
  in ExtraDefn{..}


getDerivation ∷ Options → SrcSpec → IO Derivation
getDerivation Options{..} sspec = do
  let optHpack           = False
      optHackageDb       = Nothing
      optHackageSnapshot = Nothing
      optUrl             = T.unpack $ fromURL $ attrC2NUrl sspec
      optRevision        = Nothing
      optSha256          = Nothing
      optSubpath         = T.unpack ∘ fromDir <$> ssDir sspec
      optSystem          = oSystem
      optCompiler        = oCompiler
      optExtraArgs       = []
      opts               = ImportOptions{..}
  package ← getPackage optHpack optHackageDb optHackageSnapshot $
            Source optUrl (fromMaybe "" optRevision) (maybe UnknownHash Guess optSha256) (fromMaybe "" optSubpath)
  pure $ packageDerivation opts package

data ImportOptions = ImportOptions
  { optCompiler    ∷ CompilerId
  , optSystem      ∷ Platform
  , optSubpath     ∷ Maybe FilePath
  , optExtraArgs   ∷ [String]
  }

packageDerivation ∷ ImportOptions → Package → Derivation
packageDerivation ImportOptions{..} pkg = do
  let
      withHpackOverrides :: Derivation -> Derivation
      withHpackOverrides = if pkgRanHpack pkg then hpackOverrides else id

      hpackOverrides :: Derivation -> Derivation
      hpackOverrides = over phaseOverrides (<> "preConfigure = \"hpack\";")
                       . set (libraryDepends . tool . contains (PP.pkg "hpack")) True

      flags :: FlagAssignment
      flags = normalizeCabalFlags $
                configureCabalFlags (packageId (pkgCabal pkg))

      deriv :: Derivation
      deriv = withHpackOverrides $ fromGenericPackageDescription (const True)
                                            (\i -> Just (binding # (i, path # [i])))
                                            optSystem
                                            (unknownCompilerInfo optCompiler NoAbiTag)
                                            flags
                                            []
                                            (pkgCabal pkg)
              & src .~ pkgSource pkg
              & subpath .~ fromMaybe "." optSubpath
              & extraFunctionArgs %~ Set.union (Set.fromList ("inherit stdenv":map (fromString . ("inherit " ++)) optExtraArgs))
  deriv
