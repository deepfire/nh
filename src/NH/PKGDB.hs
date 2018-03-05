{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.PKGDB
where

import           Control.Lens                        ((<&>))
import           Control.Monad                       (foldM, forM_)
import           Data.Hourglass                      (Seconds(..))
import           Data.Hourglass.Epoch                (ElapsedSince(..))
import           Data.Function                       ((&))
import qualified Data.List                        as L
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                         as Set
import           Data.Text                           (Text, pack, unpack)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as Sys
import           Prelude                      hiding (read)
import           Prelude.Unicode
import qualified System.Directory                 as Sys
import qualified System.FilePath                  as Sys
import           Text.Printf

import           Language.Nix.PrettyPrinting  hiding ((<>), empty, Text)
import           Text.PrettyPrint.HughesPJClass      ( Doc, Pretty(..), Style(..), Mode(..)
                                                     , renderStyle, fsep, text, sep, fsep, lbrack, rbrack, lbrace, rbrace, empty
                                                     , vcat, nest, doubleQuotes, (<+>), semi)
import qualified Text.Read                        as R

import           NH.Types
import           NH.Config
import           NH.FS
import qualified NH.Projection                    as Proj -- XXX: wacky dep
import           NH.Misc



data PKGDB = PKGDB
  { pkgdbPath ∷ Text
  }

open ∷ Text → IO (Maybe PKGDB)
open path = do
  valid ← foldM (\acc sub→ (acc ∧) <$> Sys.doesDirectoryExist (unpack $ path </> sub))
    True (typeSub <$> [ETOver, ETMeta, ETGithub, ETHackage, ETBuild])
  if valid
    then do -- printf "Found valid PKGDB at:  %s\n" $ unpack path
            pure $ Just $ PKGDB path
    else pure Nothing
  
data EType
  = ETPkg
  | ETOver
  | ETMeta
  | ETGithub
  | ETHackage
  | ETBuild
  | ETCache
  deriving (Eq, Show)

typeSub ∷ EType → Text
typeSub ETPkg     = "def/non-nixpkgs"
typeSub ETOver    = "def/over"
typeSub ETMeta    = "def/meta"
typeSub ETGithub  = "def/github"
typeSub ETHackage = "def/hackage"
typeSub ETBuild   = "build"
typeSub ETCache   = "cache"

path ∷ PKGDB → EType → Text → Field → Text
path PKGDB{..} ty at (Field fi) =
  pkgdbPath </> typeSub ty </> at <.> fi

pathDir ∷ PKGDB → EType → Text
pathDir PKGDB{..} ty =
  pkgdbPath </> typeSub ty

read ∷ PKGDB → EType → Text → Field → IO (Maybe Text)
read  db ty at fi = do
  let p = unpack $ path db ty at fi
  (∃) ← Sys.doesFileExist p
  if (∃)
    then Just <$> Sys.readFile p
    else do
    -- putStrLn $ "missing field: "<>show ty<>"/"<>unpack (fromField fi)<>" at " <>show p
    pure Nothing

has ∷ PKGDB → EType → Text → Field → IO Bool
has  db ty at fi = do
  let p = unpack $ path db ty at fi
  Sys.doesFileExist p

write ∷ PKGDB → EType → Text → Field → Maybe Text → IO ()
write db ty at fi mval = do
  let fpath   = unpack $ path db ty at fi
  fileExists ← Sys.doesFileExist fpath
  case (fileExists, mval) of
    (False, Nothing) → pure ()
    (True,  Nothing) → Sys.removeFile fpath
    (_,     Just v)  → do
      dirExists ← Sys.doesDirectoryExist $ unpack $ pathDir db ty
      if dirExists
        then Sys.writeFile fpath v
        else error $ "Malformed PKGDB: structural subdir doesn't exist: " <> (unpack $ pathDir db ty)

list ∷ PKGDB → EType → IO (Set Text)
list db ty = do
  let dir = pathDir db ty
  fulls ← (T.pack <$>) <$> Sys.listDirectory (unpack dir)
  let split = T.splitOn "." <$> fulls
      names = (!! 0) <$> split
  pure $ Set.fromList names

listExtraDefs ∷ PKGDB → IO (Set Attr)
listExtraDefs db = (Set.fromList ∘ (Attr <$>) ∘ Set.toList) <$> list db ETPkg
-- listExtraDefs db = (Attr <$>) <$> list db ETPkg



attrRepoName ∷ Attr → Meta → RepoName
attrRepoName (Attr name) Meta{..} = flip fromMaybe meRepoName $ RepoName name



extraDefnPassField ∷ ExtraDefn → DerivationField → Maybe Doc
extraDefnPassField ExtraDefn{..} fname = Map.lookup fname edDrvFields

extraDefnPassFieldOpt ∷ ExtraDefn → DerivationField → Doc
extraDefnPassFieldOpt ed fname = extraDefnPassField ed fname &
  fromMaybe empty

extraDefnPassFieldMand ∷ ExtraDefn → DerivationField → Doc
extraDefnPassFieldMand ed fname = extraDefnPassField ed fname &
  (errNothing $ printf "Missing extra definition passfield '%s'." $ show fname)

writeExtraDefn ∷ PKGDB → ExtraDefn → IO ()
writeExtraDefn db ExtraDefn{..} =
  writeMeta      db edAttr                edMeta >>
  writeGithub    db (ghRepoName edGithub) edGithub >>
  writeDrvFields db edAttr                edDrvFields
  where
    docText = T.pack ∘ renderStyle (Style OneLineMode 1 1)
    writeMeta db (Attr attr) Meta{..} = do
      write db ETMeta  attr "repoName" $ fromRepo <$> meRepoName
      write db ETMeta  attr "disable"  $ flagIf meDisable (Just "disable") Nothing
      write db ETMeta  attr "chdir"    $ meChdir
      write db ETMeta  attr "erdeps"   $ if not $ L.null meEssentialRevDeps
                                         then Just $ docText $ fsep $ text ∘ T.unpack ∘ fromAttr <$> meEssentialRevDeps
                                         else Nothing
      write db ETMeta  attr "attrName" $ fromAttr <$> meAttrName
    writeGithub db (RepoName repo) Github{..} = do
      write db ETGithub      repo "upstream"  $ Just $ fromUser $ ghUpstream
      write db ETGithub      repo "user"      $ Just $ fromUser $ fromMaybe ghUpstream ghUser
      write db ETGithub      repo "rev"       $ Just $ fromRef  $ ghGitRef
      write db ETGithub      repo "hash"      $ Just $ fromNixHash ghNixHash
      write db ETGithub      repo "pr"        $ fromPR    <$> ghPR
      write db ETGithub      repo "issue"     $ fromIssue <$> ghIssue
      write db ETGithub      repo "timestamp"   Nothing -- XXX
    writeDrvFields db (Attr attr) drvFields = do
      forM_ (Map.toList drvFields) $
        \(field, content) → do
          write db ETPkg     attr (Field $ Proj.drvFieldNixName field) $ Just $ docText content

readExtraDefn ∷ PKGDB → Attr → IO ExtraDefn
readExtraDefn db attr = do
  let readMeta   db (Attr attr) = do
        meRepoName         ← (RepoName <$>) <$> read db ETMeta attr "repoName"
        meDisable          ← fromBool       <$> has  db ETMeta attr "disable"
        meChdir            ←                    read db ETMeta attr "chdir"
        meEssentialRevDeps ← (Attr <$>) ∘ parseSequence ∘ fromMaybe ""
                                            <$> read db ETMeta attr "erdeps"
        meAttrName         ← (Attr     <$>) <$> read db ETMeta attr "attrName" -- for versioned shadow attributes
        pure Meta{..}
      readGithub db ghRepoName@(RepoName repo) = do
        let ghUser = Nothing
        ghUpstream         ← read db ETGithub repo "upstream" <&>
                             GithubUser ∘ fromMaybe (error $ printf "Missing field '%s' in repo '%s'." ("upstream"∷String) $ unpack repo)
        ghUser             ← read db ETGithub repo "user" <&>
                             \case
                               Nothing   → Nothing
                               Just user → if user ≡ fromUser ghUpstream
                                           then Nothing
                                           else Just $ GithubUser user
        ghGitRef           ← read db ETGithub repo "rev" <&>
                             GitRef ∘ fromMaybe (error $ printf "Missing field '%s' in repo '%s'." ("rev"∷String) $ unpack repo)
        ghNixHash          ← read db ETGithub repo "hash" <&>
                             NixHash ∘ fromMaybe (error $ printf "Missing field '%s' in repo '%s'." ("hash"∷String) $ unpack repo)
        ghPR               ← (GithubPR    <$>) <$> read db ETGithub repo "pr"
        ghIssue            ← (GithubIssue <$>) <$> read db ETGithub repo "issue"
        ghTimestamp        ← (ElapsedSince ∘ Seconds ∘ R.read ∘ unpack <$>) <$> read db ETGithub repo "timestamp"
        pure Github{..}
      parseAttributes ∷ Text → [Attr]
      parseAttributes raw = Attr <$> L.delete "" (T.splitOn " " raw)
      parseStrings    ∷ Text → [String]
      parseStrings    raw = parseSequence raw
      edAttr  = attr
  edMeta      ← readMeta        db attr
  edGithub    ← readGithub      db (attrRepoName attr edMeta)
  let readField db (Attr attr) field = do
        ((,) field ∘ parseFieldTyped (drvFieldType field) <$>) <$> read db ETPkg attr (Field $ Proj.drvFieldNixName field)
      parseFieldTyped   (NTList NTVar) raw = sep [ fsep $ text ∘ unpack ∘ fromAttr <$> parseAttributes raw ]
      parseFieldTyped   (NTList NTStr) raw = sep [ lbrack
                                                 , fsep $ text <$> parseStrings raw
                                                 , rbrack ]
      parseFieldTyped t@(NTList _)     _   = error $ printf "Unsupported list type: %s" (show t)
      parseFieldTyped _                raw = text $ unpack raw
  edDrvFields ← Map.fromList ∘ catMaybes <$> mapM (readField db attr) (Set.toList Proj.drvPassthroughFields)
  -- putStrLn $ "read back fields:  " <> (show $ Map.toList edDrvFields)
  pure $ ExtraDefn{..}

maybeAttr ∷ Field → Maybe Doc → Doc
maybeAttr _ Nothing = empty
maybeAttr (Field field) (Just doc)  = attr (unpack field) doc

maybeAttr' ∷ Field → Maybe Doc → Doc
maybeAttr' _ Nothing = empty
maybeAttr' (Field field) (Just doc) = vcat
  [ text (unpack field) <+> equals <+> lbrack
  , nest 2 doc
  , rbrack <> semi ]
  
instance Pretty ExtraDefn where
  pPrint ed@(ExtraDefn attrib meta@Meta{..} Github{..} fields) =
    vcat
    [ text "with pkgs;" <+> text "with self;" <+> text "mkDerivation" <+> lbrace
    , text " "
    , nest 2 $ vcat
      [ attr "pname"   $ string $ unpack $ fromAttr attrib
      , attr "version" $ extraDefnPassFieldMand ed DFversion
      , text "src" <+> equals <+> text "fetchFromGitHub" <+> lbrace
           , nest 2 $ vcat
             [ attr "owner"  $ string $ unpack (fromUser    ghUpstream)
             , attr "repo"   $ string $ unpack (fromRepo $ attrRepoName attrib meta)
             , attr "rev"    $ string $ unpack (fromRef     ghGitRef)
             , attr "sha256" $ string $ unpack (fromNixHash ghNixHash)
             ]
      , rbrace <> semi
      , maybeAttr "postUnpack" (meChdir <&> (\cd→ string ("sourceRoot+=/" <> unpack cd <> "; echo source root reset to $sourceRoot")))
      , maybeAttr "configureFlags"               $ extraDefnPassField ed DFconfigureFlags
      , maybeAttr "isLibrary"                    $ extraDefnPassField ed DFisLibrary
      , maybeAttr "isExecutable"                 $ extraDefnPassField ed DFisExecutable
      , maybeAttr "enableSeparateDataOutput"     $ extraDefnPassField ed DFenableSeparateDataOutput

      , maybeAttr' "setupHaskellDepends"         $ extraDefnPassField ed DFsetupHaskellDepends     
      , maybeAttr' "libraryHaskellDepends"       $ extraDefnPassField ed DFlibraryHaskellDepends   
      , maybeAttr' "executableHaskellDepends"    $ extraDefnPassField ed DFexecutableHaskellDepends
      , maybeAttr' "testHaskellDepends"          $ extraDefnPassField ed DFtestHaskellDepends      
      , maybeAttr' "benchmarkHaskellDepends"     $ extraDefnPassField ed DFbenchmarkHaskellDepends 

      , maybeAttr' "setupSystemDepends"          $ extraDefnPassField ed DFsetupSystemDepends     
      , maybeAttr' "librarySystemDepends"        $ extraDefnPassField ed DFlibrarySystemDepends   
      , maybeAttr' "executableSystemDepends"     $ extraDefnPassField ed DFexecutableSystemDepends
      , maybeAttr' "testSystemDepends"           $ extraDefnPassField ed DFtestSystemDepends      
      , maybeAttr' "benchmarkSystemDepends"      $ extraDefnPassField ed DFbenchmarkSystemDepends 

      , maybeAttr' "setupPkgconfigDepends"       $ extraDefnPassField ed DFsetupPkgconfigDepends     
      , maybeAttr' "libraryPkgconfigDepends"     $ extraDefnPassField ed DFlibraryPkgconfigDepends   
      , maybeAttr' "executablePkgconfigDepends"  $ extraDefnPassField ed DFexecutablePkgconfigDepends
      , maybeAttr' "testPkgconfigDepends"        $ extraDefnPassField ed DFtestPkgconfigDepends      
      , maybeAttr' "benchmarkPkgconfigDepends"   $ extraDefnPassField ed DFbenchmarkPkgconfigDepends 

      , maybeAttr' "setupToolDepends"            $ extraDefnPassField ed DFsetupToolDepends     
      , maybeAttr' "libraryToolDepends"          $ extraDefnPassField ed DFlibraryToolDepends   
      , maybeAttr' "executableToolDepends"       $ extraDefnPassField ed DFexecutableToolDepends
      , maybeAttr' "testToolDepends"             $ extraDefnPassField ed DFtestToolDepends      
      , maybeAttr' "benchmarkToolDepends"        $ extraDefnPassField ed DFbenchmarkToolDepends 
      
      , maybeAttr "enableLibraryProfiling"       $ extraDefnPassField ed DFenableLibraryProfiling
      , maybeAttr "enableExecutableProfiling"    $ extraDefnPassField ed DFenableExecutableProfiling
      , maybeAttr "enableSplitObjs"              $ extraDefnPassField ed DFenableSplitObjs
      , maybeAttr "doHaddock"                    $ Nothing -- Over{..}
      , maybeAttr "jailbreak"                    $ Nothing -- Over{..}
      , maybeAttr "doCheck"                      $ Nothing -- Over{..}
      , maybeAttr "testTarget"                   $ extraDefnPassField ed DFtestTarget
      , maybeAttr "hyperlinkSource"              $ extraDefnPassField ed DFhyperlinkSource
      -- XXX: not really sure how to handle this
      -- , maybeAttr "phaseOverrides"              $ (vcat ∘ (map text . lines) <$> extraDefnPassField ed DFphaseOverrides)
      , maybeAttr "homepage"    $ extraDefnPassField     ed DFmetaSectionHomepage
      , maybeAttr "description" $ extraDefnPassField     ed DFmetaSectionDescription
      , attr      "license"     $ extraDefnPassFieldMand ed DFmetaSectionLicense
      , maybeAttr "platforms"   $ extraDefnPassField     ed DFmetaSectionPlatforms
      , maybeAttr "maintainers" $ extraDefnPassField     ed DFmetaSectionMaintainers
      ]
    , rbrace
    ]
