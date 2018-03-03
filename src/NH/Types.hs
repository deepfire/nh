{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.Types
where

import           Data.Hourglass.Epoch
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import           Data.String
import           Data.Text
import           Data.Semigroup
import           Prelude.Unicode
import           Text.PrettyPrint.HughesPJClass      (Doc(..))



newtype Attr                    = Attr                    { fromAttr              ∷ Text } deriving (Eq, IsString, Ord, Show)
newtype Field                   = Field                   { fromField             ∷ Text } deriving (Eq, IsString, Show)

newtype GHCVer                  = GHCVer                  { fromGHCVer            ∷ Text } deriving (Eq, IsString, Show)

newtype GitRef                  = GitRef                  { fromRef               ∷ Text } deriving (Eq, IsString, Show)
newtype GithubUser              = GithubUser              { fromUser              ∷ Text } deriving (Eq, IsString, Show)
newtype GithubPR                = GithubPR                { fromPR                ∷ Text } deriving (Eq, IsString, Show)
newtype GithubIssue             = GithubIssue             { fromIssue             ∷ Text } deriving (Eq, IsString, Show)
newtype RepoName                = RepoName                { fromRepo              ∷ Text } deriving (Eq, IsString, Show)
newtype Subdir                  = Subdir                  { fromDir               ∷ Text } deriving (Eq, IsString, Show)

newtype NixHash                 = NixHash                 { fromNixHash           ∷ Text } deriving (Eq, IsString, Show)

newtype URL                     = URL                     { fromURL               ∷ Text } deriving (Eq, IsString, Show)


-- * Flag machinery
class (Bounded a, Eq a) ⇒ Flag a where
  toBool ∷ a → Bool
  toBool = (≡ enabled)
  fromBool ∷ Bool → a
  fromBool x = if x then minBound else maxBound
  enabled, disabled ∷ a
  enabled  = minBound
  disabled = maxBound
  opposite ∷ a → a
  opposite = fromBool . not . toBool
  flagIf ∷ a → b → b → b
  flagIf f true false = if toBool f then true else false

-- flag ∷ Flag a ⇒ a → ArgName → Char → Optional HelpMessage → Parser a
-- flag effect long ch help = (\case
--                                True  → effect
--                                False → opposite effect) <$> switch long ch help



data SrcSpec
  = SSGithub  { ssAttr ∷ Attr, ssUser ∷ GithubUser, ssRepo ∷ RepoName, ssDir ∷ (Maybe Subdir), ssRef ∷ GitRef }
  | SSHackage { ssAttr ∷ Attr,                                         ssDir ∷ (Maybe Subdir) }
  deriving (Show)



data NixType
  = NTStr
  | NTPath
  | NTBool
  | NTInt
  | NTVar
  | NTList NixType
  | NTAttrset (Map Text NixType)
  | NTFunction [NixType] NixType
  deriving (Eq, Ord, Show)



drvFieldType ∷ DerivationField → NixType
drvFieldType DFinputs = NTList NTVar
drvFieldType DFpname = NTStr
drvFieldType DFversion = NTStr
drvFieldType DFsrcUrl = NTStr
drvFieldType DFsrcSha256 = NTStr -- nixhash
drvFieldType DFsrcRev = NTStr
drvFieldType DFsubpath = NTStr
drvFieldType DFrevision = NTInt
drvFieldType DFeditedCabalFile = NTStr -- nixhash
drvFieldType DFconfigureFlags = NTList NTStr
drvFieldType DFisLibrary = NTBool
drvFieldType DFisExecutable = NTBool
drvFieldType DFenableSeparateDataOutput = NTBool
drvFieldType _ = NTList NTVar

data DerivationField
  = DFinputs
  | DFpname
  | DFversion
  | DFsrcUrl
  | DFsrcSha256
  | DFsrcRev
  | DFsubpath
  | DFrevision
  | DFeditedCabalFile

  | DFconfigureFlags
  | DFisLibrary
  | DFisExecutable
  | DFenableSeparateDataOutput

  | DFsetupHaskellDepends
  | DFlibraryHaskellDepends
  | DFexecutableHaskellDepends
  | DFtestHaskellDepends
  | DFbenchmarkHaskellDepends

  | DFsetupSystemDepends
  | DFlibrarySystemDepends
  | DFexecutableSystemDepends
  | DFtestSystemDepends
  | DFbenchmarkSystemDepends

  | DFsetupPkgconfigDepends
  | DFlibraryPkgconfigDepends
  | DFexecutablePkgconfigDepends
  | DFtestPkgconfigDepends
  | DFbenchmarkPkgconfigDepends

  | DFsetupToolDepends
  | DFlibraryToolDepends
  | DFexecutableToolDepends
  | DFtestToolDepends
  | DFbenchmarkToolDepends

  | DFenableLibraryProfiling
  | DFenableExecutableProfiling
  | DFenableSplitObjs
  | DFdoHaddock
  | DFjailbreak
  | DFdoCheck
  | DFtestTarget
  | DFhyperlinkSource
  | DFphaseOverrides

  | DFmetaSectionHomepage
  | DFmetaSectionDescription
  | DFmetaSectionLicense
  | DFmetaSectionPlatforms
  | DFmetaSectionMaintainers
  -- | DFmetaSectionLongDescription
  deriving (Bounded, Enum, Eq, Ord, Show)



data DisableOverride = DisableOverride         | KeepOverride     deriving (Bounded, Eq, Ord, Show); instance Flag DisableOverride

data Meta = Meta
  { meRepoName         ∷ Maybe RepoName
  , meDisable          ∷ DisableOverride
  , meChdir            ∷ Maybe Text
  , meEssentialRevDeps ∷ [Attr]
  , meAttrName         ∷ Maybe Attr -- for versioned shadow attributes
  -- XXX: how to map attr.*.explanation ?
  } deriving (Eq, Show)

data Github = Github
  { ghRepoName         ∷ RepoName
  , ghUpstream         ∷ GithubUser
  , ghUser             ∷ Maybe GithubUser
  , ghGitRef           ∷ GitRef
  , ghNixHash          ∷ NixHash
  , ghPR               ∷ Maybe GithubPR
  , ghIssue            ∷ Maybe GithubIssue
  , ghTimestamp        ∷ Maybe (ElapsedSince UnixEpoch)
  } deriving (Eq, Show)

data ExtraDefn = ExtraDefn
  { edAttr             ∷ Attr
  , edMeta             ∷ Meta
  , edGithub           ∷ Github
  , edDrvFields        ∷ Map DerivationField Doc
  } deriving (Eq, Show)
