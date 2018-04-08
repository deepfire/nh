{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module NH.Types
where

import           Data.Coerce                         (Coercible, coerce)
import           Data.Hourglass.Epoch
import           Data.Map                            (Map)
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           Data.String
import           Data.Text
import           Data.Semigroup               hiding (All)
import           Generics.SOP                        (Proxy)
import qualified Generics.SOP                     as SOP
import qualified GHC.Generics                     as GHC
import           Prelude                      hiding (length, drop)
import           Prelude.Unicode
import           Text.PrettyPrint.HughesPJClass      (Doc(..))

import qualified Debug.Trace                      as DBG

-- * Local
import           NH.Misc



instance Functor Set.Set where
  fmap = Set.mapMonotonic



newtype CtxName                 = CtxName                 { unCtxName             ∷ Text } deriving (Eq, IsString, Ord, Show)

attrCtx ∷ Attr     → CtxName
repoCtx ∷ RepoName → CtxName
attrCtx = CtxName ∘ coerce
repoCtx = CtxName ∘ coerce

newtype Field                   = Field                   { fromField             ∷ Text } deriving (Eq, IsString, Ord, Show, MapKey)

newtype GHCVer                  = GHCVer                  { fromGHCVer            ∷ Text } deriving (Eq, IsString, Show)
newtype Subdir                  = Subdir                  { fromDir               ∷ Text } deriving (Eq, IsString, Show)
newtype URL                     = URL                     { fromURL               ∷ Text } deriving (Eq, IsString, Show)



-- * Stored in PKGDB

newtype Attr                    = Attr                    { fromAttr              ∷ Text } deriving (Eq, IsString, Ord, Show, MapKey)
newtype RepoName                = RepoName                { fromRepoName          ∷ Text } deriving (Eq, IsString, Ord, Show)
newtype Release                 = Release                 { fromRelease           ∷ Text } deriving (Eq, IsString, Show)
newtype GitRef                  = GitRef                  { fromRef               ∷ Text } deriving (Eq, IsString, Show)
newtype GithubUser              = GithubUser              { fromUser              ∷ Text } deriving (Eq, IsString, Show)
newtype GithubPR                = GithubPR                { fromPR                ∷ Text } deriving (Eq, IsString, Show)
newtype GithubIssue             = GithubIssue             { fromIssue             ∷ Text } deriving (Eq, IsString, Show)
newtype NixHash                 = NixHash                 { fromNixHash           ∷ Text } deriving (Eq, IsString, Show)


data Flags
  = Local
  | Disable
  | Jailbreak
  | Revision
  | Check
  | Haddock
  | Target

instance CFlag Local          where
  data    Flag Local          = ToLocal         | ToNixpkgs     deriving (Bounded, Eq, Ord, Show)
instance CFlag Disable        where
  data    Flag Disable        = DisableOverride | KeepOverride  deriving (Bounded, Eq, Ord, Show)
instance CFlag Jailbreak      where
  data    Flag Jailbreak      = DoJailbreak     | DontJailbreak deriving (Bounded, Eq, Ord, Show)
instance CFlag Revision       where
  data    Flag Revision       = DoRevision      | KeepRevision  deriving (Bounded, Eq, Ord, Show)
instance CFlag Check          where
  data    Flag Check          = DoCheck         | DontCheck     deriving (Bounded, Eq, Ord, Show)
instance CFlag Haddock        where
  data    Flag Haddock        = DoHaddock       | DontHaddock   deriving (Bounded, Eq, Ord, Show)

-- * Flag machinery
class (Bounded (Flag a), Eq (Flag a)) ⇒ CFlag (a ∷ Flags) where
  data Flag a
  toBool ∷ (Flag a) → Bool
  toBool = (≡ enabled)
  fromBool ∷ Bool → (Flag a)
  fromBool x = if x then minBound else maxBound
  enabled, disabled ∷ (Flag a)
  enabled  = minBound
  disabled = maxBound
  opposite ∷ Flag a → Flag a
  opposite = fromBool . not . toBool
  flagIf ∷ (Flag a) → b → b → b
  flagIf f true false = if toBool f then true else false
  -- XXX: most should be de-TC-ised,
  -- however, using TC's as poor-man's modules is so alluring..

enabledIsJust ∷ CFlag b ⇒ a → Flag b → Maybe a
enabledIsJust x (toBool → True) = Just x
enabledIsJust _ _               = Nothing

-- flag ∷ Flag a ⇒ a → ArgName → Char → Optional HelpMessage → Parser a
-- flag effect long ch help = (\case
--                                True  → effect
--                                False → opposite effect) <$> switch long ch help



data SrcSpec
  = SSGithub  { ssAttr ∷ Attr, ssUser ∷ GithubUser, ssRepoName ∷ RepoName, ssDir ∷ (Maybe Subdir), ssRef ∷ GitRef }
  | SSHackage { ssAttr ∷ Attr,                                             ssDir ∷ (Maybe Subdir) }
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


-- Global TODO:
-- 
-- 1. We're writing doCheck and doHaddock -- writing might be ok, per se
--    ..but how do we know, when it comes to emission, whether to emit or not?
--    Anyhow, better have a handle on whether to write.
--    ..and 'fieldActive' already serves that role.
--    Could be driven by significance of defaults, I guess.
-- 2. db conversion
--
data Package = Package
  { pkAttr             ∷ Attr
  , pkRepo             ∷ Maybe GithubRepo
  , pkMeta             ∷ Meta
  , pkOver             ∷ Overrides        -- ^ Carries overridable fields
  , pkDrvFields        ∷ Map DrvField DFValue -- ^ Non-overridable fields only
  , pkDrvMeta          ∷ DrvMeta
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Package
instance SOP.HasDatatypeInfo Package

data DrvMeta = DrvMeta
  { dmLicense          ∷ Text
  , dmDescription      ∷ Maybe Text
  , dmHomepage         ∷ Maybe Text
  , dmPlatforms        ∷ Maybe Text
  , dmMaintainers      ∷ Maybe Text
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         DrvMeta
instance SOP.HasDatatypeInfo DrvMeta

data GithubRepo = GithubRepo
  { grRepoName         ∷ RepoName
  , grUpstream         ∷ GithubUser
  , grPr               ∷ Maybe GithubPR
  , grIssue            ∷ Maybe GithubIssue
  , grTimestamp        ∷ Maybe (ElapsedSince UnixEpoch)
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         GithubRepo
instance SOP.HasDatatypeInfo GithubRepo



type EName = Text

data Meta = Meta
  { meRepoName         ∷ Maybe RepoName
  , meDisable          ∷ Flag Disable
  , meChdir            ∷ Maybe Text
  , meErdeps           ∷ [Attr]
  , meAttrName         ∷ Maybe Attr        -- ^ Useful for versioned shadow attributes.
  , meLocal            ∷ Flag Local
  , meExplanation      ∷ Map Field Text
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Meta
instance SOP.HasDatatypeInfo Meta

data Src
  = Hackage
    { srNixHash          ∷ NixHash
    , haRelease          ∷ Release
    }
  | Github
    { srNixHash          ∷ NixHash
    , ghRepoName         ∷ RepoName
    , ghUser             ∷ GithubUser
    , ghRev              ∷ GitRef
    }
  deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Src
instance SOP.HasDatatypeInfo Src

data Overrides = Overrides
  { ovSrc              ∷ Maybe Src
  , ovJailbreak        ∷ Flag Jailbreak
  , ovRevision         ∷ Flag Revision
  , ovDoCheck          ∷ Flag Check
  , ovDoHaddock        ∷ Flag Haddock
  , ovInputs           ∷ Map Attr Attr
  , ovDrvFields        ∷ Map DrvField DFValue -- ^ Overridable fields only
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Overrides
instance SOP.HasDatatypeInfo Overrides

instance Semigroup Overrides where
  l <> r = Overrides
    { ovSrc              =                  ovSrc       r
    , ovJailbreak        =                  ovJailbreak r
    , ovRevision         =                  ovRevision  r
    , ovDoCheck          =                  ovDoCheck   r
    , ovDoHaddock        =                  ovDoHaddock r
    , ovInputs           = ovInputs    l <> ovInputs    r
    , ovDrvFields        = ovDrvFields l <> ovDrvFields r
    }

instance Monoid Overrides where
  mempty = Overrides
    { ovSrc              = Nothing
    , ovJailbreak        = DontJailbreak
    , ovRevision         = KeepRevision
    , ovDoCheck          = DoCheck
    , ovDoHaddock        = DoHaddock
    , ovInputs           = Map.empty
    , ovDrvFields        = Map.empty
    }



data Status
  = StFulldefn
  | StShadowed
  | StHackaged
  | StUpstreamed
  | StUnmerged
  | StConfig
  | StDefault



data DrvField
  = DFdrvparams
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
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

class MapKey a where
  toKeyName           ∷                    a → Text
  fromKeyName         ∷                    Text → a
  default   toKeyName ∷ Coercible a Text ⇒ a → Text
  default fromKeyName ∷ Coercible Text a ⇒ Text → a
  toKeyName   = coerce
  fromKeyName = coerce

instance MapKey Text

instance MapKey DrvField where
  toKeyName   = drop 2 ∘ showT
  fromKeyName = read ∘ ("DF"<>) ∘ unpack

-- | Consider possibilities for making this type redundant.
data DFValue = DFValue
  { dfField ∷ DrvField
  , dfDoc   ∷ Doc
  } deriving (Eq, Show)
