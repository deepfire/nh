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
import           Data.Maybe
import qualified Data.Set                         as Set
import           Data.String
import           Data.Text
import           Data.Semigroup               hiding (All)
import           Generics.SOP                        (Proxy)
import qualified Generics.SOP                     as SOP
import qualified GHC.Generics                     as GHC
import qualified GHC.Types                        as Type
import           Prelude                      hiding (length, drop)
import           Prelude.Unicode
import           Text.PrettyPrint.HughesPJClass      (Doc(..))

import qualified Debug.Trace                      as DBG

import           NH.MRecord

-- * Local
import           NH.Misc



instance Functor Set.Set where
  fmap = Set.mapMonotonic



newtype CtxName                 = CtxName                 { unCtxName             ∷ Text } deriving (Eq, IsString, Ord, Show)
newtype PKGDBPath               = PKGDBPath               { fromPKGDBPath         ∷ Text } deriving (Eq, IsString, Ord, Show)

attrCtx ∷ Attr     → CtxName
repoCtx ∷ RepoName → CtxName
attrCtx = CtxName ∘ coerce
repoCtx = CtxName ∘ coerce

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
  data    Flag Revision       = DontRevision    | KeepRevision  deriving (Bounded, Eq, Ord, Show)
instance CFlag Check          where
  data    Flag Check          = DontCheck       | DoCheck       deriving (Bounded, Eq, Ord, Show)
instance CFlag Haddock        where
  data    Flag Haddock        = DontHaddock     | DoHaddock     deriving (Bounded, Eq, Ord, Show)



data SrcSpec
  = SSGithub  { ssAttr ∷ Attr, ssUser ∷ GithubUser, ssRepoName ∷ RepoName, ssDir ∷ (Maybe Subdir), ssRef ∷ GitRef }
  | SSHackage { ssAttr ∷ Attr,                                             ssDir ∷ (Maybe Subdir) }
  deriving (Show)


-- Global TODO:
-- 
-- 1. Anomalous emission for inverted flags, like doCheck and doHaddock.
--    Could be driven by significance of defaults, I guess.
-- 2. db conversion
--
data Nixpkgs = Nixpkgs
  { nixpkgsPath            ∷ Text
  , nixpkgsHackagePackages ∷ Set.Set Attr
  } deriving (Eq)

instance Show Nixpkgs where
  show Nixpkgs{..} = "#<NIXPKGS \""<>unpack nixpkgsPath<>"\">"

-- | Context type for PKGDB-oriented MRecord instances:
type PKGCtx = (PKGDB, EName)

newtype CName = CName { fromCName ∷ Text } deriving (Eq, Ord, Show)

type instance ConsCtx PKGCtx = CName

newtype GHCConfStatic = GHCConfStatic { fromGHCConfStatic ∷ Text } deriving (Eq, Ord, Show)

data PKGDB = PKGDB
  { pkgdbPath          ∷ PKGDBPath
  , pkgdbNixpkgs       ∷ Nixpkgs
  , pkgdbGHCConfStatic ∷ GHCConfStatic
  -- , pkgdbExtraAttrs    ∷ [Attr]
  } deriving (GHC.Generic, Show)
instance SOP.Generic         PKGDB
instance SOP.HasDatatypeInfo PKGDB

data OverPackage = OverPackage
  { opAttr             ∷ Attr
  , opMeta             ∷ Meta
  , opOver             ∷ Overrides
  , opNixpkgs          ∷ Nixpkgs
  , opUpstream         ∷ Maybe Upstream
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         OverPackage
instance SOP.HasDatatypeInfo OverPackage

data Package = Package
  { pkAttr             ∷ Attr
  , pkUpstream         ∷ Upstream
  , pkMeta             ∷ Meta
  , pkOver             ∷ Overrides            -- ^ Carries overridable fields
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

data Upstream = Upstream
  { upRepoName         ∷ RepoName
  , upUser             ∷ GithubUser
  , upPr               ∷ Maybe GithubPR
  , upIssue            ∷ Maybe GithubIssue
  , upTimestamp        ∷ Maybe (ElapsedSince UnixEpoch)
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Upstream
instance SOP.HasDatatypeInfo Upstream



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
  , ovPatches          ∷ [Patch]
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
    , ovPatches          = ovPatches   l <> ovPatches   r
    }

instance Monoid Overrides where
  mempty = Overrides
    { ovSrc              = Nothing
    , ovJailbreak        = DontJailbreak
    , ovRevision         = KeepRevision
    , ovDoCheck          = DoCheck
    , ovDoHaddock        = DoHaddock
    , ovInputs           = mempty
    , ovDrvFields        = mempty
    , ovPatches          = []
    }

data Patch = Patch
  { paUrl    ∷ Text
  , paSha256 ∷ Text
  } deriving (Eq, GHC.Generic, Show)
instance SOP.Generic         Patch
instance SOP.HasDatatypeInfo Patch



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
