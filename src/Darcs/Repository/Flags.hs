module Darcs.Repository.Flags
    (
      Compression (..)
    , RemoteDarcs (..)
    , remoteDarcs
    , Reorder (..)
    , Verbosity (..)
    , UpdatePending (..)
    , UseCache (..)
    , DryRun (..)
    , UMask (..)
    , LookForAdds (..)
    , LookForReplaces (..)
    , DiffAlgorithm (..)
    , LookForMoves (..)
    , RunTest (..)
    , SetScriptsExecutable (..)
    , LeaveTestDir (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , InheritDefault (..)
    , UseIndex (..)
    , ScanKnown (..)
    , CloneKind (..)
    , AllowConflicts (..)
    , ExternalMerge (..)
    , WorkRepo (..)
    , WantGuiPause (..)
    , WithPatchIndex (..)
    , WithWorkingDir (..)
    , ForgetParent (..)
    , PatchFormat (..)
    , IncludeBoring (..)
    , HooksConfig (..)
    , HookConfig (..)
    ) where

import Darcs.Prelude

import Darcs.Util.Diff ( DiffAlgorithm(..) )
import Darcs.Util.Global ( defaultRemoteDarcsCmd )


data Verbosity = Quiet | NormalVerbosity | Verbose
    deriving ( Eq, Show )

data Compression = NoCompression
                 | GzipCompression
    deriving ( Eq, Show )

data WithPatchIndex = YesPatchIndex | NoPatchIndex
    deriving ( Eq, Show )

data RemoteDarcs = RemoteDarcs String
                 | DefaultRemoteDarcs
    deriving ( Eq, Show )

remoteDarcs :: RemoteDarcs -> String
remoteDarcs DefaultRemoteDarcs = defaultRemoteDarcsCmd
remoteDarcs (RemoteDarcs x) = x

data Reorder = NoReorder | Reorder
    deriving ( Eq )

data UpdatePending = YesUpdatePending | NoUpdatePending
    deriving ( Eq, Show )

data UseCache = YesUseCache | NoUseCache
    deriving ( Eq, Show )

data DryRun = YesDryRun | NoDryRun
    deriving ( Eq, Show )

data UMask = YesUMask String | NoUMask
    deriving ( Eq, Show )

data LookForAdds = YesLookForAdds | NoLookForAdds
    deriving ( Eq, Show )

data LookForReplaces = YesLookForReplaces | NoLookForReplaces
    deriving ( Eq, Show )

data LookForMoves = YesLookForMoves | NoLookForMoves
    deriving ( Eq, Show )

data IncludeBoring = YesIncludeBoring | NoIncludeBoring
    deriving ( Eq, Show )

data RunTest = YesRunTest | NoRunTest
    deriving ( Eq, Show )

data SetScriptsExecutable = YesSetScriptsExecutable | NoSetScriptsExecutable
    deriving ( Eq, Show )

data LeaveTestDir = YesLeaveTestDir | NoLeaveTestDir
    deriving ( Eq, Show )

data RemoteRepos = RemoteRepos [String]
    deriving ( Eq, Show )

data SetDefault = YesSetDefault Bool | NoSetDefault Bool
    deriving ( Eq, Show )

data InheritDefault = YesInheritDefault | NoInheritDefault
    deriving ( Eq, Show )

data UseIndex = UseIndex | IgnoreIndex deriving ( Eq, Show )

data ScanKnown = ScanKnown -- ^Just files already known to darcs
               | ScanAll -- ^All files, i.e. look for new ones
               | ScanBoring -- ^All files, even boring ones
    deriving ( Eq, Show )

-- Various kinds of getting repositories
data CloneKind = LazyClone       -- ^Just copy pristine and inventories
               | NormalClone     -- ^First do a lazy clone then copy everything
               | CompleteClone   -- ^Same as Normal but omit telling user they can interrumpt
    deriving ( Eq, Show )

data AllowConflicts = NoAllowConflicts | YesAllowConflicts | YesAllowConflictsAndMark
    deriving ( Eq, Show )

data ExternalMerge = YesExternalMerge String | NoExternalMerge
    deriving ( Eq, Show )

data WorkRepo = WorkRepoDir String | WorkRepoPossibleURL String | WorkRepoCurrentDir
    deriving ( Eq, Show )

data WantGuiPause = YesWantGuiPause | NoWantGuiPause
    deriving ( Eq, Show )

data WithWorkingDir = WithWorkingDir | NoWorkingDir
    deriving ( Eq, Show )

data ForgetParent = YesForgetParent | NoForgetParent
    deriving ( Eq, Show )

data PatchFormat = PatchFormat1 | PatchFormat2 | PatchFormat3
    deriving ( Eq, Show )

data HooksConfig = HooksConfig
  { pre :: HookConfig
  , post :: HookConfig
  }

data HookConfig = HookConfig
  { cmd :: Maybe String
  , prompt :: Bool
  }
