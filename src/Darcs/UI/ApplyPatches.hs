module Darcs.UI.ApplyPatches
    ( PatchApplier(..)
    , PatchProxy(..)
    , StandardPatchApplier(..)
    , applyPatchesStart
    , applyPatchesFinish
    ) where

import Darcs.Prelude

import Control.Monad ( when, void )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.UI.Commands
    ( putVerbose
    , putFinished
    , setEnvDarcsPatches
    )
import Darcs.UI.Commands.Util ( printDryRunMessageAndExit )
import Darcs.UI.Flags
    ( DarcsFlag, verbosity, compress, reorder, allowConflicts, externalMerge
    , wantGuiPause, diffingOpts, setScriptsExecutable, isInteractive, testChanges
    , xmlOutput, dryRun
    )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Options ( (?) )
import Darcs.UI.Commands.Util ( testTentativeAndMaybeExit )
import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.Repository
    ( Repository
    , tentativelyMergePatches
    , finalizeRepositoryChanges
    , applyToWorking
    , invalidateIndex
    , setScriptsExecutablePatches
    )
import Darcs.Repository.Job ( RepoJob(RepoJob) )
import Darcs.Patch ( RepoPatch, RepoType, IsRepoType, description )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Patch.Witnesses.Ordered
    ( FL, Fork(..), mapFL, nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )

import Darcs.Util.English ( presentParticiple )
import Darcs.Util.Printer ( vcat, text )
import Darcs.Util.Tree( Tree )

import GHC.Exts ( Constraint )

data PatchProxy (p :: * -> * -> *) = PatchProxy

-- |This class is a hack to abstract over pull/apply and rebase pull/apply.
class PatchApplier pa where

    type ApplierRepoTypeConstraint pa (rt :: RepoType) :: Constraint

    repoJob
        :: pa
        -> (forall rt p wR wU
               . ( IsRepoType rt, ApplierRepoTypeConstraint pa rt
                 , RepoPatch p, ApplyState p ~ Tree
                 )
              => (PatchProxy p -> Repository rt p wR wU wR -> IO ()))
        -> RepoJob ()

    applyPatches
        :: forall rt p wR wU wZ
         . ( ApplierRepoTypeConstraint pa rt, IsRepoType rt
           , RepoPatch p, ApplyState p ~ Tree
           )
        => pa
        -> PatchProxy p
        -> String
        -> [DarcsFlag]
        -> Repository rt p wR wU wR
        -> Fork (PatchSet rt p)
                (FL (PatchInfoAnd rt p))
                (FL (PatchInfoAnd rt p)) Origin wR wZ
        -> IO ()

data StandardPatchApplier = StandardPatchApplier

instance PatchApplier StandardPatchApplier where
    type ApplierRepoTypeConstraint StandardPatchApplier rt = ()
    repoJob StandardPatchApplier f = RepoJob (f PatchProxy)
    applyPatches StandardPatchApplier PatchProxy = standardApplyPatches

standardApplyPatches :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                     => String
                     -> [DarcsFlag]
                     -> Repository rt p wR wU wR
                     -> Fork (PatchSet rt p)
                             (FL (PatchInfoAnd rt p))
                             (FL (PatchInfoAnd rt p)) Origin wR wZ
                     -> IO ()
standardApplyPatches cmdName opts _repository patches@(Fork _ _ to_be_applied) = do
    applyPatchesStart cmdName opts to_be_applied

    Sealed pw <- tentativelyMergePatches _repository cmdName
                         (allowConflicts opts)
                         (externalMerge ? opts) (wantGuiPause opts)
                         (compress ? opts) (verbosity ? opts)
                         (reorder ? opts) (diffingOpts opts)
                         patches
    invalidateIndex _repository
    testTentativeAndMaybeExit _repository
         (verbosity ? opts)
         (testChanges ? opts)
         (setScriptsExecutable ? opts)
         (isInteractive True opts)
         "those patches do not pass the tests." (cmdName ++ " them") Nothing

    applyPatchesFinish cmdName opts _repository pw (nullFL to_be_applied)

applyPatchesStart :: (RepoPatch p, ApplyState p ~ Tree)
                  => String -> [DarcsFlag] -> FL (PatchInfoAnd rt p) wX wY -> IO ()
applyPatchesStart cmdName opts to_be_applied = do
    printDryRunMessageAndExit cmdName
        (verbosity ? opts)
        (O.withSummary ? opts)
        (dryRun ? opts)
        (xmlOutput ? opts)
        (isInteractive True opts)
        to_be_applied
    if nullFL to_be_applied then
        putStrLn $ "You don't want to " ++ cmdName ++ " any patches, and that's fine with me!"
    else do
        putVerbose opts $ text $ "Will " ++ cmdName ++ " the following patches:"
        putVerbose opts . vcat $ mapFL description to_be_applied
        setEnvDarcsPatches to_be_applied

applyPatchesFinish :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                   => String
                   -> [DarcsFlag]
                   -> Repository rt p wR wU wR
                   -> FL (PrimOf p) wU wY
                   -> Bool
                   -> IO ()
applyPatchesFinish cmdName opts _repository pw any_applied = do
    withSignalsBlocked $ do
        _repository <-
            finalizeRepositoryChanges _repository YesUpdatePending (compress ? opts)
        void $ applyToWorking _repository (verbosity ? opts) pw
        when (setScriptsExecutable ? opts == O.YesSetScriptsExecutable) $
            setScriptsExecutablePatches pw
        return ()
    case (any_applied, reorder ? opts == O.Reorder) of
        (True,True)  -> putFinished opts $ "reordering"
        (False,True) -> putFinished opts $ presentParticiple cmdName ++ " and reordering"
        _            -> putFinished opts $ presentParticiple cmdName

