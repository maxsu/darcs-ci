--  Copyright (C) 2009 Ganesh Sittampalam
--
--  BSD3
module Darcs.Patch.Rebase.Change
    ( RebaseChange(..)
    , toRebaseChanges
    , extractRebaseChange
    , reifyRebaseChange
    , partitionUnconflicted
    , rcToPia
    , WithDroppedDeps(..)
    , WDDNamed
    , commuterIdWDD
    , simplifyPush, simplifyPushes
    , addNamedToRebase
    ) where

import Darcs.Prelude

import Darcs.Patch.Commute ( commuteFL, commuteRL )
import Darcs.Patch.CommuteFn
    ( CommuteFn
    , MergeFn
    , commuterFLId, commuterIdFL
    )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Info ( PatchInfo, patchinfo, displayPatchInfo )
import Darcs.Patch.Invert ( Invert, invert, invertFL )
import Darcs.Patch.Merge ( selfMerger )
import Darcs.Patch.Named
    ( Named(..)
    , HasDeps(..)
    , infopatch
    , mergerIdNamed
    , patchcontents
    , ShowDepsFormat(..)
    , showDependencies
    )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, PatchInfoAndG, n2pia )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Show ( ShowPatch(..), displayPatch )
import Darcs.Patch.Summary
    ( ConflictState(..)
    , IsConflictedPrim(..)
    , Summary(..)
    , plainSummary
    , plainSummaryFL
    )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), FromPrim(..) )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanFL )
import Darcs.Patch.Prim.Class ( PrimPatch )
import Darcs.Patch.Rebase.Fixup
    ( RebaseFixup(..)
    , commuteFixupNamed, commuteNamedFixup
    , flToNamesPrims
    , pushFixupFixup
    )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Patch.Rebase.PushFixup
  ( PushFixupFn, dropFixups
  , pushFixupFLMB_FLFLMB
  , pushFixupIdMB_FLFLMB
  , pushFixupIdMB_FLIdFLFL
  )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.RepoType ( RepoType(..), RebaseType(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatchFor(..), ShowContextPatch(..) )
import Darcs.Patch.Unwind ( Unwound(..), fullUnwind )
import Darcs.Patch.Witnesses.Maybe ( Maybe2(..) )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.Parser ( lexString )
import Darcs.Util.Printer ( Doc, ($$), ($+$), (<+>), blueText, redText, empty, vcat )

import qualified Data.ByteString.Char8 as BC ( pack )
import Data.List ( (\\) )
import Data.List.Ordered ( nubSort )
import Data.Maybe ( fromMaybe )

data RebaseChange prim wX wY where
    RC :: FL (RebaseFixup prim) wX wY -> Named prim wY wZ -> RebaseChange prim wX wZ

instance Show2 prim => Show1 (RebaseChange prim wX)

instance Show2 prim => Show2 (RebaseChange prim)

deriving instance Show2 prim => Show (RebaseChange prim wX wY)

-- |Get hold of the 'Named' patch inside a 'RebaseChange' and wrap it in a
-- 'PatchInfoAnd'.
rcToPia :: RebaseChange prim wX wY -> Sealed2 (PatchInfoAnd ('RepoType 'NoRebase) prim)
rcToPia (RC _ toEdit) = Sealed2 (n2pia toEdit)

instance PrimPatch prim => PrimPatchBase (RebaseChange prim) where
  type PrimOf (RebaseChange prim) = prim

instance PatchDebug prim => PatchDebug (RebaseChange prim)

instance HasDeps (RebaseChange prim) where
  getdeps (RC _ toedit) = getdeps toedit

type instance PatchId (RebaseChange prim) = PatchInfo

instance Ident (RebaseChange prim) where
  ident (RC _ toedit) = ident toedit

instance Apply prim => Apply (RebaseChange prim) where
   type ApplyState (RebaseChange prim) = ApplyState prim
   apply (RC fixups toedit) = apply fixups >> apply toedit
   unapply (RC fixups toedit) = unapply toedit >> unapply fixups

instance Commute prim => Summary (RebaseChange prim) where
  conflictedEffect (RC fixups toedit) =
    case flToNamesPrims fixups of
      _names :> prims ->
        -- Report on the conflicts we would get if we unsuspended just this patch.
        -- An alternative implementation strategy would be to "force commute"
        -- prims :> toedit and report on the resulting conflicts in toedit.
        -- However this ties us to a specific RepoPatch type which isn't really
        -- needed for a simple calculation like this.
        --
        -- The rebase invariants should mean that 'fixups' (if non-empty) won't
        -- commute with 'changes' as a whole, but here we need to report each individual
        -- prim as conflicted or not, so we try to push the fixups as far through
        -- the individual prims as we can.
        --
        -- Taking the effect also means that any conflicts already present in the
        -- suspended patch won't be reported, but in general such conflicts
        -- are not supported anyway.
        case genCommuteWhatWeCanFL (commuterFLId commute) (prims :> patchcontents toedit) of
          unconflicted :> _ :> conflicted ->
            mapFL (IsC Okay) unconflicted ++ mapFL (IsC Conflicted) conflicted

instance (ShowPatchBasic prim, Invert prim, PatchListFormat prim)
  => ShowPatchBasic (RebaseChange prim) where
  showPatch ForStorage (RC fixups toedit) =
    blueText "rebase-change"
      <+> blueText "(" $$ showPatch ForStorage fixups $$ blueText ")"
      $$ showPatch ForStorage toedit
  showPatch ForDisplay p@(RC _ (NamedP n _ _)) =
    displayPatchInfo n $$ rebaseChangeContent p

rebaseChangeContent :: (ShowPatchBasic prim, Invert prim)
                   => RebaseChange prim wX wY -> Doc
rebaseChangeContent (RC fixups contents) =
  vcat (mapFL (showPatch ForDisplay) (patchcontents contents)) $+$
  if nullFL fixups
    then empty
    else redText "conflicts:" $+$ vcat (mapRL showFixup (invertFL fixups))
  where
    showFixup (PrimFixup p) = displayPatch p
    showFixup (NameFixup n) = displayPatch n

instance PrimPatch prim => ShowPatch (RebaseChange prim) where
    -- This should really just call 'description' on the ToEdit patch,
    -- but that introduces a spurious dependency on Summary (PrimOf p),
    -- because of other methods in the Named instance, so we just inline
    -- the implementation from Named here.
    description (RC _ (NamedP n _ _)) = displayPatchInfo n
    -- TODO report conflict indicating name fixups (i.e. dropped deps)
    summary p@(RC _ (NamedP _ ds _)) =
      showDependencies ShowDepsSummary ds $$ plainSummary p
    summaryFL ps =
      showDependencies ShowDepsSummary (getdepsFL ps) $$ plainSummaryFL ps
      where
        getdepsFL = nubSort . concat . mapFL getdeps
    content = rebaseChangeContent

-- TODO this is a dummy instance that does not actually show context
instance (ShowPatchBasic prim, Invert prim, PatchListFormat prim)
  => ShowContextPatch (RebaseChange prim) where
    showContextPatch f p = return $ showPatch f p

instance (ReadPatch prim, PatchListFormat prim) => ReadPatch (RebaseChange prim) where
  readPatch' = do
    lexString (BC.pack "rebase-change")
    lexString (BC.pack "(")
    Sealed fixups <- readPatch'
    lexString (BC.pack ")")
    Sealed contents <- readPatch'
    return $ Sealed $ RC fixups contents

toRebaseChanges
    :: FL (RebaseChange prim) wX wY
    -> FL (PatchInfoAndG ('RepoType 'IsRebase) (RebaseChange prim)) wX wY
toRebaseChanges = mapFL_FL n2pia

instance Commute prim => Commute (RebaseChange prim) where
  commute (RC fixups1 edit1 :> RC fixups2 edit2) = do
    fixups2' :> edit1' <- commuterIdFL commuteNamedFixup (edit1 :> fixups2)
    edit2' :> edit1'' <- commute (edit1' :> edit2)
    fixupsS :> (fixups2'' :> edit2'') :> fixups1' <-
      return $ pushThrough (fixups1 :> (fixups2' :> edit2'))
    return (RC (fixupsS +>+ fixups2'') edit2'' :> RC fixups1' edit1'')

instance PatchInspect prim => PatchInspect (RebaseChange prim) where
   listTouchedFiles (RC fixup toedit) = nubSort (listTouchedFiles fixup ++ listTouchedFiles toedit)
   hunkMatches f (RC fixup toedit) = hunkMatches f fixup || hunkMatches f toedit

-- |Split a list of rebase patches into those that will
-- have conflicts if unsuspended and those that won't.
partitionUnconflicted
    :: Commute prim
    => FL (RebaseChange prim) wX wY
    -> (FL (RebaseChange prim) :> RL (RebaseChange prim)) wX wY
partitionUnconflicted = partitionUnconflictedAcc NilRL

partitionUnconflictedAcc
  :: Commute prim
  => RL (RebaseChange prim) wX wY -> FL (RebaseChange prim) wY wZ
  -> (FL (RebaseChange prim) :> RL (RebaseChange prim)) wX wZ
partitionUnconflictedAcc right NilFL = NilFL :> right
partitionUnconflictedAcc right (p :>: ps) =
   case commuteRL (right :> p) of
     Just (p'@(RC NilFL _) :> right')
       -> case partitionUnconflictedAcc right' ps of
            left' :> right'' -> (p' :>: left') :> right''
     _ -> partitionUnconflictedAcc (right :<: p) ps

-- | A patch, together with a list of patch names that it used to depend on,
-- but were lost during the rebasing process. The UI can use this information
-- to report them to the user.
data WithDroppedDeps p wX wY =
    WithDroppedDeps {
        wddPatch :: p wX wY,
        wddDependedOn :: [PatchInfo]
    }

noDroppedDeps :: p wX wY -> WithDroppedDeps p wX wY
noDroppedDeps p = WithDroppedDeps p []

instance PrimPatchBase p => PrimPatchBase (WithDroppedDeps p) where
   type PrimOf (WithDroppedDeps p) = PrimOf p

instance Effect p => Effect (WithDroppedDeps p) where
   effect = effect . wddPatch

-- |Given a list of rebase items, try to push a new fixup as far as possible into
-- the list as possible, using both commutation and coalescing. If the fixup
-- commutes past all the 'ToEdit' patches then it is dropped entirely.
simplifyPush
  :: PrimPatch prim
  => D.DiffAlgorithm
  -> RebaseFixup prim wX wY
  -> FL (RebaseChange prim) wY wZ
  -> Sealed (FL (RebaseChange prim) wX)
simplifyPush da fixup items = dropFixups $ pushFixupChanges da (fixup :> items)

-- |Like 'simplifyPush' but for a list of fixups.
simplifyPushes
  :: PrimPatch prim
  => D.DiffAlgorithm
  -> FL (RebaseFixup prim) wX wY
  -> FL (RebaseChange prim) wY wZ
  -> Sealed (FL (RebaseChange prim) wX)
simplifyPushes _ NilFL ps = Sealed ps
simplifyPushes da (f :>: fs) ps = unseal (simplifyPush da f) (simplifyPushes da fs ps)

pushFixupChange
  :: PrimPatch prim
  => D.DiffAlgorithm
  -> PushFixupFn
       (RebaseFixup prim) (RebaseChange prim)
       (RebaseChange prim) (Maybe2 (RebaseFixup prim))
pushFixupChange da (f1 :> RC fs2 e)
  = case pushFixupFLMB_FLFLMB (pushFixupFixup da) (f1 :> fs2) of
      fs2' :> Nothing2 -> RC fs2' e :> Nothing2
      fs2' :> Just2 f1' ->
        case commuteFixupNamed (f1' :> e) of
          -- The fixup is "stuck" so just attach it here
          Nothing -> RC (fs2' +>+ f1' :>: NilFL) e :> Nothing2
          Just (e' :> f1'') -> RC fs2' e' :> Just2 f1''

pushFixupChanges
  :: PrimPatch prim
  =>  D.DiffAlgorithm
  -> PushFixupFn
       (RebaseFixup prim) (FL (RebaseChange prim))
       (FL (RebaseChange prim)) (Maybe2 (RebaseFixup prim))
pushFixupChanges da = pushFixupIdMB_FLFLMB (pushFixupChange da)

pushFixupsChange
  :: PrimPatch prim
  => D.DiffAlgorithm
  -> PushFixupFn
       (FL (RebaseFixup prim)) (RebaseChange prim)
       (RebaseChange prim) (FL (RebaseFixup prim))
pushFixupsChange da = pushFixupIdMB_FLIdFLFL (pushFixupChange da)


-- Note, this could probably be rewritten using a generalised commuteWhatWeCanFL from
-- Darcs.Patch.Permutations.
-- |@pushThrough (ps :> (qs :> te))@ tries to commute as much of @ps@ as possible through
-- both @qs@ and @te@, giving @psStuck :> (qs' :> te') :> psCommuted@.
-- Anything that can be commuted ends up in @psCommuted@ and anything that can't goes in
-- @psStuck@.
pushThrough
  :: Commute prim
  => (FL (RebaseFixup prim) :> (FL (RebaseFixup prim) :> Named prim)) wX wY
  -> (FL (RebaseFixup prim) :> (FL (RebaseFixup prim) :> Named prim) :> FL (RebaseFixup prim)) wX wY
pushThrough (NilFL :> v) = NilFL :> v :> NilFL
pushThrough ((p :>: ps) :> v) =
  case pushThrough (ps :> v) of
   psS :> v'@(qs:>te) :> ps' ->
     fromMaybe ((p :>: psS) :> v' :> ps') $ do
       psS' :> p' <- commuteFL (p :> psS)
       qs' :> p'' <- commuteFL (p' :> qs)
       te' :> p''' <- commuteFixupNamed (p'' :> te)
       return (psS' :> (qs' :> te') :> (p''' :>: ps'))

type WDDNamed p = WithDroppedDeps (Named p)

mergerIdWDD :: MergeFn p1 p2 -> MergeFn p1 (WithDroppedDeps p2)
mergerIdWDD merger (p1 :\/: WithDroppedDeps p2 deps) =
   case merger (p1 :\/: p2) of
     p2' :/\: p1' -> WithDroppedDeps p2' deps :/\: p1'

commuterIdWDD :: CommuteFn p q -> CommuteFn p (WithDroppedDeps q)
commuterIdWDD commuter (p :> WithDroppedDeps q deps)
  = do -- no need to worry about names, because by definition a dropped dep
       -- is a name we no longer have
       -- TODO consistency checking?
       -- TODO consider inverse commutes, e.g. what happens if we wanted to
       -- commute (WithDroppedDeps ... [n] :> AddName n)?
       q' :> p' <- commuter (p :> q)
       return (WithDroppedDeps q' deps :> p')

-- |Forcibly commute a 'RebaseName' with a patch, dropping any dependencies
-- if necessary and recording them in the patch
forceCommuteName :: (RebaseName :> WDDNamed p) wX wY -> (WDDNamed p :> RebaseName) wX wY
forceCommuteName (AddName an :> WithDroppedDeps (NamedP pn deps body) ddeps)
  | an == pn = error "impossible case"
  | otherwise =
      WithDroppedDeps
        (NamedP pn (deps \\ [an]) (unsafeCoerceP body))
        (if an `elem` deps then an : ddeps else ddeps)
      :>
      AddName an
forceCommuteName (DelName dn :> p@(WithDroppedDeps (NamedP pn deps _body) _ddeps))
  | dn == pn = error "impossible case"
  | dn `elem` deps = error "impossible case"
  | otherwise = unsafeCoerceP p :> DelName dn
forceCommuteName (Rename old new :> WithDroppedDeps (NamedP pn deps body) ddeps)
  | old == pn = error "impossible case"
  | new == pn = error "impossible case"
  | old `elem` deps = error "impossible case"
  | otherwise =
      let newdeps = map (\dep -> if new == dep then old else dep) deps
      in WithDroppedDeps (NamedP pn newdeps (unsafeCoerceP body)) ddeps :> Rename old new

forceCommutePrim :: RepoPatch p
                 => (PrimOf p :> WDDNamed p) wX wY
                 -> (WDDNamed p :> FL (PrimOf p)) wX wY
forceCommutePrim (p :> wq) =
    -- rp and irp are not inverses for RepoPatchV3, only their effects are inverse
    let rp = fromAnonymousPrim p
        irp = fromAnonymousPrim (invert p)
    in case mergerIdWDD (mergerIdNamed selfMerger) (irp :\/: wq) of
        wq' :/\: irp' -> prefixWith (rp :>: irp :>: NilFL) wq' :> invert (effect irp')
    where
      -- TODO [V3INTEGRATION]:
      -- This is a hack to adapt forceCommutePrim to the stricter assumptions
      -- made by RepoPatchV3, for which resolveConflicts expects that we can
      -- find each patch we conflict with somewhere in the context.
      -- Force-commuting the fixups with the patch to be edited violates that
      -- assumption. It works for RepoPatchV1/2 because their conflictors are
      -- self-contained i.e. they contain the transitive set of conflicts in
      -- their representation, which is no longer true for RepoPatchV3.
      -- To restore the assumption for RepoPatchV3 we prefix the patches
      -- contained in the 'Named' patch with (rp;irp). The conflictor wq' can
      -- now refer to irp, and the effect of rp will cancel with that of irp
      -- on unsuspend.
      prefixWith xs (WithDroppedDeps (NamedP i ds ps) dds) =
          WithDroppedDeps (NamedP i ds (xs +>+ ps)) dds

forceCommutes :: RepoPatch p
              => (FL (RebaseFixup (PrimOf p)) :> WDDNamed p) wX wY
              -> (WDDNamed p :> FL (RebaseFixup (PrimOf p))) wX wY
forceCommutes (NilFL :> q) = q :> NilFL
forceCommutes ((NameFixup n :>: ps) :> q) =
    case forceCommutes (ps :> q) of
        q' :> ps' ->
            case forceCommuteName (n :> q') of
                q'' :> n' -> q'' :> (NameFixup n' :>: ps')
forceCommutes ((PrimFixup p :>: ps) :> q) =
    case forceCommutes (ps :> q) of
        q' :> ps' ->
            case forceCommutePrim (p :> q') of
                qs'' :> p' -> qs'' :> (mapFL_FL PrimFixup p' +>+ ps')

fromPrimNamed :: FromPrim p => Named (PrimOf p) wX wY -> Named p wX wY
fromPrimNamed (NamedP n deps ps) = NamedP n deps (fromPrims n ps)

-- |Turn a selected rebase patch back into a patch we can apply to
-- the main repository, together with residual fixups that need
-- to go back into the rebase state (unless the rebase is now finished).
-- Any fixups associated with the patch will turn into conflicts.
extractRebaseChange
  :: forall p wX wY
   . RepoPatch p
  => D.DiffAlgorithm
  -> FL (RebaseChange (PrimOf p)) wX wY
  -> (FL (WDDNamed p) :> FL (RebaseFixup (PrimOf p))) wX wY
extractRebaseChange da rcs = go (NilFL :> rcs)
  where
    go
      :: forall wA wB
       . (FL (RebaseFixup (PrimOf p)) :> FL (RebaseChange (PrimOf p))) wA wB
      -> (FL (WDDNamed p) :> FL (RebaseFixup (PrimOf p))) wA wB
    go (fixupsIn :> NilFL) = NilFL :> fixupsIn
    go (fixupsIn :> rc :>: rest) =
      -- First simplify any fixups coming from previous extract operations.
      -- Note that it's important to start at the front of the list so that
      -- we can do this, as it minimises the conflicts we end up with.
      case pushFixupsChange da (fixupsIn :> rc) of
        -- Now use 'fromPrimNamed' to change the toedit patch from
        -- Named (PrimOf p) that we store in the rebase to Named p
        -- that we store in the repository. Then, wrap it in WithDroppedDeps
        -- so we can track any explicit dependencies that were lost, and
        -- finally force-commute the fixups with this and any other patches we are
        -- unsuspending.
        RC fixups toedit :> fixupsOut2 ->
          case forceCommutes (fixups :> WithDroppedDeps (fromPrimNamed toedit) []) of
            toedit' :> fixupsOut1 ->
              case go (fixupsOut1 +>+ fixupsOut2 :> rest) of
                toedits' :> fixupsOut -> toedit' :>: toedits' :> fixupsOut

-- signature to be compatible with extractRebaseChange
-- | Like 'extractRebaseChange', but any fixups are "reified" into a separate patch.
reifyRebaseChange
  :: FromPrim p
  => String
  -> FL (RebaseChange (PrimOf p)) wX wY
  -> IO ((FL (WDDNamed p) :> FL (RebaseFixup (PrimOf p))) wX wY)
reifyRebaseChange author rs = do
    res <- concatFL <$> mapFL_FL_M reifyOne rs
    return (res :> NilFL)
  where
    reifyOne :: FromPrim p => RebaseChange (PrimOf p) wA wB -> IO (FL (WDDNamed p) wA wB)
    reifyOne (RC fixups toedit) =
      case flToNamesPrims fixups of
        names :> NilFL ->
          return $
            mapFL_FL (noDroppedDeps . mkDummy) names +>+
            noDroppedDeps (fromPrimNamed toedit) :>:
            NilFL
        names :> prims -> do
          n <- mkReified author prims
          return $
            mapFL_FL (noDroppedDeps . mkDummy) names +>+ noDroppedDeps n :>:
            noDroppedDeps (fromPrimNamed toedit) :>:
            NilFL

mkReified :: FromPrim p => String -> FL (PrimOf p) wX wY -> IO (Named p wX wY)
mkReified author ps = do
     let name = "Reified fixup patch"
     let desc = []
     date <- getIsoDateTime
     info <- patchinfo date name author desc
     return $ infopatch info ps

mkDummy :: FromPrim p => RebaseName wX wY -> Named p wX wY
mkDummy (AddName pi) = infopatch pi (unsafeCoerceP NilFL)
mkDummy (DelName _) = error "internal error: can't make a dummy patch from a delete"
mkDummy (Rename _ _) = error "internal error: can't make a dummy patch from a rename"

instance IsHunk (RebaseChange prim) where
    -- RebaseChange is a compound patch, so it doesn't really make sense to
    -- ask whether it's a hunk. TODO: get rid of the need for this.
    isHunk _ = Nothing

instance PatchListFormat (RebaseChange prim)

addNamedToRebase
  :: RepoPatch p
  => D.DiffAlgorithm
  -> Named p wX wY
  -> FL (RebaseChange (PrimOf p)) wY wZ
  -> Sealed (FL (RebaseChange (PrimOf p)) wX)
addNamedToRebase da named@(NamedP n deps _) =
  case fullUnwind named of
    Unwound before underlying after ->
      unseal (simplifyPushes da (mapFL_FL PrimFixup before)) .
      mapSeal ((RC NilFL (NamedP n deps underlying) :>:)) .
      simplifyPushes da (mapFL_FL PrimFixup (reverseRL after))
