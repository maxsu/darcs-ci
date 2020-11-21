-- Copyright (C) 2003-2004 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{- | Definitions used in this module:

[Explicit dependencies]: The set of patches that a (named) patch depends on
  "by name", i.e. irrespective of (non-)commutation (non commuting patches are
  implicit dependencies). The most important example are tags, but non-tag
  patches can also have explicit dependencies by recording them with
  --ask-deps.

[Covered]: A patch @p@ is covered by a tag @t@ if @t@ explicitly depends on
  @p@ or a tag covered by @t@ explicitly depends on @p@. In other words, the
  transitive closure of the relation "is depended on", restricted to
  situations where the right hand side is a tag. Note that it does /not/ take
  explicit dependencies of non-tag patches into account at all.

[Clean]: A tag @t@ in a repository is clean if all patches prior to the tag are
  covered by @t@. Tags normally start out as clean tags (the exception is
  if --ask-deps is used). It typically becomes unclean when it is merged into
  another repo (here the exceptions are if --reorder-patches is used, or if
  the target repo is actually a subset of the source repo).
-}

module Darcs.Patch.Depends
    ( getUncovered
    , areUnrelatedRepos
    , findCommonAndUncommon
    , mergeThem
    , findCommonWithThem
    , countUsThem
    , removeFromPatchSet
    , slightlyOptimizePatchset
    , splitOnTag
    , patchSetUnion
    , patchSetIntersection
    , findUncommon
    , cleanLatestTag
    , contextPatches
    ) where

import Darcs.Prelude

import Data.List ( delete, intersect, (\\) )
import Data.Maybe ( fromMaybe )

import Darcs.Patch.Named ( getdeps )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Ident ( fastRemoveSubsequenceRL, merge2FL )
import Darcs.Patch.Info ( PatchInfo, isTag, displayPatchInfo )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Permutations ( partitionFL, partitionRL )
import Darcs.Patch.PatchInfoAnd( PatchInfoAnd, hopefully, hopefullyM, info )
import Darcs.Patch.Set
    ( PatchSet(..)
    , Tagged(..)
    , SealedPatchSet
    , patchSet2RL
    , appendPSFL
    , patchSetSplit
    , Origin
    )
import Darcs.Patch.Progress ( progressRL )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Patch.Witnesses.Eq ( Eq2 )
import Darcs.Patch.Witnesses.Ordered
    ( (:\/:)(..), (:/\:)(..), (:>)(..), Fork(..),
    (+<<+), mapFL, RL(..), FL(..), isShorterThanRL, breakRL,
    (+<+), reverseFL, reverseRL, mapRL )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..), seal )

import Darcs.Util.Printer ( renderString, vcat )

{-|
Find clean tags that are common to both argument 'PatchSet's and return a
'Fork' with the common clean tags and whatever remains of the 'PatchSet's.
The two "uncommon" sequences may still have patches in common, even clean
tags, since we look only at the "known clean" tags of the second argument,
i.e. those that are the head of a 'Tagged' section.

This is a pretty efficient function, because it makes use of the
already-broken-up nature of 'PatchSet's.

Note that the first argument should be the repository that is more cheaply
accessed (i.e. local), as 'taggedIntersection' does its best to reduce the
number of inventories that are accessed from its second argument.
-}
taggedIntersection :: forall rt p wX wY . Commute p
                   => PatchSet rt p Origin wX -> PatchSet rt p Origin wY ->
                      Fork (RL (Tagged rt p))
                           (RL (PatchInfoAnd rt p))
                           (RL (PatchInfoAnd rt p)) Origin wX wY
taggedIntersection (PatchSet NilRL ps1) s2 = Fork NilRL ps1 (patchSet2RL s2)
taggedIntersection s1 (PatchSet NilRL ps2) = Fork NilRL (patchSet2RL s1) ps2
taggedIntersection s1 (PatchSet (_ :<: Tagged t2 _ _) ps2)
    -- If t2 is the head of any of the Tagged sections of s1,
    -- unwrap everything in s1 after t2 and be done with it.
    | Just (PatchSet ts1 ps1) <- maybeSplitSetOnTag (info t2) s1 =
        Fork ts1 ps1 (unsafeCoercePStart ps2)
taggedIntersection s1 s2@(PatchSet (ts2 :<: Tagged t2 _ t2ps) ps2) =
    -- Same case as before but now we know that t2 is not the head of any
    -- Tagged section of s1. If t2 has already been fully retrieved, then
    -- we know that the next Tagged section of s2 is available without
    -- opening another remote inventory; in this case we recurse i.e.
    -- we unwrap t2 and its patches and continue with the next Tagged of s2.
    -- Otherwise we try to make t2 clean in s1 by looking at s1's trailing
    -- patch list, too.
    -- Question by bf: Wouldn't it be better to call splitOnTag /before/ we
    -- test if t2 has already been opened? If it succeeds, then we'd get
    -- more common Tagged sections and still don't have to open a remote
    -- inventory.
    case hopefullyM t2 of
        Just _ ->
            taggedIntersection s1 (PatchSet ts2 (t2ps :<: t2 +<+ ps2))
        Nothing ->
            case splitOnTag (info t2) s1 of
                Just (PatchSet com us) ->
                      Fork com us (unsafeCoercePStart ps2)
                Nothing -> Fork NilRL (patchSet2RL s1) (patchSet2RL s2)

-- |'maybeSplitSetOnTag' takes a tag's 'PatchInfo', @t0@, and a 'PatchSet' and
-- attempts to find @t0@ in one of the 'Tagged's in the PatchSet. If the tag is
-- found, the 'PatchSet' is split up, on that tag, such that all later patches
-- are in the "since last tag" patch list. If the tag is not found, 'Nothing'
-- is returned.
-- This is a simpler version of 'splitOnTag' that only looks at the heads
-- of 'Tagged' sections and does not commute any patches.
maybeSplitSetOnTag :: PatchInfo -> PatchSet rt p wStart wX
                   -> Maybe (PatchSet rt p wStart wX)
maybeSplitSetOnTag t0 origSet@(PatchSet (ts :<: Tagged t _ pst) ps)
    | t0 == info t = Just origSet
    | otherwise = do
        PatchSet ts' ps' <- maybeSplitSetOnTag t0 (PatchSet ts (pst :<: t))
        Just $ PatchSet ts' (ps' +<+ ps)
maybeSplitSetOnTag _ _ = Nothing

-- | Take a tag's 'PatchInfo', and a 'PatchSet', and attempt to find the tag in
-- the 'PatchSet'. If found, return a new 'PatchSet', in which the tag is now
-- clean (and the last of the 'Tagged' list), while all patches that are not
-- covered by the tag are in the trailing list of patches.
-- If the tag is not in the 'PatchSet', we return 'Nothing'.
splitOnTag :: Commute p => PatchInfo -> PatchSet rt p wStart wX
           -> Maybe (PatchSet rt p wStart wX)
-- If the tag we are looking for is the first Tagged tag of the patchset, just
-- separate out the patchset's patches.
splitOnTag t s@(PatchSet (_ :<: Tagged hp _ _) _) | info hp == t = Just s
-- If the tag is the most recent patch in the set, we check if the patch is the
-- only non-depended-on patch in the set (i.e. it is a clean tag); creating a
-- new Tagged out of the patches and tag, and adding it to the patchset, if
-- this is the case. Otherwise, we try to make the tag clean.
splitOnTag t patchset@(PatchSet ts hps@(ps :<: hp)) | info hp == t =
    if getUncovered patchset == [t]
        -- If t is the only patch not covered by any tag...
        then Just $ PatchSet (ts :<: Tagged hp Nothing ps) NilRL
        else case partitionRL ((`notElem` (t : getdeps (hopefully hp))) . info) hps of
            -- Partition hps by those that are the tag and its explicit deps.
            tagAndDeps@(ds' :<: hp') :> nonDeps ->
                -- If @ds@ doesn't contain the tag of the first Tagged, that
                -- tag will also be returned by the call to getUncovered - so
                -- we need to unwrap the next Tagged in order to expose it to
                -- being partitioned out in the recursive call to splitOnTag.
                if getUncovered (PatchSet ts tagAndDeps) == [t]
                    then let tagged = Tagged hp' Nothing ds' in
                         return $ PatchSet (ts :<: tagged) nonDeps
                    else do
                        unfolded <- unwrapOneTagged $ PatchSet ts tagAndDeps
                        PatchSet xx yy <- splitOnTag t unfolded
                        return $ PatchSet xx (yy +<+ nonDeps)
            _ -> error "impossible case"
-- We drop the leading patch, to try and find a non-Tagged tag.
splitOnTag t (PatchSet ts (ps :<: p)) = do
    PatchSet ns xs <- splitOnTag t (PatchSet ts ps)
    return $ PatchSet ns (xs :<: p)
-- If there are no patches left, we "unfold" the next Tagged, and try again.
splitOnTag t0 patchset@(PatchSet (_ :<: Tagged _ _ _s) NilRL) =
    unwrapOneTagged patchset >>= splitOnTag t0
-- If we've checked all the patches, but haven't found the tag, return Nothing.
splitOnTag _ (PatchSet NilRL NilRL) = Nothing

-- | Reorder a 'PatchSet' such that the latest tag becomes clean.
cleanLatestTag :: Commute p
               => PatchSet rt p wStart wX
               -> PatchSet rt p wStart wX
cleanLatestTag inp@(PatchSet ts ps) =
  case breakRL (isTag . info) ps of
    NilRL :> _ -> inp -- no tag among the ps -> we are done
    (left@(_ :<: t) :> right) ->
      case splitOnTag (info t) (PatchSet ts left) of
        Just (PatchSet ts' ps') -> PatchSet ts' (ps' +<+ right)
        _ -> error "impossible case" -- because t is in left

-- |'unwrapOneTagged' unfolds a single Tagged object in a PatchSet, adding the
-- tag and patches to the PatchSet's patch list.
unwrapOneTagged :: PatchSet rt p wX wY -> Maybe (PatchSet rt p wX wY)
unwrapOneTagged (PatchSet (ts :<: Tagged t _ tps) ps) =
    Just $ PatchSet ts (tps :<: t +<+ ps)
unwrapOneTagged _ = Nothing

-- | Return the 'PatchInfo' for all the patches in a 'PatchSet'
-- that are not depended on by any tag (in the given 'PatchSet').
--
-- This is exactly the set of patches that a new tag recorded on top
-- of the 'PatchSet' would explicitly depend on.
getUncovered :: PatchSet rt p wStart wX -> [PatchInfo]
getUncovered patchset = case patchset of
    (PatchSet NilRL ps) -> findUncovered (mapRL infoAndExplicitDeps ps)
    (PatchSet (_ :<: Tagged t _ _) ps) ->
        findUncovered (mapRL infoAndExplicitDeps (NilRL :<: t +<+ ps))
  where
    findUncovered :: [(PatchInfo, Maybe [PatchInfo])] -> [PatchInfo]
    findUncovered [] = []
    findUncovered ((pi, Nothing) : rest) = pi : findUncovered rest
    findUncovered ((pi, Just deps) : rest) =
        pi : findUncovered (dropDepsIn deps rest)

    -- |dropDepsIn traverses the list of patches, dropping any patches that
    -- occur in the dependency list; when a patch is dropped, its dependencies
    -- are added to the dependency list used for later patches.
    dropDepsIn :: [PatchInfo] -> [(PatchInfo, Maybe [PatchInfo])]
               -> [(PatchInfo, Maybe [PatchInfo])]
    dropDepsIn [] pps = pps
    dropDepsIn _  []  = []
    dropDepsIn ds (hp : pps)
        | fst hp `elem` ds =
            let extraDeps = fromMaybe [] $ snd hp in
            dropDepsIn (extraDeps ++ delete (fst hp) ds) pps
        | otherwise = hp : dropDepsIn ds pps

    -- |infoAndExplicitDeps returns the patch info and (for tags only) the list
    -- of explicit dependencies of a patch.
    infoAndExplicitDeps :: PatchInfoAnd rt p wX wY
                        -> (PatchInfo, Maybe [PatchInfo])
    infoAndExplicitDeps p
        | isTag (info p) = (info p, getdeps `fmap` hopefullyM p)
        | otherwise = (info p, Nothing)

-- | Create a new 'Tagged' section for the most recent clean tag found in the
-- tail of un-'Tagged' patches without re-ordering patches. Note that earlier
-- tags may remain un-'Tagged' even if they are actually clean.
slightlyOptimizePatchset :: PatchSet rt p wStart wX -> PatchSet rt p wStart wX
slightlyOptimizePatchset (PatchSet ts0 ps0) =
    go $ PatchSet ts0 (progressRL "Optimizing inventory" ps0)
  where
    go :: PatchSet rt p wStart wY -> PatchSet rt p wStart wY
    go (PatchSet ts NilRL) = PatchSet ts NilRL
    go s@(PatchSet ts (ps :<: hp))
        | isTag (info hp)
        , [info hp] == getUncovered s =
            PatchSet (ts :<: Tagged hp Nothing ps) NilRL
        | otherwise = appendPSFL (go (PatchSet ts ps)) (hp :>: NilFL)

removeFromPatchSet :: (Commute p, Eq2 p) => FL (PatchInfoAnd rt p) wX wY
                   -> PatchSet rt p wStart wY -> Maybe (PatchSet rt p wStart wX)
removeFromPatchSet bad (PatchSet ts ps) | all (`elem` mapRL info ps) (mapFL info bad) = do
    ps' <- fastRemoveSubsequenceRL (reverseFL bad) ps
    return (PatchSet ts ps')
removeFromPatchSet _ (PatchSet NilRL _) = Nothing
removeFromPatchSet bad (PatchSet (ts :<: Tagged t _ tps) ps) =
    removeFromPatchSet bad (PatchSet ts (tps :<: t +<+ ps))

findCommonAndUncommon :: forall rt p wX wY . Commute p
                      => PatchSet rt p Origin wX -> PatchSet rt p Origin wY
                      -> Fork (PatchSet rt p)
                              (FL (PatchInfoAnd rt p))
                              (FL (PatchInfoAnd rt p)) Origin wX wY
findCommonAndUncommon us them = case taggedIntersection us them of
    Fork common us' them' ->
        case partitionFL (infoIn them') $ reverseRL us' of
            _ :> bad@(_ :>: _) :> _ ->
                error $ "Failed to commute common patches:\n"
                      ++ renderString
                          (vcat $ mapRL (displayPatchInfo . info) $ reverseFL bad)
            (common2 :> NilFL :> only_ours) ->
                case partitionFL (infoIn us') $ reverseRL them' of
                    _ :> bad@(_ :>: _) :> _ ->
                        error $ "Failed to commute common patches:\n"
                            ++ renderString (vcat $
                                mapRL (displayPatchInfo . info) $ reverseFL bad)
                    _ :> NilFL :> only_theirs ->
                        Fork (PatchSet common (reverseFL common2))
                            only_ours (unsafeCoercePStart only_theirs)
  where
    infoIn inWhat = (`elem` mapRL info inWhat) . info

findCommonWithThem :: Commute p
                   => PatchSet rt p Origin wX
                   -> PatchSet rt p Origin wY
                   -> (PatchSet rt p :> FL (PatchInfoAnd rt p)) Origin wX
findCommonWithThem us them = case taggedIntersection us them of
    Fork common us' them' ->
        case partitionFL ((`elem` mapRL info them') . info) $ reverseRL us' of
            _ :> bad@(_ :>: _) :> _ ->
                error $ "Failed to commute common patches:\n"
                      ++ renderString
                          (vcat $ mapRL (displayPatchInfo . info) $ reverseFL bad)
            common2 :> _nilfl :> only_ours ->
                PatchSet common (reverseFL common2) :> unsafeCoerceP only_ours

findUncommon :: Commute p
             => PatchSet rt p Origin wX -> PatchSet rt p Origin wY
             -> (FL (PatchInfoAnd rt p) :\/: FL (PatchInfoAnd rt p)) wX wY
findUncommon us them =
    case findCommonWithThem us them of
        _common :> us' -> case findCommonWithThem them us of
            _ :> them' -> unsafeCoercePStart us' :\/: them'

countUsThem :: Commute p
            => PatchSet rt p Origin wX
            -> PatchSet rt p Origin wY
            -> (Int, Int)
countUsThem us them =
    case taggedIntersection us them of
        Fork _ us' them' -> let uu = mapRL info us'
                                tt = mapRL info them' in
                            (length $ uu \\ tt, length $ tt \\ uu)

mergeThem :: (Commute p, Merge p)
          => PatchSet rt p Origin wX -> PatchSet rt p Origin wY
          -> Sealed (FL (PatchInfoAnd rt p) wX)
mergeThem us them =
    case taggedIntersection us them of
        Fork _ us' them' ->
            case merge2FL (reverseRL us') (reverseRL them') of
               them'' :/\: _ -> Sealed them''

patchSetIntersection :: Commute p
                   => [SealedPatchSet rt p Origin]
                   -> SealedPatchSet rt p Origin
patchSetIntersection [] = seal $ PatchSet NilRL NilRL
patchSetIntersection [x] = x
patchSetIntersection (Sealed y : ys) =
    case patchSetIntersection ys of
        Sealed z -> case taggedIntersection y z of
            Fork common a b -> case mapRL info a `intersect` mapRL info b of
                morecommon ->
                    case partitionRL (\e -> info e `notElem` morecommon) a of
                        commonps :> _ -> seal $ PatchSet common commonps

patchSetUnion :: (Commute p, Merge p, Eq2 p)
            => [SealedPatchSet rt p Origin]
            -> SealedPatchSet rt p Origin
patchSetUnion [] = seal $ PatchSet NilRL NilRL
patchSetUnion [x] = x
patchSetUnion (Sealed y@(PatchSet tsy psy) : Sealed y2 : ys) =
    case mergeThem y y2 of
        Sealed p2 ->
            patchSetUnion $ seal (PatchSet tsy (psy +<<+ p2)) : ys

areUnrelatedRepos :: Commute p
                  => PatchSet rt p Origin wX
                  -> PatchSet rt p Origin wY -> Bool
areUnrelatedRepos us them =
    case taggedIntersection us them of
        Fork c u t -> checkit c u t
  where
    checkit (_ :<: Tagged{}) _ _ = False
    checkit _ u t | t `isShorterThanRL` 5 = False
                  | u `isShorterThanRL` 5 = False
                  | otherwise = null $ intersect (mapRL info u) (mapRL info t)

-- | Split a 'PatchSet' at the latest clean tag. The left part is what comes
-- before the tag, the right part is the tag and its non-dependencies.
contextPatches :: PatchSet rt p wX wY
               -> (PatchSet rt p :> RL (PatchInfoAnd rt p)) wX wY
contextPatches = patchSetSplit . slightlyOptimizePatchset
