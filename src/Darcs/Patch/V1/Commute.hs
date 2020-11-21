--  Copyright (C) 2002-2003 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Darcs.Patch.V1.Commute
    (
      merge,
      merger, unravel,
      publicUnravel,
    )
       where

import Darcs.Prelude

import Control.Monad ( MonadPlus, mplus, msum, mzero, guard )
import Control.Applicative ( Alternative(..) )
import Data.Maybe ( fromMaybe )

import Darcs.Patch.Commute ( selfCommuter )
import Darcs.Patch.CommuteFn ( commuterIdFL, commuterFLId )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Patch.Invert ( invertRL )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.V1.Core ( RepoPatchV1(..),
                             isMerger,
                             mergerUndo )
import Darcs.Patch.CommuteNoConflicts
    ( CommuteNoConflicts(..)
    , mergeNoConflicts
    )
import Darcs.Patch.Conflict
  ( Conflict(..), combineConflicts, mangleOrFail
  )
import Darcs.Patch.Unwind ( Unwind(..), Unwound(..), mkUnwound )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Prim ( PrimPatch, is_filepatch )
import Darcs.Patch.Permutations
    ( headPermutationsRL
    , simpleHeadPermutationsFL
    , removeFL
    )
import Darcs.Util.Printer ( renderString, text, vcat, ($$) )
import Darcs.Patch.V1.Show ( showPatch_ )
import Data.List ( nub )
import Data.List.Ordered ( nubSort )
import Darcs.Patch.Summary
    ( Summary(..)
    , ConflictState(..)
    , IsConflictedPrim(..)
    )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed(..) , mapSeal, unseal
    , FlippedSeal(..), mapFlipped, unsealFlipped
    )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..), Eq2(..) )
import Darcs.Patch.Witnesses.Unsafe
    ( unsafeCoerceP, unsafeCoercePStart
    , unsafeCoercePEnd )
import Darcs.Patch.Witnesses.Ordered
    ( mapFL_FL, mapFL,
    FL(..), RL(..), (+>+),
    (:/\:)(..), (:\/:)(..), (:>)(..),
    lengthFL, mapRL,
    reverseFL, reverseRL, concatFL
    )

data Perhaps a = Unknown | Failed | Succeeded a

instance Functor Perhaps where
    fmap _ Unknown = Unknown
    fmap _ Failed = Failed
    fmap f (Succeeded x) = Succeeded (f x)

instance Applicative Perhaps where
    pure = Succeeded
    _ <*> Failed = Failed
    _ <*> Unknown = Unknown
    Failed <*> _ = Failed
    Unknown <*> _ = Unknown
    Succeeded f <*> Succeeded x = Succeeded (f x)

instance  Monad Perhaps where
    (Succeeded x) >>= k =  k x
    Failed   >>= _      =  Failed
    Unknown  >>= _      =  Unknown
    return              =  Succeeded

instance Alternative Perhaps where
    empty = Unknown
    Unknown <|> ys    = ys
    Failed  <|> _     = Failed
    (Succeeded x) <|> _ = Succeeded x

instance  MonadPlus Perhaps where
    mzero = Unknown
    mplus = (<|>)

toMaybe :: Perhaps a -> Maybe a
toMaybe (Succeeded x) = Just x
toMaybe _ = Nothing

toPerhaps :: Maybe a -> Perhaps a
toPerhaps (Just x) = Succeeded x
toPerhaps Nothing = Failed

-- | 'cleverCommute' attempts to commute two patches @p1@ and @p2@, in their
-- original order, with the given commute function. If the commute function
-- doesn't know how to handle the patches (i.e. it returns Unknown as a
-- result), then we try again with @invert p2@ and @invert p1@ (inverting the
-- results, if succesful).
--
-- TODO: when can the first attempt fail, but the second not? What's so clever
-- in this function?
cleverCommute :: Invert prim => CommuteFunction prim -> CommuteFunction prim
cleverCommute c (p1 :> p2) = case c (p1 :> p2) of
    Succeeded x -> Succeeded x
    Failed -> Failed
    Unknown -> case c (invert p2 :> invert p1) of
                 Succeeded (ip1' :> ip2') -> Succeeded (invert ip2' :> invert ip1')
                 Failed -> Failed
                 Unknown -> Unknown

-- | If we have two Filepatches which modify different files, we can return a
-- result early, since the patches trivially commute.
speedyCommute :: PrimPatch prim => CommuteFunction prim
speedyCommute (p1 :> p2)
    | Just m1 <- isFilepatchMerger p1
    , Just m2 <- isFilepatchMerger p2
    , m1 /= m2 = Succeeded (unsafeCoerceP p2 :> unsafeCoerceP p1)
    | otherwise = Unknown

everythingElseCommute :: forall prim . PrimPatch prim => CommuteFunction prim
everythingElseCommute (PP p1 :> PP p2) = toPerhaps $ do
    p2' :> p1' <- commute (p1 :> p2)
    return (PP p2' :> PP p1')
everythingElseCommute ps =
    msum [ cleverCommute commuteRecursiveMerger      ps
         , cleverCommute otherCommuteRecursiveMerger ps
         ]

{-
Note that it must be true that

commutex (A^-1 A, P) = Just (P, A'^-1 A')

and

if commutex (A, B) == Just (B', A')
then commutex (B^-1, A^-1) == Just (A'^-1, B'^-1)
-}

unsafeMerger :: PrimPatch prim => String -> RepoPatchV1 prim wX wY -> RepoPatchV1 prim wX wZ -> RepoPatchV1 prim wA wB
unsafeMerger x p1 p2 = unseal unsafeCoerceP $ merger x p1 p2

-- | Attempt to commute two patches, the first of which is a Merger patch.
mergerCommute :: PrimPatch prim
              => (RepoPatchV1 prim :> RepoPatchV1 prim) wX wY -> Perhaps ((RepoPatchV1 prim :> RepoPatchV1 prim) wX wY)
mergerCommute (pA :> Merger _ _ p1 p2)
    | unsafeCompare pA p1 = Succeeded (unsafeCoercePStart p2 :> unsafeMerger "0.0" p2 p1)
    | unsafeCompare pA (invert (unsafeMerger "0.0" p2 p1)) = Failed
mergerCommute (Merger _ _ b' c'' :> Merger _ _ (Merger _ _ c b) (Merger _ _ c' a))
    | unsafeCompare b' b && unsafeCompare c c' && unsafeCompare c c'' =
        Succeeded ( unsafeMerger "0.0" b (unsafeCoercePStart a) :>
                    unsafeMerger "0.0" (unsafeMerger "0.0" b (unsafeCoercePStart a)) (unsafeMerger "0.0" b c)
                  )
mergerCommute _ = Unknown

instance PrimPatch prim => CleanMerge (RepoPatchV1 prim) where
    cleanMerge = mergeNoConflicts

instance PrimPatch prim => Merge (RepoPatchV1 prim) where
    merge (p1 :\/: p2) =
        case mergeNoConflicts (p1 :\/: p2) of
            Just r -> r
            Nothing ->
                case merger "0.0" p1 p2 of
                    Sealed p2' ->
                        case merger "0.0" p2 p1 of
                            Sealed p1' -> unsafeCoercePEnd p2' :/\: unsafeCoercePEnd p1'

instance PrimPatch prim => Commute (RepoPatchV1 prim) where
    commute x = toMaybe $ msum
                  [speedyCommute x,
                   (cleverCommute mergerCommute) x,
                   everythingElseCommute x
                  ]

instance PrimPatch prim => PatchInspect (RepoPatchV1 prim) where
    -- Recurse on everything, these are potentially spoofed patches
    listTouchedFiles (Merger _ _ p1 p2) = nubSort $ listTouchedFiles p1
                                            ++ listTouchedFiles p2
    listTouchedFiles c@(Regrem{}) = listTouchedFiles $ invert c
    listTouchedFiles (PP p) = listTouchedFiles p

    hunkMatches f (Merger _ _ p1 p2) = hunkMatches f p1 || hunkMatches f p2
    hunkMatches f c@(Regrem{}) = hunkMatches f $ invert c
    hunkMatches f (PP p) = hunkMatches f p

isFilepatchMerger :: PrimPatch prim => RepoPatchV1 prim wX wY -> Maybe AnchoredPath
isFilepatchMerger (PP p) = is_filepatch p
isFilepatchMerger (Merger _ _ p1 p2) = do
     f1 <- isFilepatchMerger p1
     f2 <- isFilepatchMerger p2
     if f1 == f2 then return f1 else Nothing
isFilepatchMerger (Regrem und unw p1 p2)
    = isFilepatchMerger (Merger und unw p1 p2)

commuteRecursiveMerger :: PrimPatch prim
    => (RepoPatchV1 prim :> RepoPatchV1 prim) wX wY -> Perhaps ((RepoPatchV1 prim :> RepoPatchV1 prim) wX wY)
commuteRecursiveMerger (pA :> p@(Merger _ _ p1 p2)) = toPerhaps $
  do (_ :> pA') <- commuterIdFL selfCommuter (pA :> undo)
     _ <- commuterIdFL selfCommuter (pA' :> invert undo)
     (_ :> pAmid) <- commute (pA :> unsafeCoercePStart (invert p1))
     (p1' :> pAx) <- commute (pAmid :> p1)
     guard (pAx `unsafeCompare` pA)
     (p2' :> _) <- commute (pAmid :> p2)
     (p2o :> _) <- commute (invert pAmid :> p2')
     guard (p2o `unsafeCompare` p2)
     let p' = if unsafeCompare p1' p1 && unsafeCompare p2' p2
              then unsafeCoerceP p
              else unsafeMerger "0.0" p1' p2'
         undo' = mergerUndo p'
     (pAo :> _) <- commuterFLId selfCommuter (undo' :> pA')
     guard (pAo `unsafeCompare` pA)
     return (p' :> pA')
    where undo = mergerUndo p
commuteRecursiveMerger _ = Unknown

otherCommuteRecursiveMerger :: PrimPatch prim
    => (RepoPatchV1 prim :> RepoPatchV1 prim) wX wY -> Perhaps ((RepoPatchV1 prim :> RepoPatchV1 prim) wX wY)
otherCommuteRecursiveMerger (p_old@(Merger _ _ p1' p2') :> pA') = toPerhaps $
  do (pA :> _) <- commuterFLId selfCommuter (mergerUndo p_old :> pA')
     (pAmid :> p1) <- commute (unsafeCoercePEnd p1' :> pA)
     (_ :> pAmido) <- commute (pA :> invert p1)
     guard (pAmido `unsafeCompare` pAmid)
     (p2 :> _) <- commute (invert pAmid :> p2')
     (p2o' :> _) <- commute (pAmid :> p2)
     guard (p2o' `unsafeCompare` p2')
     let p = if p1 `unsafeCompare` p1' && p2 `unsafeCompare` p2'
             then unsafeCoerceP p_old
             else unsafeMerger "0.0" p1 p2
         undo = mergerUndo p
     guard (not $ pA `unsafeCompare` p1) -- special case here...
     (_ :> pAo') <- commuterIdFL selfCommuter (pA :> undo)
     guard (pAo' `unsafeCompare` pA')
     return (pA :> p)
otherCommuteRecursiveMerger _ = Unknown

type CommuteFunction prim = forall wX wY . (RepoPatchV1 prim :> RepoPatchV1 prim) wX wY -> Perhaps ((RepoPatchV1 prim :> RepoPatchV1 prim) wX wY)

{-
A note about mergers and type witnesses
---------------------------------------

The merger code predates the introduction of type witnesses, and because
of its complexity has proved the hardest part of the codebase to retrofit.
Attempting to do this has exposed various places where the code behaves
oddly (e.g. 'putBefore' below); these are likely to be bugs but fixing
them would be potentially disruptive and dangerous as it might change
the existing merge behaviour and thus break existing repositories.

As a result the addition of witnesses to this code has required the
liberal use of unsafe operators. In effect, witnesses bring no safety
in this area; the sole purpose of adding them here was to allow this
code to run as part of a codebase that uses witnesses everywhere else.

A key problem point is the type of the 'Merger' and 'Regrem' constructors
of Patch, where the witnesses seem odd. It is likely that some or many
of the unsafe operations could be removed by finding a better type for
these constructors.
-}


-- Recreates a patch history in reverse.
unwind :: RepoPatchV1 prim wX wY -> Sealed (RL (RepoPatchV1 prim) wX)
unwind (Merger _ unwindings _ _) = Sealed unwindings
unwind p = Sealed (NilRL :<: p)

-- Recreates a patch history in reverse. The patch being unwound is always at
-- the start of the list of patches.
trueUnwind :: PrimPatch prim
    => RepoPatchV1 prim wC wX -> RepoPatchV1 prim wC wD -> Sealed ((RL (RepoPatchV1 prim) :> RepoPatchV1 prim) wX)
trueUnwind p1 p2 =
  let fake_p = Merger NilFL NilRL p1 p2
  in
  case (unwind p1, unwind p2) of
    (Sealed (p1s:<:_),Sealed (p2s:<:_)) ->
         Sealed (unsealFlipped unsafeCoerceP (reconcileUnwindings fake_p p1s (unsafeCoercePEnd p2s)) :<: unsafeCoerceP p1 :> fake_p)
    _ -> error "impossible case"

reconcileUnwindings :: PrimPatch prim
    => RepoPatchV1 prim wA wB -> RL (RepoPatchV1 prim) wX wZ -> RL (RepoPatchV1 prim) wY wZ -> FlippedSeal (RL (RepoPatchV1 prim)) wZ
reconcileUnwindings _ NilRL p2s = FlippedSeal p2s
reconcileUnwindings _ p1s NilRL = FlippedSeal p1s
reconcileUnwindings p (p1s:<:p1) p2s@(tp2s:<:p2) =
    case [(p1s', p2s')|
          p1s'@(_:<:hp1s') <- headPermutationsRL (p1s:<:p1),
          p2s'@(_:<:hp2s') <- headPermutationsRL p2s,
          hp1s' `unsafeCompare` hp2s'] of
    ((p1s':<:p1', p2s':<:_):_) ->
        mapFlipped (:<:p1') $ reconcileUnwindings p p1s' (unsafeCoercePEnd p2s')
    [] -> case reverseFL `fmap` putBefore p1 (reverseRL p2s) of
          Just p2s' -> mapFlipped (:<:p1) $ reconcileUnwindings p p1s p2s'
          Nothing ->
              case fmap reverseFL $ putBefore p2 $
                   reverseRL (p1s:<:p1) of
              Just p1s' -> mapFlipped (:<:p2) $
                           reconcileUnwindings p p1s' tp2s
              Nothing ->
                error $ renderString
                  $ text "in function reconcileUnwindings"
                  $$ text "Original patch:"
                  $$ showPatch_ p
    _ -> error "in reconcileUnwindings"

-- This code seems wrong, shouldn't the commute be invert p1 :> p2 ? And why isn't p1' re-inverted?
-- it seems to have been this way forever:
-- Fri May 23 10:27:04 BST 2003  droundy@abridgegame.org
--    * fix bug in unwind and add docs on unwind algorithm.
putBefore :: PrimPatch prim
    => RepoPatchV1 prim wY wZ -> FL (RepoPatchV1 prim) wX wZ -> Maybe (FL (RepoPatchV1 prim) wY wW)
putBefore p1 (p2:>:p2s) =
    do p1' :> p2' <- commute (unsafeCoerceP p2 :> invert p1)
       _ <- commute (p2' :> p1)
       (unsafeCoerceP p2' :>:) `fmap` putBefore p1' (unsafeCoerceP p2s)
putBefore _ NilFL = Just (unsafeCoerceP NilFL)

instance PrimPatch prim => CommuteNoConflicts (RepoPatchV1 prim) where
  commuteNoConflicts x =
    toMaybe $ msum [ speedyCommute x
                   , everythingElseCommute x
                   ]

instance PrimPatch prim => Conflict (RepoPatchV1 prim) where
  resolveConflicts _ = map mangleOrFail . combineConflicts resolveOne
    where
      resolveOne p | isMerger p = [publicUnravel p]
      resolveOne _ = []

instance PrimPatch prim => Unwind (RepoPatchV1 prim) where
  fullUnwind (PP prim) = mkUnwound NilFL (prim :>: NilFL) NilFL
  fullUnwind (Merger a _ c d) =
    case fullUnwind d of
      Unwound before prim _after ->
        mkUnwound
          (invert (effect c) +>+ before)
          prim
          (invert prim +>+ invert before +>+ effect c +>+ effect a)
  fullUnwind (Regrem a b c d) = invert (fullUnwind (Merger a b c d))

instance PrimPatch prim => Summary (RepoPatchV1 prim) where
  conflictedEffect x
    | isMerger x = mapFL (IsC Conflicted) $ effect x
    | otherwise = mapFL (IsC Okay) $ effect x

-- This type seems wrong - the most natural type for the result would seem to be
-- [Sealed (FL prim wX)], given the type of unwind.
-- However downstream code in darcs convert assumes the wY type, and I was unable
-- to figure out whether this could/should reasonably be changed -- Ganesh 13/4/10
--
-- bf says: the type here is correct, those of unwind and unravel are wrong,
-- because conflict resolution applies to the end of the repo.
publicUnravel :: PrimPatch prim => RepoPatchV1 prim wX wY -> [Sealed (FL prim wY)]
publicUnravel = map (mapSeal unsafeCoercePStart) . unravel

dropAllInverses :: (Commute p, Invert p, Eq2 p) => FL p wX wY -> FL p wX wY
dropAllInverses NilFL = NilFL
dropAllInverses (p :>: ps) =
  let ps' = dropAllInverses ps in
  fromMaybe (p :>: ps') $ removeFL (invert p) ps'

unravel :: PrimPatch prim => RepoPatchV1 prim wX wY -> [Sealed (FL prim wX)]
unravel p = nub $ map (mapSeal (dropAllInverses . concatFL . mapFL_FL effect)) $
            getSupers $ map (mapSeal reverseRL) $ unseal (newUr p) $ unwind p

getSupers :: PrimPatch prim
    => [Sealed (FL (RepoPatchV1 prim) wX)] -> [Sealed (FL (RepoPatchV1 prim) wX)]
getSupers (x:xs) =
    case filter (not.(x `isSuperpatchOf`)) xs of
    xs' -> if any (`isSuperpatchOf` x) xs'
           then getSupers xs'
           else x : getSupers xs'
getSupers [] = []

isSuperpatchOf :: PrimPatch prim
    => Sealed (FL (RepoPatchV1 prim) wX) -> Sealed (FL (RepoPatchV1 prim) wX) -> Bool
Sealed x `isSuperpatchOf` Sealed y | lengthFL y > lengthFL x = False -- should be just an optimisation
Sealed x `isSuperpatchOf` Sealed y = x `iso` y
    where iso :: PrimPatch prim => FL (RepoPatchV1 prim) wX wY -> FL (RepoPatchV1 prim) wX wZ -> Bool
          _ `iso` NilFL = True
          NilFL `iso` _ = False
          a `iso` (b:>:bs) =
              head $ ([as `iso` bs | (ah :>: as) <- simpleHeadPermutationsFL a, IsEq <- [ah =\/= b]] :: [Bool]) ++ [False]

-- | merger takes two patches, (which have been determined to conflict) and
-- constructs a Merger patch to represent the conflict. @p1@ is considered to
-- be conflicting with @p2@ (@p1@ is the "first" patch in the repo ordering),
-- the resulting Merger is therefore a representation of @p2@.
merger :: PrimPatch prim
    => String -> RepoPatchV1 prim wX wY -> RepoPatchV1 prim wX wZ -> Sealed (RepoPatchV1 prim wY)
merger "0.0" p1 p2 = final_p
    where
          sealed_unwindings = trueUnwind p1 p2
          final_p =
            case (sealed_undoit, sealed_unwindings) of
              (Sealed undoit, Sealed unwindings)
                -> Sealed $ Merger undoit ((\(a :> b) -> (a :<: b)) unwindings) p1 p2
          sealed_undoit =
              case (isMerger p1, isMerger p2) of
              (True ,True ) -> case sealed_unwindings of
                                 Sealed (t :> _) -> Sealed $ unsafeCoercePStart $ invertRL t
              (False,False) -> Sealed $ invert p1 :>: NilFL
              (True ,False) -> Sealed NilFL
              (False,True ) -> Sealed $ invert p1 :>: mergerUndo p2
merger g _ _ =
    error $ "Cannot handle mergers other than version 0.0\n"++g
    ++ "\nPlease use darcs optimize --modernize with an older darcs."

instance PrimPatch prim => Effect (RepoPatchV1 prim) where
    effect p@(Merger{}) = dropAllInverses $ effect $ mergerUndo p
    effect p@(Regrem{}) = invert $ effect $ invert p
    effect (PP p) = p :>: NilFL

instance IsHunk prim => IsHunk (RepoPatchV1 prim) where
    isHunk p = do PP p' <- return p
                  isHunk p'

newUr :: PrimPatch prim
    => RepoPatchV1 prim wA wB -> RL (RepoPatchV1 prim) wX wY -> [Sealed (RL (RepoPatchV1 prim) wX)]
newUr p (ps :<: Merger _ _ p1 p2) =
   case filter (\(_:<:pp) -> pp `unsafeCompare` p1) $ headPermutationsRL ps of
   ((ps':<:_):_) -> newUr p (ps':<:unsafeCoercePStart p1) ++ newUr p (ps':<:unsafeCoercePStart p2)
   _ -> error $ renderString $ text "in function newUr"
                 $$ text "Original patch:"
                 $$ showPatch_ p
                 $$ text "Unwound:"
                 $$ vcat (unseal (mapRL showPatch_) $ unwind p)

newUr op ps =
    case filter (\(_:<:p) -> isMerger p) $ headPermutationsRL ps of
    [] -> [Sealed ps]
    (ps':_) -> newUr op ps'

instance Invert prim => Invert (RepoPatchV1 prim) where
    invert (Merger undo unwindings p1 p2)
        = Regrem undo unwindings p1 p2
    invert (Regrem undo unwindings p1 p2)
        = Merger undo unwindings p1 p2
    invert (PP p) = PP (invert p)

instance Eq2 prim => Eq2 (RepoPatchV1 prim) where
    unsafeCompare = eqPatches

instance Eq2 prim => Eq (RepoPatchV1 prim wX wY) where
    (==) = unsafeCompare

eqPatches :: Eq2 prim => RepoPatchV1 prim wX wY -> RepoPatchV1 prim wW wZ -> Bool
eqPatches (PP p1) (PP p2) = unsafeCompare p1 p2
eqPatches (Merger _ _ p1a p1b) (Merger _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches (Regrem _ _ p1a p1b) (Regrem _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches _ _ = False
