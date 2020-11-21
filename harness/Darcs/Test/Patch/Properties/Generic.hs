--  Copyright (C) 2007 David Roundy
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

module Darcs.Test.Patch.Properties.Generic
    ( invertInvolution
    , inverseComposition
    , invertRollback
    , recommute
    , commuteInverses
    , effectPreserving
    , inverseDoesntCommute
    , permutivity
    , squareCommuteLaw
    , mergeEitherWay
    , showRead
    , mergeEitherWayValid
    , mergeCommute
    , mergeConsistent
    , mergeArgumentsConsistent
    , coalesceEffectPreserving
    , coalesceCommute
    , PatchProperty
    , MergeProperty
    , SequenceProperty
    ) where

import Darcs.Prelude

import Darcs.Test.Patch.RepoModel
    ( ModelOf
    , RepoModel
    , RepoState
    , eqModel
    , maybeFail
    , repoApply
    , showModel
    )
import Darcs.Test.Util.TestResult
    ( TestResult
    , failed
    , maybeFailed
    , rejected
    , succeeded
    )
import Darcs.Test.Patch.WithState ( WithState(..) )
import Darcs.Test.Patch.Arbitrary.Generic
    ( MightBeEmptyHunk(..)
    , MightHaveDuplicate(..)
    , TestablePrim
    )
import Darcs.Test.Patch.Properties.Check ( checkAPatch, Check )

import Control.Monad ( msum )

import Darcs.Patch.Witnesses.Show ( Show2(..), show2 )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show
    ( ShowPatchBasic, displayPatch, showPatch, ShowPatchFor(ForStorage) )
import Darcs.Patch ()
import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch.Commute ( Commute, commute, commuteFL )
import Darcs.Patch.CommuteFn ( CommuteFn )
import Darcs.Patch.Merge ( Merge(merge) )
import Darcs.Patch.Read ( readPatch )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    , FL(..)
    , RL(..)
    , eqFL
    , mapFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Util.Printer ( Doc, renderPS, redText, greenText, ($$), text, vcat )
--import Darcs.ColorPrinter ( traceDoc )

type PatchProperty p = forall wA wB. p wA wB -> TestResult
-- type PairProperty p = forall wA wB. (p :> p) wA wB -> TestResult
type MergeProperty p = forall wA wB. (FL p :\/: FL p) wA wB -> TestResult
type SequenceProperty p = forall wA wB. RL p wA wB -> TestResult

-- | @A^^=A@
invertInvolution :: (Invert p, Eq2 p, ShowPatchBasic p) => p wA wB -> TestResult
invertInvolution p =
  let p' = invert (invert p)
  in case p =\/= p' of
    IsEq  -> succeeded
    NotEq ->
      failed $ redText "p /= p^^, where"
      $$ text "##p=" $$ displayPatch p
      $$ text "##p^^=" $$ displayPatch p'

displayPatchFL :: ShowPatchBasic p => FL p wX wY -> Doc
displayPatchFL = vcat . mapFL displayPatch

-- | @(AB)^ = B^A^@
inverseComposition :: (Invert p, Eq2 p, ShowPatchBasic p)
                   => (p :> p) wX wY -> TestResult
inverseComposition (a :> b) =
  let ab = a:>:b:>:NilFL
      iab = invert ab
      ibia = invert b:>:invert a:>:NilFL
  in case eqFL iab ibia of
    IsEq -> succeeded
    NotEq ->
      failed $ redText "ab^ /= b^a^, where"
      $$ text "##ab=" $$ displayPatchFL ab
      $$ text "##(ab)^=" $$ displayPatchFL iab
      $$ text "##b^a^=" $$ displayPatchFL ibia

-- | @ apply A x = y ==> apply A^ y = x@
invertRollback
  :: ( ApplyState p ~ RepoState model
     , Invert p
     , Apply p
     , ShowPatchBasic p
     , RepoModel model
     , model ~ ModelOf p
     )
  => WithState p wA wB
  -> TestResult
invertRollback (WithState a x b) =
  case maybeFail $ repoApply b (invert x) of
    Nothing -> failed $ redText "x^ not applicable to b."
    Just a' ->
      if a' `eqModel` a
        then
          succeeded
        else
          failed $
            redText "##original repo a:" $$ text (showModel a) $$
            redText "##with patch x:" $$ displayPatch x $$
            redText "##results in b:" $$ text (showModel b) $$
            redText "##but (invert x):" $$ displayPatch (invert x) $$
            redText "##applied to b is a':" $$ text (showModel a') $$
            redText "##which is not equal to a."

-- | recommute   AB ↔ B′A′ if and only if B′A′ ↔ AB
recommute :: (ShowPatchBasic p, Eq2 p, MightHaveDuplicate p)
          => CommuteFn p p
          -> (p :> p) wA wB -> TestResult
recommute c (x :> y) =
    case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x')
      -- this test unfortunately fails on some V2 patches that contain duplicates
      -- after the commute. While in theory the underlying bug should be fixed,
      -- we don't know how to and even if we did, it would probably involve a repository
      -- migration to a new patch type.
      | hasDuplicate y' || hasDuplicate x' -> rejected
      | otherwise ->
       case c (y' :> x') of
         Nothing -> failed (redText "failed, where x" $$ displayPatch x $$
                              redText ":> y" $$ displayPatch y $$
                              redText "y'" $$ displayPatch y' $$
                              redText ":> x'" $$ displayPatch x')
         Just (x'' :> y'') ->
             case y'' =/\= y of
             NotEq -> failed (redText "y'' =/\\= y failed, where x" $$ displayPatch x $$
                              redText ":> y" $$ displayPatch y $$
                              redText "y'" $$ displayPatch y' $$
                              redText ":> x'" $$ displayPatch x' $$
                              redText "x''" $$ displayPatch x'' $$
                              redText ":> y''" $$ displayPatch y'')
             IsEq -> case x'' =/\= x of
                     NotEq -> failed (
                              redText "x'' /= x, where x" $$ displayPatch x $$
                              redText ":> y" $$ displayPatch y $$
                              redText "y'" $$ displayPatch y' $$
                              redText ":> x'" $$ displayPatch x' $$
                              redText "x''" $$ displayPatch x'' $$
                              redText ":> y''" $$ displayPatch y'')
                     IsEq -> succeeded

-- | commuteInverses   AB ↔ B′A′ if and only if B⁻¹A⁻¹ ↔ A′⁻¹B′⁻¹
commuteInverses :: (Invert p, ShowPatchBasic p, Eq2 p)
                => CommuteFn p p
                -> (p :> p) wA wB -> TestResult
commuteInverses c (x :> y) =
    case c (x :> y) of
    Nothing ->
      -- check that inverse commute neither
      case c (invert y :> invert x) of
        Just _ -> failed $
          redText "second commute did not fail"
          $$ redText "x" $$ displayPatch x
          $$ redText "y" $$ displayPatch y
          $$ redText "invert y" $$ displayPatch (invert y)
          $$ redText "invert x" $$ displayPatch (invert x)
        Nothing -> succeeded
    Just (y' :> x') ->
        case c (invert y :> invert x) of
        Nothing -> failed $ redText "second commute failed" $$
                            redText "x" $$ displayPatch x $$ redText "y" $$ displayPatch y $$
                            redText "invert y" $$ displayPatch (invert y) $$ redText "invert x" $$ displayPatch (invert x)
        Just (ix' :> iy') ->
            case invert ix' =/\= x' of
            NotEq -> failed $ redText "invert ix' /= x'" $$
                              redText "x" $$ displayPatch x $$
                              redText "y" $$ displayPatch y $$
                              redText "y'" $$ displayPatch y' $$
                              redText "x'" $$ displayPatch x' $$
                              redText "ix'" $$ displayPatch ix' $$
                              redText "iy'" $$ displayPatch iy' $$
                              redText "invert ix'" $$ displayPatch (invert ix') $$
                              redText "invert iy'" $$ displayPatch (invert iy')
            IsEq -> case y' =\/= invert iy' of
                    NotEq -> failed $ redText "y' /= invert iy'" $$ displayPatch iy' $$ displayPatch y'
                    IsEq -> succeeded

-- | effect preserving  AB <--> B'A' then effect(AB) = effect(B'A')
effectPreserving
  :: ( Apply p
     , MightBeEmptyHunk p
     , RepoModel model
     , model ~ ModelOf p
     , ApplyState p ~ RepoState model
     , ShowPatchBasic p
     )
  => CommuteFn p p
  -> WithState (p :> p) wA wB
  -> TestResult
effectPreserving _ (WithState _ (x :> _) _)
  | isEmptyHunk x = rejected
effectPreserving c (WithState r (x :> y) r') =
  case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x') ->
      case maybeFail $ repoApply r y' of
        Nothing ->
          failed
          $  redText "##x" $$ displayPatch x
          $$ redText "##y" $$ displayPatch y
          $$ redText "##y'" $$ displayPatch y'
          $$ redText "##x'" $$ displayPatch x'
          $$ redText "##y' is not applicable to r"
          $$ displayModel r
        Just r_y' ->
          case maybeFail $ repoApply r_y' x' of
            Nothing ->
              failed
              $  redText "##x" $$ displayPatch x
              $$ redText "##y" $$ displayPatch y
              $$ redText "##y'" $$ displayPatch y'
              $$ redText "##x'" $$ displayPatch x'
              $$ redText "##x' is not applicable to r_y'"
              $$ displayModel r_y'
            Just r_y'x' ->
              if r_y'x' `eqModel` r'
                then succeeded
                else
                  failed
                  $  redText "##x" $$ displayPatch x
                  $$ redText "##y" $$ displayPatch y
                  $$ redText "##y'" $$ displayPatch y'
                  $$ redText "##x'" $$ displayPatch x'
                  $$ redText "##r_y'x'"
                  $$ displayModel r_y'x'
                  $$ redText "##is not equal to r'"
                  $$ displayModel r'
  where
    displayModel = text . showModel

-- | squareCommuteLaw   If AB ↔ B′A′ then A⁻¹B′ ↔ BA′⁻¹
squareCommuteLaw
  :: (Invert p, ShowPatchBasic p, Eq2 p)
  => CommuteFn p p
  -> (p :> p) wA wB
  -> TestResult
squareCommuteLaw c (x :> y) =
  case c (x :> y) of
    Nothing -> rejected
    Just (y' :> x') ->
      case c (invert x :> y') of
        Nothing ->
          failed $
          redText "-------- original (x :> y)" $$
          displayPatch x $$ redText ":>" $$ displayPatch y $$
          redText "-------- result (y' :> x')" $$
          displayPatch y' $$ redText ":>" $$ displayPatch x' $$
          redText "-------- failed commute (invert x :> y')" $$
          displayPatch (invert x) $$ redText ":>" $$ displayPatch y'
        Just (y'' :> ix') ->
          case y'' =\/= y of
            NotEq ->
              failed $ redText "y'' /= y" $$
              redText "x" $$ displayPatch x $$
              redText "y" $$ displayPatch y $$
              redText "y'" $$ displayPatch y' $$
              redText "x'" $$ displayPatch x' $$
              redText "y''" $$ displayPatch y'' $$
              redText "ix'" $$ displayPatch ix'
            IsEq ->
              case x' =\/= invert ix' of
                NotEq ->
                  failed $ redText "x' /= invert ix'" $$
                  redText "x" $$ displayPatch x $$
                  redText "y" $$ displayPatch y $$
                  redText "y'" $$ displayPatch y' $$
                  redText "x'" $$ displayPatch x' $$
                  redText "invert x" $$ displayPatch (invert x) $$
                  redText "y'" $$ displayPatch y' $$
                  redText "invert ix'" $$ displayPatch (invert ix')
                IsEq -> succeeded

permutivity :: (ShowPatchBasic p, Eq2 p)
            => CommuteFn p p
            -> (p :> p :> p) wA wB -> TestResult
permutivity c (x :> y :> z) =
  case c (x :> y) of
   Nothing -> rejected
   Just (y1 :> x1) ->
    case c (y :> z) of
    Nothing -> rejected
    Just (z2 :> y2) ->
      case c (x :> z2) of
      Nothing ->
        case c (x1 :> z) of
          Just _ -> failed $ redText "##partial permutivity:" $$
            redText "##x" $$ displayPatch x $$
            redText "##y" $$ displayPatch y $$
            redText "##z" $$ displayPatch z $$
            redText "##y1" $$ displayPatch y1 $$
            redText "##x1" $$ displayPatch x1 $$
            redText "##z2" $$ displayPatch z2 $$
            redText "##y2" $$ displayPatch y2 $$
            redText "##x :> z2 does not commute, whereas x1 :> z does"
          Nothing -> succeeded
      Just (z3 :> x3) ->
        case c (x1 :> z) of
          Nothing ->
            failed $ redText "##permutivity1:" $$
              redText "##x" $$ displayPatch x $$
              redText "##y" $$ displayPatch y $$
              redText "##z" $$ displayPatch z $$
              redText "##y1" $$ displayPatch y1 $$
              redText "##y2" $$ displayPatch y2 $$
              redText "##failed commute with z of" $$
              redText "##x1" $$ displayPatch x1 $$
              redText "##whereas x commutes with" $$
              redText "##z2" $$ displayPatch z2
          Just (z4 :> x4) ->
            --traceDoc (greenText "third commuted" $$
            --          greenText "about to commute" $$
            --          greenText "y1" $$ displayPatch y1 $$
            --          greenText "z4" $$ displayPatch z4) $
            case c (y1 :> z4) of
            Nothing ->
              failed $ redText "##permutivity2:" $$
                redText "##failed to commute y1 with z4, where" $$
                redText "##x" $$ displayPatch x $$
                redText "##y" $$ displayPatch y $$
                redText "##z" $$ displayPatch z $$
                redText "##y1" $$ displayPatch y1 $$
                redText "##x1" $$ displayPatch x1 $$
                redText "##z2" $$ displayPatch z2 $$
                redText "##y2" $$ displayPatch y2 $$
                redText "##z3" $$ displayPatch z3 $$
                redText "##x3" $$ displayPatch x3 $$
                redText "##z4" $$ displayPatch z4 $$
                redText "##x4" $$ displayPatch x4
            Just (z3_ :> y4)
                | IsEq <- z3_ =\/= z3 ->
                     --traceDoc (greenText "passed z3") $ error "foobar test" $
                     case c (y4 :> x4) of
                     Nothing -> failed $
                        redText "##permutivity5: input was" $$
                        redText "##x" $$ displayPatch x $$
                        redText "##y" $$ displayPatch y $$
                        redText "##z" $$ displayPatch z $$
                        redText "##z3" $$ displayPatch z3 $$
                        redText "##z4" $$ displayPatch z4 $$
                        redText "##failed commute of" $$
                        redText "##y4" $$ displayPatch y4 $$
                        redText "##x4" $$ displayPatch x4 $$
                        redText "##whereas commute of x and y give" $$
                        redText "##y1" $$ displayPatch y1 $$
                        redText "##x1" $$ displayPatch x1
                     Just (x3_ :> y2_)
                          | NotEq <- x3_ =\/= x3 ->
                              failed $
                                redText "##permutivity6: x3_ /= x3" $$
                                redText "##x3_" $$ displayPatch x3_ $$
                                redText "##x3" $$ displayPatch x3
                          | NotEq <- y2_ =/\= y2 ->
                              failed $
                                redText "##permutivity7: y2_ /= y2" $$
                                redText "##y2_" $$ displayPatch y2_ $$
                                redText "##y2" $$ displayPatch y2
                          | otherwise -> succeeded
                | otherwise ->
                    failed $ redText "##permutivity failed" $$
                             redText "##z3" $$ displayPatch z3 $$
                             redText "##z3_" $$ displayPatch z3_

mergeArgumentsConsistent :: (ShowPatchBasic p) =>
                              (forall wX wY . p wX wY -> Maybe Doc)
                           -> (p :\/: p) wA wB -> TestResult
mergeArgumentsConsistent isConsistent (x :\/: y) =
  maybeFailed $
    msum [(\z -> redText "mergeArgumentsConsistent x" $$ displayPatch x $$ z) `fmap` isConsistent x,
          (\z -> redText "mergeArgumentsConsistent y" $$ displayPatch y $$ z) `fmap` isConsistent y]

mergeConsistent :: (ShowPatchBasic p, Merge p) =>
                           (forall wX wY . p wX wY -> Maybe Doc)
                        -> (p :\/: p) wA wB -> TestResult
mergeConsistent isConsistent (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' ->
      maybeFailed $
        msum [(\z -> redText "mergeConsistent x" $$ displayPatch x $$ z) `fmap` isConsistent x,
              (\z -> redText "mergeConsistent y" $$ displayPatch y $$ z) `fmap` isConsistent y,
              (\z -> redText "mergeConsistent x'" $$ displayPatch x' $$ z $$
                     redText "where x' comes from x" $$ displayPatch x $$
                     redText "and y" $$ displayPatch y) `fmap` isConsistent x',
              (\z -> redText "mergeConsistent y'" $$ displayPatch y' $$ z) `fmap` isConsistent y']

-- merge (A\/B) = B'/\A' <==> merge (B\/A) = A'/\B'
--  or, equivalently,
-- merge . swap_par = swap_antipar . merge
--  where swap_par  (A\/B) = B\/A and swap_antipar (A/\B) = B/\A
-- It should not be needed to test this, since it follows from
-- mergeCommute and recommute.
mergeEitherWay :: (Eq2 p, ShowPatchBasic p, Merge p)
               => (p :\/: p) wX wY -> TestResult
mergeEitherWay (x :\/: y) =
  case merge (x :\/: y) of
    y' :/\: x' ->
      case merge (y :\/: x) of
        x'' :/\: y''
          | IsEq <- x'' =\/= x'
          , IsEq <- y'' =\/= y' -> succeeded
          | otherwise ->
            failed $
              redText "##x" $$ displayPatch x $$
              redText "##y" $$ displayPatch y $$
              redText "##y'" $$ displayPatch y' $$
              redText "##x'" $$ displayPatch x' $$
              redText "##x''" $$ displayPatch x'' $$
              redText "##y''" $$ displayPatch y'' $$
              redText "##x'' /= x' or y'' /= y'"

-- merge (A\/B) = B'/\A' ==> AB' <--> BA'
mergeCommute :: (Eq2 p, ShowPatchBasic p, Commute p, Merge p, MightHaveDuplicate p)
             => (p :\/: p) wX wY -> TestResult
mergeCommute (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x'
     -- this test unfortunately fails on some V2 patches that contain duplicates
     -- after the merge. While in theory the underlying bug should be fixed,
     -- we don't know how to and even if we did, it would probably involve a repository
     -- migration to a new patch type.
     | hasDuplicate x' || hasDuplicate y' -> rejected
     | otherwise ->
        case commute (x :> y') of
        Nothing -> failed $ redText "mergeCommute 1" $$
                            redText "x" $$ displayPatch x $$
                            redText "y" $$ displayPatch y $$
                            redText "x'" $$ displayPatch x' $$
                            redText "y'" $$ displayPatch y'
        Just (y_ :> x'_)
            | IsEq <- y_ =\/= y,
              IsEq <- x'_ =\/= x' ->
                      case commute (y :> x') of
                      Nothing -> failed $ redText "mergeCommute 2 failed" $$
                                          redText "x" $$ displayPatch x $$
                                          redText "y" $$ displayPatch y $$
                                          redText "x'" $$ displayPatch x' $$
                                          redText "y'" $$ displayPatch y'
                      Just (x_ :> y'_)
                           | IsEq <- x_ =\/= x,
                             IsEq <- y'_ =\/= y' -> succeeded
                           | otherwise -> failed $ redText "mergeCommute 3" $$
                                                   redText "x" $$ displayPatch x $$
                                                   redText "y" $$ displayPatch y $$
                                                   redText "x'" $$ displayPatch x' $$
                                                   redText "y'" $$ displayPatch y' $$
                                                   redText "x_" $$ displayPatch x_ $$
                                                   redText "y'_" $$ displayPatch y'_
            | otherwise -> failed $ redText "mergeCommute 4" $$
                                    redText "x" $$ displayPatch x $$
                                    redText "y" $$ displayPatch y $$
                                    redText "x'" $$ displayPatch x' $$
                                    redText "y'" $$ displayPatch y' $$
                                    redText "x'_" $$ displayPatch x'_ $$
                                    redText "y_" $$ displayPatch y_


-- | coalesce effect preserving
coalesceEffectPreserving
            :: TestablePrim prim
            => (forall wX wY . (prim :> prim) wX wY -> Maybe (FL prim wX wY))
            -> WithState (prim :> prim) wA wB -> TestResult
coalesceEffectPreserving j (WithState r (a :> b) r') =
  case j (a :> b) of
       Nothing -> rejected
       Just x  -> case maybeFail $ repoApply r x of
                       Nothing  -> failed $ redText "x is not applicable to r."
                                        $$ text (showModel r)
                                        $$ displayPatch x
                                        $$ redText "a:>b"
                                        $$ displayPatch a $$ displayPatch b
                                        $$ redText "r'="
                                        $$ text (showModel r')
                       Just r_x -> if r_x `eqModel` r'
                                      then succeeded
                                      else failed $ redText "r_x /= r', r="
                                        $$ text (showModel r)
                                        $$ redText "a:>b="
                                        $$ displayPatch a $$ displayPatch b
                                        $$ redText "x="
                                        $$ displayPatch x
                                        $$ redText "r'="
                                        $$ text (showModel r')
                                        $$ redText "r_x="
                                        $$ text (showModel r_x)

coalesceCommute
          :: (TestablePrim prim, MightBeEmptyHunk prim)
          => (forall wX wY . (prim :> prim) wX wY -> Maybe (FL prim wX wY))
          -> (prim :> prim :> prim) wA wB -> TestResult
coalesceCommute _ (a :> _ :> _) | isEmptyHunk a = rejected
coalesceCommute j (a :> b :> c) =
    case j (b :> c) of
    Nothing -> rejected
    Just x  ->
      case commuteFL (a :> b :>: c :>: NilFL) of
        Just (b' :>: c' :>: NilFL :> a') ->
          case commuteFL (a :> x) of
            Just (x' :> a'') ->
              case a'' =/\= a' of
                NotEq ->
                  failed $ greenText "a'' =/\\= a' failed"
                    $$ display1
                    $$ display2
                IsEq ->
                  case j (b' :> c') of
                    Nothing ->
                      failed $ greenText "coalesce (b':>c') failed"
                        $$ display1
                        $$ display2
                    Just x'' ->
                      case x' =\/= x'' of
                        NotEq ->
                          failed $ greenText "x' =\\/= x'' failed"
                            $$ display1
                            $$ display2
                            $$ display3
                        IsEq -> succeeded
                      where
                        display3 = redText "## coalesce (b':>c') => x''"
                                   $$ displayPatch x''
              where
                display2 =
                     redText "## commute (a:>x) => x'" $$ displayPatch x'
                  $$ redText "## :> a''" $$ displayPatch a''
            _ -> failed $ greenText "commute a x failed" $$ display1
          where
            display1 =
                 redText "## a" $$ displayPatch a
              $$ redText "## b" $$ displayPatch b
              $$ redText "## c" $$ displayPatch c
              $$ redText "## coalesce (b:>c) => x" $$ displayPatch x
              $$ redText "## commute (a:>b:>c) => a'" $$ displayPatch a'
              $$ redText "## b'" $$ displayPatch b'
              $$ redText "## c'" $$ displayPatch c'
        _ -> rejected

-- note: we would normally use displayPatch in the failure message
-- but that would be very misleading here
showRead :: (Show2 p, Eq2 p, ReadPatch p, ShowPatchBasic p) => p wA wB -> TestResult
showRead p =
  let ps = renderPS (showPatch ForStorage p)
   in case readPatch ps of
        Left e -> failed (redText "unable to read " $$ showPatch ForStorage p $$ text e)
        Right (Sealed p')
          | IsEq <- p' =\/= p -> succeeded
          | otherwise ->
            failed $
            redText "##trouble reading patch p" $$ showPatch ForStorage p $$
            redText "##reads as p'" $$
            showPatch ForStorage p' $$
            redText "##aka" $$
            greenText (show2 p) $$
            redText "##and" $$
            greenText (show2 p')

-- vim: fileencoding=utf-8 :

mergeEitherWayValid
  :: (Check p, Merge p, Invert p, ShowPatchBasic p)
  => (p :\/: p) wX wY
  -> TestResult
mergeEitherWayValid (p1 :\/: p2) =
  case merge (p1 :\/: p2) of
    _ :/\: p1' ->
      case p2 :>: p1' :>: NilFL of
        combo2 ->
          case merge (p2 :\/: p1) of
            _ :/\: p2' ->
              case p1 :>: p2' :>: NilFL of
                combo1
                  | not $ checkAPatch combo1 ->
                      failed $ text "combo1 invalid: p1="
                      $$ displayPatch p1
                      $$ text "p2="
                      $$ displayPatch p2
                      $$ text "combo1="
                      $$ vcat (mapFL displayPatch combo1)
                  | checkAPatch (invert combo1 :>: combo2 :>: NilFL) ->
                      succeeded
                  | otherwise ->
                      failed $ text "merge both ways invalid: p1="
                      $$ displayPatch p1
                      $$ text "p2="
                      $$ displayPatch p2
                      $$ text "combo1="
                      $$ vcat (mapFL displayPatch combo1)
                      $$ text "combo2="
                      $$ vcat (mapFL displayPatch combo2)

inverseDoesntCommute :: (ShowPatchBasic p, Invert p, Commute p)
                     => p wY1 wY2 -> TestResult
inverseDoesntCommute x =
  case commute (x :> invert x) of
    Nothing -> succeeded
    Just (ix' :> x') -> failed $ redText "x:" $$ displayPatch x
      $$ redText "commutes with x^ to ix':" $$ displayPatch ix'
      $$ redText "x':" $$ displayPatch x'
