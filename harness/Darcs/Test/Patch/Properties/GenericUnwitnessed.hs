module Darcs.Test.Patch.Properties.GenericUnwitnessed where

import Darcs.Prelude

import qualified Darcs.Test.Patch.Properties.Generic as W
     ( permutivity
     , mergeConsistent, mergeArgumentsConsistent, mergeEitherWay
     , mergeCommute, squareCommuteLaw, coalesceCommute, commuteInverses
     , recommute
     , showRead )
import Darcs.Test.Patch.Arbitrary.Generic ( MightHaveDuplicate )
import Darcs.Test.Patch.Arbitrary.PrimV1 ()

import Darcs.Test.Patch.WSub
import Darcs.Test.Util.TestResult

import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic, displayPatch )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Merge ( Merge )
import Darcs.Util.Printer ( Doc, redText, ($$) )


permutivity :: (ShowPatchBasic wp, Eq2 wp, WSub wp p)
            => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
            -> (p :> p :> p) wA wB -> TestResult
permutivity f = W.permutivity (fmap toW . f . fromW) . toW

mergeEitherWay :: (ShowPatchBasic wp, Eq2 wp, Merge wp, WSub wp p) => (p :\/: p) wX wY -> TestResult
mergeEitherWay = W.mergeEitherWay . toW

commuteInverses :: (Invert wp, ShowPatchBasic wp, Eq2 wp, WSub wp p)
                 => (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                 -> (p :> p) wA wB -> TestResult
commuteInverses f = W.commuteInverses (fmap toW . f . fromW) . toW

recommute :: (ShowPatchBasic wp, MightHaveDuplicate wp, Eq2 wp, WSub wp p)
          => (forall wX wY . ((p :> p) wX wY -> Maybe ((p :> p) wX wY)))
          -> (p :> p) wA wB -> TestResult
recommute f = W.recommute (fmap toW . f . fromW) . toW

mergeCommute :: ( MightHaveDuplicate wp
                , ShowPatchBasic wp
                , Eq2 wp
                , Commute wp
                , Merge wp
                , WSub wp p
                )
             => (p :\/: p) wX wY
             -> TestResult
mergeCommute = W.mergeCommute . toW

mergeConsistent :: (Merge wp, ShowPatchBasic wp, WSub wp p) =>
                           (forall wX wY . p wX wY -> Maybe Doc)
                        -> (p :\/: p) wA wB -> TestResult
mergeConsistent f = W.mergeConsistent (f . fromW) . toW

mergeArgumentsConsistent :: (ShowPatchBasic wp, WSub wp p) =>
                              (forall wX wY . p wX wY -> Maybe Doc)
                           -> (p :\/: p) wA wB -> TestResult
mergeArgumentsConsistent f = W.mergeArgumentsConsistent (f . fromW) . toW

showRead :: (ShowPatchBasic p, ReadPatch p, Eq2 p, Show2 p) => p wX wY -> TestResult
showRead = W.showRead

squareCommuteLaw :: (Invert wp, ShowPatchBasic wp, Eq2 wp, WSub wp p) =>
                             (forall wX wY . (p :> p) wX wY -> Maybe ((p :> p) wX wY))
                          -> (p :> p) wA wB -> TestResult
squareCommuteLaw f = W.squareCommuteLaw (fmap toW . f . fromW) . toW


coalesceCommute :: (forall wX wY . (Prim2 :> Prim2) wX wY -> Maybe (FL Prim2 wX wY))
                -> (Prim2 :> Prim2 :> Prim2) wA wB -> TestResult
coalesceCommute f = W.coalesceCommute (fmap toW . f . fromW) . toW

commuteFails :: ShowPatchBasic p
             => ((p :> p) wX wY -> Maybe ((p :> p) wX wY))
             -> (p :> p) wX wY
             -> TestResult
commuteFails c (x :> y) = case c (x :> y) of
                            Nothing -> succeeded
                            Just (y' :> x') ->
                              failed $ redText "x" $$ displayPatch x $$
                                       redText ":> y" $$ displayPatch y $$
                                       redText "y'" $$ displayPatch y' $$
                                       redText ":> x'" $$ displayPatch x'
