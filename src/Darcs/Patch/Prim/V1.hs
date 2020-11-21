{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1 ( Prim ) where

import Darcs.Prelude

import Data.Maybe ( fromMaybe )

import Darcs.Patch.Prim.V1.Apply ()
import Darcs.Patch.Prim.V1.Coalesce ()
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core ( Prim )
import Darcs.Patch.Prim.V1.Details ()
import Darcs.Patch.Prim.V1.Mangle ()
import Darcs.Patch.Prim.V1.Read ()
import Darcs.Patch.Prim.V1.Show ()

import Darcs.Patch.Commute ( Commute(..), commuteFL )
import Darcs.Patch.Invert ( Invert(..), dropInverses )
import Darcs.Patch.Prim.Class
    ( PrimSift(..)
    , PrimClassify
      ( primIsHunk
      , primIsBinary
      , primIsSetpref
      , primIsAddfile
      , primIsAdddir
      )
    , PrimCanonize(tryToShrink)
    )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , (:>)(..)
    , allFL
    , lengthFL
    , reverseFL
    , filterOutFLFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

instance PrimSift Prim where
  siftForPending = v1siftForPending where

    -- | An optimized version of 'siftForPending' that avoids commutation
    -- in case all prim patches are "simple" i.e. hunk, binary, or setpref.
    -- Otherwise it returns the original sequence.
    crudeSift :: forall prim wX wY. PrimClassify prim
              => FL prim wX wY -> FL prim wX wY
    crudeSift xs =
      if isSimple xs
        then filterOutFLFL ishunkbinary xs
        else xs
      where
        ishunkbinary :: prim wA wB -> EqCheck wA wB
        ishunkbinary x
          | primIsHunk x || primIsBinary x = unsafeCoerceP IsEq
          | otherwise = NotEq
        isSimple = allFL $ \x -> primIsHunk x || primIsBinary x || primIsSetpref x

    -- | Alternately 'sift' and 'tryToShrink' until shrinking no longer reduces
    -- the length of the sequence. Here, 'sift' means to commute hunks
    -- and binary patches to the end of the sequence and then drop them.
    v1siftForPending
      :: forall prim wX wY.
         (Commute prim, Invert prim, Eq2 prim, PrimCanonize prim, PrimClassify prim)
      => FL prim wX wY
      -> Sealed (FL prim wX)
    v1siftForPending simple_ps
      -- optimization: no need to sift if only adddir or addfile are present
      | allFL (\p -> primIsAddfile p || primIsAdddir p) oldps = seal oldps
      | otherwise =
          case sift (reverseFL oldps) NilFL of
            Sealed x ->
              let ps = tryToShrink x in
              if (lengthFL ps < lengthFL oldps)
                then v1siftForPending ps
                else seal ps
      where
        oldps = fromMaybe simple_ps $ dropInverses $ crudeSift simple_ps
        -- get rid of any hunk/binary patches that we can commute out the
        -- back (ie. we work our way backwards, pushing the patches down
        -- to the very end and popping them off; so in (addfile f :> hunk)
        -- we can nuke the hunk, but not so in (hunk :> replace)
        sift :: RL prim wA wB -> FL prim wB wC -> Sealed (FL prim wA)
        sift NilRL sofar = seal sofar
        sift (ps :<: p) sofar
          | primIsHunk p || primIsBinary p
          , Just (sofar' :> _) <- commuteFL (p :> sofar) = sift ps sofar'
          | otherwise = sift ps (p :>: sofar)
