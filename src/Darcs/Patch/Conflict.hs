--  Copyright (C) 2002-2003 David Roundy, 2010 Ganesh Sittampalam
module Darcs.Patch.Conflict
    ( Conflict(..)
    , ConflictDetails(..)
    , Mangled
    , Unravelled
    , mangleOrFail
    , combineConflicts
    ) where

import Darcs.Prelude

import Darcs.Patch.CommuteFn ( commuterIdFL )
import Darcs.Patch.CommuteNoConflicts ( CommuteNoConflicts(..) )
import Darcs.Patch.Permutations ()
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Prim ( PrimMangleUnravelled(..), Mangled, Unravelled )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..), (:>)(..) )

data ConflictDetails prim wX =
  ConflictDetails {
    conflictMangled :: Maybe (Mangled prim wX),
    conflictParts :: Unravelled prim wX
  }

mangleOrFail :: PrimMangleUnravelled prim
             => Unravelled prim wX -> ConflictDetails prim wX
mangleOrFail parts =
  ConflictDetails {
    conflictMangled = mangleUnravelled parts,
    conflictParts = parts
  }

class Conflict p where
    -- | The first parameter is a context containing all patches
    -- preceding the ones for which we want to calculate the conflict
    -- resolution, which is the second parameter.
    -- Each element of the result list represents the resolution
    -- of one maximal set of transitively conflicting alternatives,
    -- in other words, a connected subset of the conflict graph.
    -- But the elements themselves must not conflict with each other,
    -- guaranteeing that they can be cleanly merged into a single 'FL' of prims.
    resolveConflicts :: RL p wO wX -> RL p wX wY -> [ConflictDetails (PrimOf p) wY]

-- | By definition, a conflicting patch is resolved if another patch
-- (that is not itself conflicted) depends on the conflict. If the
-- representation of conflicts is self-contained as it is for V1 and V2,
-- then we can calculate the maximal set of conflicting alternatives for
-- a conflict separately for each conflictor at the end of a repo.
-- This function can then be used to lift this to an 'RL' of patches.
--
-- So, when looking for conflicts in a list of patches, we go
-- through the whole list looking for individual patches that represent
-- a conflict. But then we try to commute them past all the
-- patches we've already seen. If we fail, i.e. there's something
-- that depends on the conflict, then we forget about the conflict;
-- this is the Nothing case of the 'commuteNoConflictsFL' call.
-- Otherwise the patch is now in the correct position to extract the
-- conflicting alternatives.
combineConflicts
    :: forall p wX wY. CommuteNoConflicts p
    => (forall wA wB. p wA wB -> [Unravelled (PrimOf p) wB])
    -> RL p wX wY -> [Unravelled (PrimOf p) wY]
combineConflicts resolveOne x = rcs x NilFL
  where
    rcs :: RL p wX wM -> FL p wM wY -> [Unravelled (PrimOf p) wY]
    rcs NilRL _ = []
    rcs (ps :<: p) passedby
      | null (resolveOne p) = seq passedby rest -- TODO why seq here?
      | otherwise =
        case commuterIdFL commuteNoConflicts (p :> passedby) of
          Just (_ :> p') -> resolveOne p' ++ rest
          Nothing -> rest
      where
        rest = rcs ps (p :>: passedby)
