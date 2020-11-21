module Darcs.Test.Patch.Arbitrary.Shrink
  ( Shrinkable(..)
  ) where

import Darcs.Prelude

import Darcs.Patch.Commute
import Darcs.Patch.Permutations

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed

-- |This class encapsulates the general concept of shrinking a patch
-- without using any information about the repository state the
-- patch is applied to.
class Shrinkable p where
  -- |Shrink a patch while preserving the start and end contexts.
  shrinkInternally :: p wX wY -> [p wX wY]
  -- |Shrink a patch, preserving the start context, but maybe not the end context.
  shrinkAtEnd :: p wX wY -> [Sealed (p wX)]
  -- |Shrink a patch, preserving the end context, but maybe not the start context.
  shrinkAtStart :: p wX wY -> [FlippedSeal p wY]

instance (Shrinkable p, Shrinkable q) => Shrinkable (p :> q) where
  shrinkInternally (p :> q) =
    ((:> q) <$> shrinkInternally p) ++
    ((p :>) <$> shrinkInternally q)
  shrinkAtEnd (p :> q) = do
    Sealed q' <- shrinkAtEnd q
    return (Sealed (p :> q'))
  shrinkAtStart (p :> q) = do
    FlippedSeal p' <- shrinkAtStart p
    return (FlippedSeal (p' :> q))

instance (Commute p, Shrinkable p) => Shrinkable (FL p) where
  shrinkInternally NilFL = []
  shrinkInternally (p :>: ps) =
    ((:>: ps) <$> shrinkInternally p) ++ ((p :>: ) <$> shrinkInternally ps)

  shrinkAtStart ps = do
    q :> qs <- headPermutationsFL ps
    FlippedSeal qs:map (mapFlipped (:>: qs)) (shrinkAtStart q)

  shrinkAtEnd = map (mapSeal reverseRL) . shrinkAtEnd . reverseFL

instance (Commute p, Shrinkable p) => Shrinkable (RL p) where
  shrinkInternally = map reverseFL . shrinkInternally . reverseRL

  shrinkAtStart = map (mapFlipped reverseFL) . shrinkAtStart . reverseRL

  shrinkAtEnd ps = do
    qs :<: q <- headPermutationsRL ps
    Sealed qs:map (mapSeal (qs :<:)) (shrinkAtEnd q)
