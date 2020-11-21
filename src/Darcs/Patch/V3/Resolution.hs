{- | Conflict resolution for 'RepoPatchV3' -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V3.Resolution () where

import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Darcs.Prelude

import Darcs.Patch.Commute ( commuteFL )
import Darcs.Patch.Conflict ( Conflict(..), ConflictDetails(..), mangleOrFail )
import Darcs.Patch.Ident
    ( Ident(..)
    , SignedId(..)
    , StorableId(..)
    , findCommonFL
    )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Prim.WithName ( PrimWithName, wnPatch )
import Darcs.Patch.Show hiding ( displayPatch )
import Darcs.Patch.V3.Contexted
    ( Contexted
    , ctxId
    , ctxNoConflict
    , ctxToFL
    )
import Darcs.Patch.V3.Core ( RepoPatchV3(..), (-|) )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    , FL(..)
    , Fork(..)
    , RL(..)
    , (+>+)
    , mapFL_FL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )
import Darcs.Patch.Witnesses.Show ( Show2 )

import Darcs.Util.Graph ( Vertex, components, ltmis )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , redText
    , renderString
    )

-- * Conflict Resolution

{- This gives an overview of the algorithm for marking conflicts.

The goal is to calculate the markup for a trailing RL of patches, usually
the ones we are going to add to our repo. But since in V3 we store only the
direct conflicts, not the transitive set, we also require the full context
of all previous patches. This is needed because we may need to traverse a
(hopefully small, trailing) part of it in order to find out whether a
conflict with some older patch has been resolved or not.

The markup presents each /transitive/ unresolved conflict in the form of a
set of alternative changes that all apply at the end of the repo, such that
each alternative conflicts with all others. Constructing these alternatives
is the main part of the algorithm.

Our first goal is to construct what we call the /conflict graph/. Its
vertices are contexted named prims starting at the end of the repo. An edge
exists between two such vertices iff they conflict. So this is an undirected
graph.

Finding the vertices is the task of 'findVertices', see its documentation
for details about how this is done. We then call 'findEdges' which
determines the edges by calling 'conflictsWith' for each pair of vertices.
The graph is returned as a list of 'Node's, that is, vertices plus their
adjacency sets.

Next we partition the graph into connected components, since these are what
makes up one transitive conflict.

For each component we determine its maximal independent sets. These are
defined as the maximal subsets of vertices with the property that none of
its elements are adjacent to each other. This is done by the function
'alternatives'. Since none of the elements of an independent set conflict,
we can now convert them to plain FLs and merge them cleanly.

The result of merging all maximal independent sets in a component gives us
one set of alternatives, which is then passed as input to 'mangleUnravelled'
to generate the markup. -}

instance (SignedId name, StorableId name, PrimPatch prim) =>
         Conflict (RepoPatchV3 name prim) where
  resolveConflicts context =
      resolveComponents . findEdges . findVertices context
    where
      resolveComponents :: [Node name prim wX] -> [ConflictDetails prim wX]
      resolveComponents = map (mangleOrFail . map mergeThem) . alternatives
      mergeThem :: [Contexted (PrimWithName name prim) wX] -> Sealed (FL prim wX)
      mergeThem = mapSeal (mapFL_FL wnPatch) . mergeList . map ctxToFL

-- | A single 'Node' in the conflict graph. The 'neighbors' are those that we
-- know are in conflict with 'self'.
data Node name prim wY = Node
  { self :: Contexted (PrimWithName name prim) wY
  , neighbors :: S.Set (Contexted (PrimWithName name prim) wY)
  }

deriving instance (Show name, Show2 prim) => Show (Node name prim wY)

{- | Find the set of vertices of the conflict graph by searching the history
for unresolved conflicts. The history is split into an initial 'RL' of
patches (the context) and a trailing 'RL' of patches we are interested in.

We maintain the following state: a list of contexted patches, which will
become the resulting vertices of the conflict graph; and a set of patch
identifiers. The latter serves as markers for patches further back in the
past that we have become interested in during our traversal. We maintain the
invariant that this set never contains the identifier of any patch we have
already traversed.

Any conflictor in the trailing 'RL' is a possible candidate for a vertex;
and likewise any other patch anywhere in the history which we marked as
interesting. Every patch we encounter when traversing the history is
unmarked by removing its identifier from the marker set. The traversal
terminates when the trailing 'RL' has been fully traversed and the marker
set is empty; that is, when there are no more patches to encounter that
might interest us.

If we enounter a candidate, we try to commute it to the head; if that
succeeds, then its commuted version must be a conflictor. (Either it was a
conflictor to begin with, in which case it remains one; or it is a patch
that a later conflictor conflicted with, and that means it must itself
become conflicted when commuted to the head.) We then add the contexted
patch that the conflictor represents to the result. We also mark any patch
that the candidate conflicts with as interesting by adding its identifier to
the marker set. In order to maintain our invariant, we must extract the set
of conflicts from the patch /in its uncommuted form/. (If we took them from
the commuted version, then we might mark patches that we already traversed.)

Candidate patches that cannot be commuted to the head are ignored. We do
this uniformly for every candidate, whether it is a 'Conflictor' or a
'Prim'. The rationale for this is that a patch that some other patch depends
on is either part of a conflict that has been resolved; or else it would be
subsumed by another patch in the same component of the conflict graph that
depends on it and therefore already has it in its context.

The last point is a bit subtle. RepoPatchV1 explicitly removes any vertex
from the graph that another vertex depends on (the function there has the
beautiful and expressive name 'getSupers';-). By uniformly ignoring any
patch we cannot commute to the head we achieve the same result implicitly.
-}

findVertices
  :: forall name prim wO wX wY
   . (SignedId name, StorableId name, PrimPatch prim)
  => RL (RepoPatchV3 name prim) wO wX
  -> RL (RepoPatchV3 name prim) wX wY
  -> [Contexted (PrimWithName name prim) wY]
findVertices context patches = go S.empty [] context patches NilFL where
  go :: S.Set name
     -> [Contexted (PrimWithName name prim) wY]
     -> RL (RepoPatchV3 name prim) wO wA
     -> RL (RepoPatchV3 name prim) wA wB
     -> FL (RepoPatchV3 name prim) wB wY
     -> [Contexted (PrimWithName name prim) wY]
  go check done cs (ps :<: p) passedby
    | isConflicted p || ident p `S.member` check
    , Just (_ :> Conflictor _ _ cp) <- commuteFL (p :> passedby) =
        go (conflicts p <> ident p -| check) (cp : done) cs ps (p :>: passedby)
    | otherwise =
        go (ident p -| check) done cs ps (p :>: passedby)
  go check done _ NilRL _
    | S.null check = done
  go check done (cs :<: p) NilRL passedby
    | ident p `S.member` check
    , Just (_ :> Conflictor _ _ cp) <- commuteFL (p :> passedby) =
        go (conflicts p <> ident p -| check) (cp : done) cs NilRL (p :>: passedby)
    | otherwise =
        go (ident p -| check) done cs NilRL (p :>: passedby)
  go _ _ NilRL NilRL _ = error "autsch, hit the bottom"

  isConflicted Conflictor{} = True
  isConflicted Prim{} = False

  conflicts (Conflictor _ x _) = S.map ctxId x
  conflicts _ = S.empty

-- | Note that 'ctxNoConflict' also regards dependent contexted prims as
-- "conflicting". That is, if @q@ depends on @p@, then
-- 
-- prop> 'ctxNoConflict' ('ctx' p) ('ctxAdd' p ('ctx' q) == 'False'
-- 
-- This is what we need here, too, in order to avoid conflicts between
-- the separately mangled components.
conflictsWith
  :: (SignedId name, PrimPatch prim)
  => Contexted (PrimWithName name prim) wX
  -> Contexted (PrimWithName name prim) wX
  -> Bool
conflictsWith cp cq = not (ctxNoConflict cp cq)

-- | Determine the conflict graph from a set of contexted prims.
-- This calls 'conflictsWith' for every pair of elements and from that
-- builds a list of 'Node's repesenting this graph.
--
-- TODO: optimize this to calculate only one triangle of the adjacency
-- matrix, then complete the graph by adding inverted edges.
findEdges
  :: (SignedId name, PrimPatch prim)
  => [Contexted (PrimWithName name prim) wY] -> [Node name prim wY]
findEdges = fromVertexSet . S.fromList
  where
    -- Note we could as well fold over the original input list,
    -- but going via the set has the advantage that the result is
    -- now sorted and therefore independent of the order of patches
    -- in the repo.
    fromVertexSet vs = foldr go [] vs
      where
        go cp ns = Node cp (S.filter (conflictsWith cp) vs) : ns

-- | The input is a list of nodes with no duplicates representing a connected
-- component of the conflict graph. The list contains an element for
-- each node in the graph, but the conflicts (edges) may refer to non-nodes.
-- The output is a list of the maximal independent sets of the conflict graph:
-- each one can be merged cleanly.
alternatives
  :: SignedId name
  => [Node name prim wX]
  -> [[[Contexted (PrimWithName name prim) wX]]]
alternatives nodes =
    map (map fromVertexSet . ltmis (True, True)) $ components graph
  where
    -- a map from indexes (of type Vertex) to nodes
    from_index = V.fromList $ map self nodes
    -- a map from prim IDs (representing nodes) to indexes
    to_index = M.fromList $ zip (map (ctxId . self) nodes) [(0::Vertex) ..]
    -- the full component of the conflict graph with each node
    -- represented by an index of type Vertex
    graph = V.fromList $ map adj_list nodes
    -- the adjacency list of a single node
    adj_list =
      catMaybes . map (flip M.lookup to_index . ctxId) . S.toList . neighbors
    fromVertexSet = map (from_index V.!)

-- This is similar to the mergeList in Darcs.Patch.CommuteNoConflicts
-- but not the same since we have PatchIds.
mergeList
  :: (SignedId name, StorableId name, PrimPatch p)
  => [Sealed (FL (PrimWithName name p) wX)] -> Sealed (FL (PrimWithName name p) wX)
mergeList = foldr mergeTwo (Sealed NilFL)
  where
    mergeTwo (Sealed ps) (Sealed qs) =
      case findCommonFL ps qs of
        Fork com ps' qs' ->
          case cleanMerge (ps' :\/: qs') of
            Just (qs'' :/\: _) -> Sealed $ com +>+ ps' +>+ qs''
            Nothing ->
              error $ renderString
                $ redText "resolutions conflict:"
                $$ displayPatch ps
                $$ redText "conflicts with"
                $$ displayPatch qs

-- We only use displayPatch for error messages here, so it makes sense
-- to use the storage format that contains the patch names.
displayPatch :: ShowPatchBasic p => p wX wY -> Doc
displayPatch p = showPatch ForStorage p
