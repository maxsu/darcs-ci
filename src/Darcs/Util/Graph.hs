{- The idea of the ltmis algorithm is based on this paper:

Loukakis, E & Tsouros, Constantin. (1981). A depth first search algorithm to
generate the family of maximal independent sets of a graph
lexicographically. Computing. 27. 349-366. 10.1007/BF02277184.

This is basically the same as Bron-Kerbosch but with two special
optimizations, one to avoid needless backtracking and one to avoid needless
branching. For large graphs the gains in efficiency are significant. On my
computer generating all MIS for the first 100000 graphs of size 12 takes
0.757 seconds with ltmis (True,True) and over 10 seconds with bkmis.

-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Darcs.Util.Graph
  ( Graph
  , Vertex
  , VertexSet
  , Component(..)
  -- * Algorithms
  , ltmis
  , bkmis
  , components
  -- * Generating graphs
  , genGraphs
  , genComponents
  -- * Properties
  , prop_ltmis_eq_bkmis
  , prop_ltmis_maximal_independent_sets
  , prop_ltmis_all_maximal_independent_sets
  , prop_components
  ) where

import Control.Monad ( filterM )
import Control.Monad.ST ( runST, ST )

import Data.List ( sort )
import qualified Data.Set as S

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Darcs.Prelude

-- | Vertices are represented as 'Int'.
type Vertex = Int

-- | Set of vertices, represented as a list for efficiency (yes, indeed).
type VertexSet = [Vertex]

-- | Undirected graph represented as a 'V.Vector' of adjacency 'VertexSet's.
type Graph = V.Vector VertexSet

data Component = Component Graph VertexSet deriving Show

-- | The neighbors of a 'Vertex' in a 'Graph'.
neighbours :: Graph -> Vertex -> VertexSet
neighbours g v = g V.! v

has_edge :: Graph -> Vertex -> Vertex -> Bool
has_edge g u v = u `elem` neighbours g v

has_any_edge :: Graph -> VertexSet -> Vertex -> Bool
has_any_edge g vs u = any (has_edge g u) vs

all_vertices :: Graph -> VertexSet
all_vertices g = [0..(gsize g - 1)]

-- | The number of vertices in a 'Graph'.
gsize :: Graph -> Int
gsize v = V.length v

-- * Maximal independent sets

-- | Simple helper type used in the 'ltmis' and 'components' algorithms.
type Helper = U.Vector Bool

-- | Determine the maximal independent sets in a 'Component' of a 'Graph'.
ltmis :: (Bool,Bool) -> Component -> [VertexSet]
ltmis (bt1,bt2) (Component g comp) =
    -- the map reverse is because we use (:) to add vertices to r
    -- when branching
    map reverse $ go [] 0 init_h
  where
    size = gsize g
    init_h = U.replicate (gsize g) True U.// zip comp (repeat False)
    -- h[v] = neighbours g v `intersectsWith` r || v `elem` r || v `notElem` comp
    go :: VertexSet -> Vertex -> Helper -> [VertexSet]
    go r !sep h =
      case candidates sep h of
        [] -> [r]
        br:_ ->
          (if bt1 && done_branching sep' h' then [] else go (br:r) sep' h')
          ++
          (if bt2 && done_backtracking sep' h br then [] else go r sep' h)
          where
            h' = h U.// zip (br : neighbours g br) (repeat True)
            sep' = br + 1

    candidates :: Vertex -> Helper -> VertexSet
    candidates sep h = filter (not . (h U.!)) $ [sep..(size-1)]

    excludes :: Vertex -> Helper -> [Vertex]
    excludes sep h = filter (not . (h U.!)) [0 .. (sep-1)]

    is_candidate :: Vertex -> Helper -> Vertex -> Bool
    is_candidate sep h v = v >= sep && not ((h U.!) v)

    intersects_candidates :: Vertex -> Helper -> VertexSet -> Bool
    intersects_candidates sep h = any (is_candidate sep h)

    -- for some x in X, N(x) does not intersect C
    -- means whatever candidate we add we won't get an MIS
    -- so can stop branching
    done_branching :: Vertex -> Helper -> Bool
    done_branching sep h =
      any (not . intersects_candidates sep h) $ map (neighbours g) $ excludes sep h

    -- if done_backtracking (neighbours g v), then v must
    -- be a member of any MIS containing R
    done_backtracking :: Vertex -> Helper -> Vertex -> Bool
    done_backtracking sep h v = not $ intersects_candidates sep h $ neighbours g v

-- | The classic Bron-Kerbosch algorithm for determining the maximal
-- independent sets in a 'Graph'.
bkmis :: Graph -> [VertexSet]
bkmis g = reverse $ map reverse $ go [] [] (all_vertices g) where
  go r [] [] = [r]
  go r xs cs = loop xs cs where
    loop _ [] = []
    loop xs (c:cs) = loop (c:xs) cs ++ go (c:r) (res c xs) (res c cs)
    res v = filter (not . has_edge g v)

-- * Generating graphs

genGraph :: Monad m => (Int -> Int -> m VertexSet) -> Int -> m Graph
genGraph genSubset = go 0 where
  go _ 0 = return V.empty
  go s n = do -- list monad
    g <- go (s+1) (n-1)
    vs <- genSubset (s+1) (n-1)
    return $ V.modify (\h -> mapM_ (adjust h) vs) (V.cons vs g)
    where
      adjust g i = do
        vs <- MV.read g (i-s)
        MV.write g (i-s) (s:vs)

-- | Enumerate all (simple) graphs of a given size (number of vertices).
genGraphs :: Int -> [Graph]
genGraphs = genGraph subsets where
  -- Subsets of the n elements [s..(s+n-1)] (each subset is ordered)
  subsets _ 0 = return []
  subsets s n = do
    vs <- subsets (s+1) (n-1)
    [vs,s:vs]

genComponents :: Int -> [Component]
genComponents n = do
  g <- genGraphs n
  components g

-- * Connected components

-- | Split a 'Graph' into connected components. For efficiency we don't
-- represent the result as a list of Graphs, but rather of 'VertexSet's.
components :: Graph -> [Component]
components g = reverse $ map (Component g) $ runST go where
  size = gsize g
  go :: ST s [VertexSet]
  go = do
    mh <- MU.replicate size False
    loop 0 mh []
  loop v mh r
    | v == size = return r
    | otherwise = do
      c <- new_component v
      if null c
        then loop (v + 1) mh r
        else loop (v + 1) mh (c : r)
    where
      new_component v = do
        visited <- MU.read mh v
        if visited
          then return []
          else do
            -- mark v as visited
            MU.write mh v True
            cs <- mapM new_component (neighbours g v)
            return $ v : concat cs

-- * Properties

-- | Whether a 'VertexSet' is independent i.e. no edge exists between any
-- two of its vertices.
prop_is_independent_set :: Graph -> VertexSet -> Bool
prop_is_independent_set g vs = all (not . has_any_edge g vs) vs

-- | Whether a 'VertexSet' is maximally independent i.e. it is independent
-- and no longer independent if we add any other vertex.
prop_is_maximal_independent_set :: Component -> VertexSet -> Bool
prop_is_maximal_independent_set (Component g c) vs =
    prop_is_independent_set g vs &&
    all (has_any_edge g vs) other_vertices
  where
    other_vertices = filter (`notElem` vs) c

-- | Whether 'ltmis' is equivalent to 'bkmis'.
prop_ltmis_eq_bkmis :: Graph -> Bool
prop_ltmis_eq_bkmis g =
  ltmis (True, True) (Component g (all_vertices g)) == bkmis g

-- | Whether 'ltmis' generates only maximal independent sets.
prop_ltmis_maximal_independent_sets :: Component -> Bool
prop_ltmis_maximal_independent_sets sg =
  all (prop_is_maximal_independent_set sg) (ltmis (True, True) sg)

-- | Whether 'ltmis' generates /all/ maximal independent sets.
prop_ltmis_all_maximal_independent_sets :: Component -> Bool
prop_ltmis_all_maximal_independent_sets sg@(Component _ c) =
    all (not . prop_is_maximal_independent_set sg) other_subsets
  where
    mis = ltmis (True, True) sg
    all_subsets = powerset c
    other_subsets = filter (`notElem` mis) all_subsets

-- | Whether a list of 'VertexSet's of a 'Graph' is a partition of
-- the set of all its vertices.
prop_is_partition :: Graph -> [VertexSet] -> Bool
prop_is_partition g cs = sort (concat cs) == all_vertices g

-- | Whether there is no edge between a 'VertexSet' of a 'Graph' and the rest
-- of the 'Graph'.
prop_self_contained :: Graph -> VertexSet -> Bool
prop_self_contained g c =
  S.unions (map (S.fromList . neighbours g) c) `S.isSubsetOf` S.fromList c

-- | Whether a 'VertexSet' of a 'Graph' is connected.
prop_connected :: Graph -> VertexSet -> Bool
prop_connected g = not . any (prop_self_contained g) . proper_non_empty_subsets
  where
    proper_non_empty_subsets = filter (not . null) . tail . powerset

-- | Whether a 'VertexSet' is a connected component of the 'Graph'.
prop_connected_component :: Component -> Bool
prop_connected_component (Component g vs) =
  prop_self_contained g vs && prop_connected g vs

-- | Complete specification of the 'components' function.
prop_components :: Graph -> Bool
prop_components g =
    all prop_connected_component cs &&
      prop_is_partition g (map vertices cs) && all (== g) (map graph cs)
  where
    vertices (Component _ vs) = vs
    graph (Component g _) = g
    cs = components g

powerset :: VertexSet -> [VertexSet]
powerset = map sort . filterM (const [True, False])
