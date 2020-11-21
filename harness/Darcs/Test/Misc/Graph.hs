{- | Calculating the properties of graph algorithms scales very badly
because the specifications aren't optimised (naturally). Exhaustive testing
is a lot more effective than randomized testing in this case because it
avoids computations on large graphs. -}

module Darcs.Test.Misc.Graph ( testSuite ) where

import Darcs.Prelude

import Darcs.Util.Graph
  ( Graph
  , Component
  , genGraphs
  , genComponents
  , prop_ltmis_eq_bkmis
  , prop_ltmis_maximal_independent_sets
  , prop_ltmis_all_maximal_independent_sets
  , prop_components
  )

import Test.Framework
    ( Test
    , plusTestOptions
    , testGroup
    , topt_maximum_generated_tests
    )
import Test.Framework.Providers.LeanCheck ( testProperty )
import Test.LeanCheck

testSuite :: Test
testSuite =
  {- Unfortunately, test-framework is a bit limited in that it doesn't allow
  to scale the number of tests, just to set them to a fixed value. We opt to
  set it to 0x8000 which roughly covers the graphs up to size 6 and
  completes reasonably fast. The estimate is not precise because it doesn't
  account for graphs with more than one component; however, the overall
  error is not big because the majority of graphs have only one component,
  e.g. for graphs of size 6 the average number of components is 1.22. -}
  plusTestOptions (mempty { topt_maximum_generated_tests = Just 0x8000 }) $
  testGroup "Darcs.Util.Graph"
  [ testProperty "ltmis is equivalent to bkmis" prop_ltmis_eq_bkmis
  , testProperty
      "ltmis generates only maximal independent sets"
      prop_ltmis_maximal_independent_sets
  , testProperty
      "ltmis generates all maximal independent sets"
      prop_ltmis_all_maximal_independent_sets
  , testProperty
      "components generates all connected components"
      prop_components
  ]

instance Listable Graph where
  tiers = map genGraphs [0..]

instance Listable Component where
  tiers = map genComponents [0..]
