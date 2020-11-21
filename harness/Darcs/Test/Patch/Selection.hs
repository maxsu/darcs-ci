-- Copyright (C) 2016 G. Hoffmann

module Darcs.Test.Patch.Selection ( testSuite ) where

import Darcs.Prelude

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit ( testCase )

import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..)  )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Patch.V2 ( RepoPatchV2 )
import qualified Darcs.Patch.V2.Prim as V2
import Darcs.Patch.RepoType ( RepoType(..), RebaseType(..) )

import Darcs.UI.SelectChanges
    ( PatchSelectionOptions(..)
    , selectionConfig
    , runSelection
    , WhichChanges(..)
    )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, patchInfoAndPatch )
import Darcs.Patch.Info ( rawPatchInfo )
import Darcs.UI.Options.All
    ( Verbosity(..), WithSummary(..)
    , WithContext(..), SelectDeps(..), MatchFlag(..) )

import Darcs.Test.TestOnly.Instance ()

-- A test module for interactive patch selection.

type Patch = RepoPatchV2 V2.Prim

testSuite :: Test
testSuite = testGroup "Darcs.Patch.Selection" $
  [ dontReadContents whch | whch <- [Last, LastReversed, First, FirstReversed] ]

dontReadContents :: WhichChanges -> Test
dontReadContents whch =
    testCase ("Matching on patch metadata does not open patch contents: " ++ show whch)
      $ do
   let -- here is an FL of patches whose metadata can be read but whose contents
       -- should NEVER be read, otherwise something really bad would happen.
       launchNuclearMissilesPatches = unsafeCoerceP $ lnmPatches [ "P " ++ show i | i <- [1..5::Int] ]
       lnmPatches [] = NilFL
       lnmPatches (n:names) = buildPatch n  :>: lnmPatches names
       buildPatch :: String -> PatchInfoAnd ('RepoType 'NoRebase) Patch wX wY
       buildPatch name = patchInfoAndPatch (rawPatchInfo "1999" name "harness" [] False) (error "Patch content read!")
       pso = PatchSelectionOptions
           { verbosity = Quiet
           , matchFlags = [OnePatch "."]  -- match on every patch
           , interactive = False
           , selectDeps = AutoDeps
           , withSummary = NoSummary
           , withContext = NoContext
           }
       context = selectionConfig whch "select" pso Nothing Nothing
   (unselected :> selected) <- runSelection launchNuclearMissilesPatches context
   -- Forcing selection to happen (at least to the point of knowing whether unselected
   -- and unselected are NilFL or not) should not evaluate the `undefined` inside of our
   -- patches, ie, we don't need to read too much.
   unselected `seq` selected `seq` return ()

