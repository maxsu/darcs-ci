{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.ShowDependencies ( showDeps ) where

import Darcs.Prelude

import qualified Data.Map.Strict as M
import Data.Maybe( fromJust, fromMaybe )
import qualified Data.Set as S

import Darcs.Repository ( RepoJob(..), readRepo, withRepositoryLocation )

import Darcs.UI.Flags ( DarcsFlag, getRepourl, useCache )
import Darcs.UI.Options ( oid, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, findRepository, withStdOpts )
import Darcs.UI.Commands.Util ( matchRange )
import Darcs.UI.Completion ( noArgs )

import Darcs.Util.Hash ( sha1short, showAsHex )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer
    ( Doc
    , (<+>)
    , ($+$)
    , formatText
    , formatWords
    , hsep
    , prefixLines
    , putDocLn
    , quoted
    , renderString
    , text
    , vcat
    )
import Darcs.Util.Progress ( beginTedious, endTedious, progress, tediousSize )

import Darcs.Patch.Commute ( Commute, commuteFL )
import Darcs.Patch.Ident ( PatchId, Ident(..) )
import Darcs.Patch.Info ( PatchInfo, piName, makePatchname )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , FL(..)
    , RL(..)
    , reverseFL
    , lengthFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..) )

showDepsDescription :: String
showDepsDescription = "Generate the graph of dependencies."

showDepsHelp :: Doc
showDepsHelp =
  formatWords
    [ "This command creates a graph of the dependencies between patches."
    , "The output format is the Dot Language, see"
    , "https://www.graphviz.org/doc/info/lang.html. The resulting graph"
    , "is transitively reduced, in other words,"
    , "it contains only the direct dependencies, not the indirect ones."
    ]
  $+$ formatWords
    [ "By default all patches in your repository are considered. You can"
    , "limit this to a range of patches using patch matching options, see"
    , "`darcs help patterns` and the options avaiable for this command."
    , "For instance, to visualize the dependencies between all patches"
    , "since the last tag, do:"
    ]
  $+$ "    darcs show dependencies --from-tag=. | dot -Tpdf -o FILE.pdf"
  $+$ formatWords
    [ "This command can take a very(!) long time to compute its result,"
    , "depending on the number of patches in the selected range. For N"
    , "patches it needs to do on the order of N^3 commutations in the"
    , "worst case."
    ]

showDeps :: DarcsCommand
showDeps = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "dependencies"
    , commandHelp = showDepsHelp
    , commandDescription = showDepsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = depsCmd
    , commandPrereq = findRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showDepsBasicOpts
    , commandDefaults = defaultFlags showDepsOpts
    , commandCheckOptions = ocheck showDepsOpts
    }
  where
    showDepsBasicOpts = O.matchRange
    showDepsOpts = showDepsBasicOpts `withStdOpts` oid

progressKey :: String
progressKey = "Determining dependencies"

depsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
depsCmd _ opts _ = do
    let repodir = fromMaybe "." (getRepourl opts)
    withRepositoryLocation (useCache ? opts) repodir $ RepoJob $ \repo -> do
        Sealed2 range <- matchRange (O.matchRange ? opts) <$> readRepo repo
        beginTedious progressKey
        tediousSize progressKey (lengthFL range)
        putDocLn $ renderDepsGraphAsDot $ depsGraph $ reverseFL range
        endTedious progressKey

-- | A 'M.Map' from 'PatchId's to 'Deps'.
type DepsGraph p = M.Map (PatchId p) (Deps p)

-- | A pair of (direct, indirect) dependencies. For the result we need only the
-- direct dependencies. We store the indirect ones as an optimization to avoid
-- doing commutes for which we already know that they cannot succeed. Note that
-- the two sets are always disjoint.
type Deps p = (S.Set (PatchId p), S.Set (PatchId p))

-- | Determine the 'DepsGraph' of an 'RL' of patches.
depsGraph :: forall p wX wY. (Commute p, Ident p) => RL p wX wY -> DepsGraph p
depsGraph NilRL = M.empty
depsGraph (ps :<: p) =
  M.insert (ident p) (foldDeps ps (p :>: NilFL) NilFL (S.empty, S.empty)) m
  where
    -- First recurse on the context. The result now has all the 'Deps' for
    -- all patches preceding p.
    m = depsGraph ps
    -- Lookup all (direct and indirect) dependencies of a patch in a given
    -- 'DepthGraph'
    allDeps j = uncurry S.union . fromJust . M.lookup j
    -- Add all (direct and indirect) dependencies of a patch to a given set
    -- assuming 'm' already
    addDeps j = S.insert j . S.union (allDeps j m)
    -- Add direct and indirect dependencies of a patch, assuming that the
    -- graph has already been constructed for all patches in the context.
    foldDeps :: RL p wA wB -> FL p wB wC -> FL p wC wD -> Deps p -> Deps p
    foldDeps NilRL _ _ acc = progress progressKey acc
    foldDeps (qs :<: q) p_and_deps non_deps acc@(direct, indirect)
      -- If we already know we indirectly depend on q, then there is
      -- nothing left to do. Note that (j `S.member` direct) is impossible.
      | j `S.member` indirect = foldDeps qs (q :>: p_and_deps) non_deps acc
      -- If q commutes past p_and_deps then we don't depend on it
      | Just (p_and_deps' :> q') <- commuteFL (q :> p_and_deps) =
        foldDeps qs p_and_deps' (q' :>: non_deps) acc
      -- We have a new dependency which must be a direct one, so add it to
      -- 'direct' and all its dependencies to 'indirect'. The invariant that
      -- direct and indirect are disjoint is maintained because neither the
      -- direct and indirect deps of a patch contain its own 'PatchId'.
      | otherwise =
        foldDeps qs (q :>: p_and_deps) non_deps (S.insert j direct, addDeps j indirect)
      where
        j = ident q

-- | Render a 'DepthGraph' in the Dot Language format. This function
-- considers only the direct dependencies.
renderDepsGraphAsDot :: M.Map PatchInfo (S.Set PatchInfo, S.Set PatchInfo) -> Doc
renderDepsGraphAsDot g = vcat ["digraph {", indent body, "}"]
  where
    indent = prefixLines ("  ")
    body = vcat
      [ "graph [rankdir=LR];"
      , "node [imagescale=true];"
      , vcat (map showNode (map fst pairs))
      , vcat (map showEdges pairs)
      ]
    pairs = M.toList $ M.map fst g
    showEdges (i, ds)
      | S.null ds = mempty
      | otherwise =
          hsep [showID i, "->", "{" <> hsep (map showID (S.toList ds)) <> "}"]
    showNode i = showID i <+> "[label=" <> showLabel i <> "]"
    showID = quoted . showAsHex . sha1short . makePatchname
    showLabel i = text $ show $ renderString $ formatText 20 [piName i]
