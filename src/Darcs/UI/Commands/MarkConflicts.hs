--  Copyright (C) 2002-2003,2005 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}

module Darcs.UI.Commands.MarkConflicts ( markconflicts ) where

import Darcs.Prelude

import System.Exit ( exitSuccess )
import Data.List.Ordered ( nubSort, isect )
import Control.Monad ( when, unless, void )

import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Path ( AbsolutePath, AnchoredPath, anchorPath )
import Darcs.Util.Printer
    ( Doc, pathlist, putDocLnWith, text, redText, debugDocLn, vsep, (<+>), ($$) )
import Darcs.Util.Printer.Color ( fancyPrinters )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , withStdOpts
    , nodefaults
    , amInHashedRepository
    , putInfo
    , putFinished
    )
import Darcs.UI.Commands.Util ( filterExistingPaths )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.Flags
    ( DarcsFlag, diffingOpts, verbosity, dryRun, umask
    , useCache, pathSetFromArgs )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository.Flags ( UpdatePending (..) )
import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    , applyToWorking
    , readRepo
    , unrecordedChanges )

import Darcs.Patch ( invert, listTouchedFiles, effectOnPaths )
import Darcs.Patch.Show
import Darcs.Patch.TouchesFiles ( chooseTouching )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , patchsetConflictResolutions
    , warnUnmangled
    )

-- * The mark-conflicts command

markconflictsDescription :: String
markconflictsDescription =
 "Mark unresolved conflicts in working tree, for manual resolution."

markconflictsHelp :: Doc
markconflictsHelp = text $ unlines
 ["Darcs requires human guidance to unify changes to the same part of a"
 ,"source file.  When a conflict first occurs, darcs will add the"
 ,"initial state and both choices to the working tree, delimited by the"
 ,"markers `v v v`, `=====`,  `* * *` and `^ ^ ^`, as follows:"
 ,""
 ,"    v v v v v v v"
 ,"    Initial state."
 ,"    ============="
 ,"    First choice."
 ,"    *************"
 ,"    Second choice."
 ,"    ^ ^ ^ ^ ^ ^ ^"
 ,""
 ,"However, you might revert or manually delete these markers without"
 ,"actually resolving the conflict.  In this case, `darcs mark-conflicts`"
 ,"is useful to show where are the unresolved conflicts.  It is also"
 ,"useful if `darcs apply` or `darcs pull` is called with"
 ,"`--allow-conflicts`, where conflicts aren't marked initially."
 ,""
 ,"Unless you use the `--dry-run` flag, any unrecorded changes to the"
 ,"affected files WILL be lost forever when you run this command!"
 ,"You will be prompted for confirmation before this takes place."
 ]

markconflicts :: DarcsCommand
markconflicts = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "mark-conflicts"
    , commandHelp = markconflictsHelp
    , commandDescription = markconflictsDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = markconflictsCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc markconflictsAdvancedOpts
    , commandBasicOptions = odesc markconflictsBasicOpts
    , commandDefaults = defaultFlags markconflictsOpts
    , commandCheckOptions = ocheck markconflictsOpts
    }
  where
    markconflictsBasicOpts
      = O.useIndex
      ^ O.repoDir
      ^ O.diffAlgorithm
      ^ O.dryRunXml
    markconflictsAdvancedOpts = O.umask
    markconflictsOpts = markconflictsBasicOpts `withStdOpts` markconflictsAdvancedOpts

markconflictsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
markconflictsCmd fps opts args = do
  paths <- maybeToOnly <$> pathSetFromArgs fps args
  debugDocLn $ "::: paths =" <+>  (text . show) paths
  withRepoLock (dryRun ? opts) (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \_repository -> do

{-
    What we do here:
    * read the unrecorded changes (all of them)
    * extract functions representing path rename effects from unrecorded
    * convert argument paths to pre-pending
    * read conflict resolutions that touch pre-pending argument paths
    * affected paths = intersection of paths touched by resolutions
                       and pre-pending argument paths
    * for these paths, revert pending changes
    * apply the (filtered, see above) conflict resolutions

    Technical side-note:
    Ghc can't handle pattern bindings for existentials. So 'let' is out,
    one has to use 'case expr of var ->' or 'do var <- return expr'.
    Case is clearer but do-notation does not increase indentation depth.
    So we use case for small-scope bindings and <-/return when the scope
    is a long do block.
-}

    let (useidx, scan, _) = diffingOpts opts
        verb = verbosity ? opts
    classified_paths <-
      traverse (filterExistingPaths _repository verb useidx scan O.NoLookForMoves) paths

    unrecorded <- unrecordedChanges (diffingOpts opts)
      O.NoLookForMoves O.NoLookForReplaces
      _repository (fromOnly Everything)

    let forward_renames = effectOnPaths unrecorded
        backward_renames = effectOnPaths (invert unrecorded)
        existing_paths = fmap snd classified_paths
        pre_pending_paths = fmap backward_renames existing_paths
    debugDocLn $ "::: pre_pending_paths =" <+> (text . show) pre_pending_paths

    r <- readRepo _repository
    Sealed res <- case patchsetConflictResolutions r of
      conflicts -> do
        -- FIXME this should warn only about unmangled conflicts
        -- involving the file paths we care about
        warnUnmangled conflicts
        Sealed mangled_res <- return $ mangled conflicts
        let raw_res_paths = pathSet $ listTouchedFiles mangled_res
        debugDocLn $ "::: raw_res_paths =" <+>  (text . show) raw_res_paths
        return $ chooseTouching (fromOnly pre_pending_paths) mangled_res
    let res_paths = pathSet $ listTouchedFiles res
    debugDocLn $ "::: res_paths =" <+>  (text . show) res_paths

    let affected_paths = res_paths `isectPathSet` pre_pending_paths
    debugDocLn $ "::: affected_paths =" <+>  (text . show) affected_paths

    when (affected_paths == Only []) $ do
      putInfo opts "No conflicts to mark."
      exitSuccess

    to_revert <- unrecordedChanges (diffingOpts opts)
      O.NoLookForMoves O.NoLookForReplaces
      _repository (fromOnly affected_paths)

    let post_pending_affected_paths = forward_renames <$> affected_paths
    putInfo opts $ "Marking conflicts in:" <+> showPathSet post_pending_affected_paths <> "."

    debugDocLn $ "::: to_revert =" $$ vsep (mapFL displayPatch to_revert)
    debugDocLn $ "::: res = " $$ vsep (mapFL displayPatch res)
    when (O.yes (dryRun ? opts)) $ do
        putInfo opts $ "Conflicts will not be marked: this is a dry run."
        exitSuccess

    _repository <- case to_revert of
      NilFL -> return _repository
      _ -> do
        -- TODO:
        -- (1) create backups for all files where we revert changes
        -- (2) try to add the reverted stuff to the unrevert bundle
        -- after (1) and (2) is done we can soften the warning below
        putDocLnWith fancyPrinters $
          "Warning: This will revert all unrecorded changes in:"
          <+> showPathSet post_pending_affected_paths <> "."
          $$ redText "These changes will be LOST."
        confirmed <- promptYorn "Are you sure? "
        unless confirmed exitSuccess

{-      -- copied from Revert.hs, see comment (2) above
        debugMessage "About to write the unrevert file."
        case commute (norevert:>p) of
          Just (p':>_) -> writeUnrevert repository p' recorded NilFL
          Nothing -> writeUnrevert repository (norevert+>+p) recorded NilFL
        debugMessage "About to apply to the working tree."
-}

        let to_add = invert to_revert
        addToPending _repository (O.useIndex ? opts) to_add
        applyToWorking _repository (verbosity ? opts) to_add
    withSignalsBlocked $
      do addToPending _repository (O.useIndex ? opts) res
         void $ applyToWorking _repository (verbosity ? opts) res
    putFinished opts "marking conflicts"

-- * Generic 'PathSet' support

{- $SupportCode

What follows is generic support code for working with argument path lists
that are used to restrict operations to a subset of the working or pristine
tree. The rest of Darcs uses two types for this:

 * @'Maybe' ['SubPath']@

 * @'Maybe' ['FilePath']@

The problem with both is the contra-intuitive name 'Nothing', which here
stands for 'Everything'. To make the intended use clearer, we use the 'Only'
type instead (which is is isomorphic to 'Maybe') and the synonym 'PathSet'
defined below.

These abstractions should get their own module (or become integrated into
Darcs.Util.Path) if and when someone decides to reuse it elsewhere. The
functionality provided is intentionally minimal and light-weight.
-}

-- | 'Only' is isomorphic to 'Maybe' but with the opposite semantics.
--
-- About the name: I like the data constructor names, they are pretty
-- suggestive. The data type name is up for grabs; a possible alternative
-- is @AtMost@.
data Only a = Everything | Only a deriving (Eq, Ord, Show)

instance Functor Only where
  fmap _ Everything = Everything
  fmap f (Only x) = Only (f x)

instance Foldable Only where
  foldMap _ Everything = mempty
  foldMap f (Only x) = f x

instance Traversable Only where
  traverse _ Everything = pure Everything
  traverse f (Only x) = Only <$> f x

-- | This is mostly for conversion to legacy APIs
fromOnly :: Only a -> Maybe a
fromOnly Everything = Nothing
fromOnly (Only x) = Just x

maybeToOnly :: Maybe a -> Only a
maybeToOnly Nothing = Everything
maybeToOnly (Just x) = Only x

{- | A set of repository paths. 'Everything' means every path in the repo,
it usually originates from an empty list of path arguments. The list of
'AnchoredPath's is always kept in sorted order with no duplicates.

It uses lists because the number of elements is expected to be small.
-}
type PathSet a = Only [a]

-- | Intersection of two 'PathSet's
isectPathSet :: Ord a => PathSet a -> PathSet a -> PathSet a
isectPathSet Everything ys = ys
isectPathSet xs Everything = xs
isectPathSet (Only xs) (Only ys) = Only (isect xs ys)

{-
-- | Union of two 'PathSet's
union :: PathSet -> PathSet -> PathSet
union Everything ys = Everything
union xs Everything = Everything
union (Only xs) (Only ys) = Only (union xs ys)
-}

pathSet :: Ord a => [a] -> PathSet a
pathSet = Only . nubSort

-- | Convert a 'PathSet' to a 'Doc'. Uses the English module
-- to generate a nicely readable list of file names.
showPathSet :: PathSet AnchoredPath -> Doc
showPathSet Everything = text "all paths"
showPathSet (Only xs) = pathlist (map (anchorPath "") xs)
