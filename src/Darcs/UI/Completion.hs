-- | How to complete arguments
{-# LANGUAGE NamedFieldPuns #-}
module Darcs.UI.Completion
    ( fileArgs, knownFileArgs, unknownFileArgs, modifiedFileArgs
    , noArgs, prefArgs
    ) where

import Darcs.Prelude

import Data.List ( (\\), stripPrefix )
import Data.List.Ordered ( nubSort, minus )
import Data.Maybe ( mapMaybe )

import Darcs.Patch ( listTouchedFiles )

import Darcs.Repository.Flags
    ( UseCache(..)
    )
import Darcs.Repository.Prefs
    ( getPreflist
    )
import Darcs.Repository.Job
    ( RepoJob(..)
    , withRepository
    )
import Darcs.Repository.State
    ( readRecordedAndPending
    , readUnrecordedFiltered
    , unrecordedChanges
    , restrictDarcsdir
    , applyTreeFilter
    , TreeFilter(..)
    )

import Darcs.UI.Flags ( DarcsFlag )
import qualified Darcs.UI.Flags as Flags
import qualified Darcs.UI.Options.All as O

import Darcs.Util.File
    ( doesDirectoryReallyExist
    )
import Darcs.Util.Global
    ( darcsdir
    )
import Darcs.Util.Path
    ( AnchoredPath, anchorPath
    , AbsolutePath, toPath, floatSubPath, makeSubPathOf
    )
import Darcs.Util.Tree as Tree
    ( Tree, ItemType(..)
    , expand, expandPath, list, findTree, itemType, emptyTree
    )
import Darcs.Util.Tree.Plain ( readPlainTree )

-- | Return all files available under the original working
-- directory regardless of their repo state.
-- Subdirectories get a separator (slash) appended.
fileArgs :: (AbsolutePath, AbsolutePath)
         -> [DarcsFlag]
         -> [String]
         -> IO [FilePath]
fileArgs (_, orig) _flags args =
  notYetListed args $
  fmap (map anchoredToFilePath . listItems) $
  Tree.expand . applyTreeFilter restrictDarcsdir =<< readPlainTree (toPath orig)

-- | Return all files available under the original working directory that
-- are unknown to darcs but could be added.
-- Subdirectories get a separator (slash) appended.
unknownFileArgs :: (AbsolutePath, AbsolutePath)
                -> [DarcsFlag]
                -> [String]
                -> IO [FilePath]
unknownFileArgs fps flags args = notYetListed args $ do
  let sk = if Flags.includeBoring flags then O.ScanBoring else O.ScanAll
      lfm = Flags.lookForMoves flags
      lfr = Flags.lookForReplaces flags
  RepoTrees {have, known} <- repoTrees O.UseIndex sk lfm lfr
  known_paths <- listHere known fps
  have_paths <- listHere have fps
  return $ map anchoredToFilePath $ nubSort have_paths `minus` nubSort known_paths

-- | Return all files available under the original working directory that
-- are known to darcs (either recorded or pending).
-- Subdirectories get a separator (slash) appended.
knownFileArgs :: (AbsolutePath, AbsolutePath)
              -> [DarcsFlag]
              -> [String]
              -> IO [FilePath]
knownFileArgs fps flags args = notYetListed args $ do
  let (ui, sk, _) = Flags.diffingOpts flags
      lfm = Flags.lookForMoves flags
      lfr = Flags.lookForReplaces flags
  RepoTrees {known} <- repoTrees ui sk lfm lfr
  map anchoredToFilePath <$> listHere known fps

-- | Return all files available under the original working directory that
-- are modified (relative to the recorded state).
-- Subdirectories get a separator (slash) appended.
modifiedFileArgs :: (AbsolutePath, AbsolutePath)
                 -> [DarcsFlag]
                 -> [String]
                 -> IO [FilePath]
modifiedFileArgs fps flags args = notYetListed args $ do
  let (ui, sk, _) = Flags.diffingOpts flags
      lfm = Flags.lookForMoves flags
      lfr = Flags.lookForReplaces flags
  RepoTrees {new} <- repoTrees ui sk lfm lfr
  case uncurry makeSubPathOf fps of
    Nothing -> return []
    Just here ->
      return $ mapMaybe (stripPathPrefix (toPath here)) $ map (anchorPath "") new

-- | Return the available prefs of the given kind.
prefArgs :: String
         -> (AbsolutePath, AbsolutePath)
         -> [DarcsFlag]
         -> [String]
         -> IO [String]
prefArgs name _ _ _ = getPreflist name

-- | Return an empty list.
noArgs :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO [String]
noArgs _ _ _ = return []

-- * unexported helper functions

data RepoTrees m = RepoTrees
  { have  :: Tree m       -- ^ working tree
  , known :: Tree m       -- ^ recorded and pending
  , new :: [AnchoredPath] -- ^ unrecorded paths
  }

repoTrees :: O.UseIndex -> O.ScanKnown -> O.LookForMoves -> O.LookForReplaces
          -> IO (RepoTrees IO)
repoTrees ui sk lfm lfr = do
  inDarcsRepo <- doesDirectoryReallyExist darcsdir
  if inDarcsRepo then
    withRepository NoUseCache $ RepoJob $ \r -> do
      known <- readRecordedAndPending r
      have <- readUnrecordedFiltered r ui sk lfm Nothing
      -- we are only interested in the affected paths so the diff
      -- algorithm is irrelevant
      new <- listTouchedFiles <$> unrecordedChanges (ui, sk, O.MyersDiff) lfm lfr r Nothing
      return $ RepoTrees {..}
  else
    return RepoTrees {have = emptyTree, known = emptyTree, new = []}

-- this is for completion which should give us everything under the original wd
subtreeHere :: Tree IO -> (AbsolutePath, AbsolutePath) -> IO (Maybe (Tree IO))
subtreeHere tree fps =
  case floatSubPath <$> uncurry makeSubPathOf fps of
    Nothing -> do
      return Nothing -- here is no subtree of the repo
    Just here -> do
      flip findTree here <$> expandPath tree here

listHere :: Tree IO
         -> (AbsolutePath, AbsolutePath)
         -> IO [(AnchoredPath, ItemType)]
listHere tree fps = do
  msubtree <- subtreeHere tree fps
  case msubtree of
    Nothing -> return []
    Just subtree -> listItems <$> expand subtree

listItems :: Tree m -> [(AnchoredPath, ItemType)]
listItems = map (\(p, i) -> (p, itemType i)) . Tree.list

anchoredToFilePath :: (AnchoredPath, ItemType) -> [Char]
anchoredToFilePath (path, TreeType) = anchorPath "" path -- ++ "/"
anchoredToFilePath (path, BlobType) = anchorPath "" path

stripPathPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPathPrefix = stripPrefix . addSlash where
  addSlash [] = []
  addSlash xs = xs ++ "/"

-- | Turn an action that creates all possible completions into one
-- that removes already given arguments.
notYetListed :: [String] -> IO [String] -> IO [String]
notYetListed already complete = do
  possible <- complete
  return $ possible \\ already
