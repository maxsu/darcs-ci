{-# LANGUAGE CPP #-}
-- Copyright (C) 2009 Petr Rockai
--           (C) 2012 José Neder
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Darcs.Repository.State
    ( restrictSubpaths, restrictBoring, TreeFilter(..), restrictDarcsdir
    -- * Diffs
    , unrecordedChanges
    -- * Trees
    , readRecorded, readUnrecorded, readRecordedAndPending, readWorking
    , readPendingAndWorking, readUnrecordedFiltered
    -- * Index
    , readIndex, updateIndex, invalidateIndex, UseIndex(..), ScanKnown(..)
    -- * Utilities
    , filterOutConflicts
    -- * Pending-related functions that depend on repo state
    , addPendingDiffToPending, addToPending
    ) where

import Darcs.Prelude

import Control.Monad ( when, foldM, forM )
import Control.Monad.State ( StateT, runStateT, get, put, liftIO )
import Control.Exception ( catch, IOException )
import Data.Maybe ( isJust )
import Data.Ord ( comparing )
import Data.List ( sortBy, union, delete )
import Text.Regex( matchRegex )

import System.Directory( removeFile, doesFileExist, doesDirectoryExist, renameFile )
import System.FilePath
    ( (</>)
#if mingw32_HOST_OS
    , (<.>)
#endif
    )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( catchIOError )

import qualified Data.ByteString as B
    ( ByteString, readFile, writeFile, empty, concat )
import qualified Data.ByteString.Char8 as BC
    ( pack, unpack )
import qualified Data.ByteString.Lazy as BL ( toChunks )

import Darcs.Patch ( RepoPatch, PrimOf, sortCoalesceFL
                   , PrimPatch, maybeApplyToTree
                   , tokreplace, forceTokReplace, move )
import Darcs.Patch.Named ( anonymous )
import Darcs.Patch.Apply ( ApplyState, applyToTree, effectOnPaths )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+)
                                     , (:>)(..), reverseRL, reverseFL
                                     , mapFL, concatFL, toFL, nullFL )
import Darcs.Patch.Witnesses.Eq ( EqCheck(IsEq, NotEq) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, unFreeLeft, mapSeal
                                    , freeGap, emptyGap, joinGap, FreeLeft, Gap(..) )
import Darcs.Patch.Commute ( commuteFL )
import Darcs.Patch.Permutations ( partitionConflictingFL, genCommuteWhatWeCanRL )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.TokenReplace ( breakToTokens, defaultToks )

import Darcs.Repository.Flags ( UseIndex(..), ScanKnown(..), DiffAlgorithm(..)
                              , UpdatePending(..), LookForMoves(..), LookForReplaces(..) )

import Darcs.Repository.InternalTypes ( Repository, repoFormat, repoLocation )
import Darcs.Repository.Format(formatHas, RepoProperty(NoWorkingDir))
import qualified Darcs.Repository.Pending as Pending
import Darcs.Repository.Prefs ( filetypeFunction, boringRegexps )
import Darcs.Repository.Diff ( treeDiff )
import Darcs.Repository.Inventory ( peekPristineHash, getValidHash )
import Darcs.Repository.Paths
    ( pristineDirPath
    , hashedInventoryPath
    , oldPristineDirPath
    , oldCurrentDirPath
    , patchesDirPath
    , indexPath
    , indexInvalidPath
    )

import Darcs.Util.Path
    ( AnchoredPath
    , anchorPath
    , filterPaths
    , inDarcsdir
    , parents
    , movedirfilename
    )
import Darcs.Util.Hash( Hash( NoHash ) )
import Darcs.Util.Tree( Tree, restrict, FilterTree, expand, emptyTree, overlay, find
                      , ItemType(..), itemType, readBlob, modifyTree, findFile, TreeItem(..)
                      , makeBlobBS, expandPath )
import qualified Darcs.Util.Tree.Plain as PlainTree ( readPlainTree )
import Darcs.Util.Tree.Hashed
    ( darcsTreeHash, readDarcsHashed, decodeDarcsHash, decodeDarcsSize )
import qualified Darcs.Util.Index as I
import qualified Darcs.Util.Tree as Tree
import Darcs.Util.Index ( listFileIDs, getFileID )

#define TEST_INDEX 0

#if TEST_INDEX
import Control.Monad ( unless )
import Darcs.Util.Path ( displayPath )
import Darcs.Util.Tree ( list )
#endif

newtype TreeFilter m = TreeFilter { applyTreeFilter :: forall tr . FilterTree tr m => tr m -> tr m }

-- | From a repository and a list of AnchoredPath's, construct a filter that can be
-- used on a Tree (recorded or unrecorded state) of this repository. This
-- constructed filter will take pending into account, so the subpaths will be
-- translated correctly relative to pending move patches.
restrictSubpaths :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository rt p wR wU wT -> [AnchoredPath]
                 -> IO (TreeFilter m)
restrictSubpaths repo paths = do
  Sealed pending <- Pending.readPending repo
  restrictSubpathsAfter pending repo paths

-- | Like 'restrictSubpaths' but with the pending patch passed as a parameter.
-- The 'Repository' parameter is not used, we need it only to avoid
-- abiguous typing of @p@.
restrictSubpathsAfter :: (RepoPatch p, ApplyState p ~ Tree)
                      => FL (PrimOf p) wR wP
                      -> Repository rt p wR wU wT
                      -> [AnchoredPath]
                      -> IO (TreeFilter m)
restrictSubpathsAfter pending _repo paths = do
  let paths' = paths `union` effectOnPaths pending paths
      restrictPaths :: FilterTree tree m => tree m -> tree m
      restrictPaths = Tree.filter (filterPaths paths')
  return (TreeFilter restrictPaths)

-- note we assume pending starts at the recorded state
maybeRestrictSubpaths :: (RepoPatch p, ApplyState p ~ Tree)
                      => FL (PrimOf p) wR wP
                      -> Repository rt p wR wU wT
                      -> Maybe [AnchoredPath]
                      -> IO (TreeFilter m)
maybeRestrictSubpaths pending repo =
  maybe (return $ TreeFilter id) (restrictSubpathsAfter pending repo)

-- | Construct a 'TreeFilter' that removes any boring files that are not also
-- contained in the argument 'Tree'.
--
-- The standard use case is for the argument to be the recorded state, possibly
-- with further patches applied, so as not to discard any files already known
-- to darcs. The result is usually applied to the full working state.
restrictBoring :: Tree m -> IO (TreeFilter m)
restrictBoring guide = do
  boring <- boringRegexps
  let boring' p | inDarcsdir p = False
      boring' p = not $ any (\rx -> isJust $ matchRegex rx p') boring
          where p' = anchorPath "" p
      restrictTree :: FilterTree t m => t m -> t m
      restrictTree = Tree.filter $ \p _ -> case find guide p of
                                             Nothing -> boring' p
                                             _ -> True
  return (TreeFilter restrictTree)

-- | Construct a Tree filter that removes any darcs metadata files the
-- Tree might have contained.
restrictDarcsdir :: TreeFilter m
restrictDarcsdir = TreeFilter $ Tree.filter $ \p _ -> not (inDarcsdir p)

{- |
For a repository and an optional list of paths (when 'Nothing', take
everything) compute a (forward) list of prims (i.e. a patch) going from the
recorded state of the repository (pristine) to the unrecorded state of the
repository (the working tree + pending). When a list of paths is given, at
least the files that live under any of these paths in either recorded or
unrecorded will be included in the resulting patch. NB. More patches may be
included in this list, eg. the full contents of the pending patch. This is
usually not a problem, since selectChanges will properly filter the results
anyway.

This also depends on the options given:

--look-for-moves: Detect pending file moves using the index. The resulting
  patches are added to pending and taken into consideration, when filtering
  the tree according to the given path list.

--look-for-adds: Include files in the working state that do not exist in the
  recorded + pending state.

--include-boring: Include even boring files.

--look-for-replaces: Detect pending replace patches. Like detected moves,
  these are added to the pending patch. Note that, like detected moves,
  these are mere proposals for the user to consider or reject.

--ignore-times: Disables index usage completely -- for each file, we read
  both the unrecorded and the recorded copy and run a diff on them. This is
  very inefficient, although in extremely rare cases, the index could go out
  of sync (file is modified, index is updated and file is modified again
  within a single second).

  Note that use of the index is also disabled when we detect moves or
  replaces, since this implies that the index is out of date.
-}
unrecordedChanges :: (RepoPatch p, ApplyState p ~ Tree)
                  => (UseIndex, ScanKnown, DiffAlgorithm)
                  -> LookForMoves
                  -> LookForReplaces
                  -> Repository rt p wR wU wR
                  -> Maybe [AnchoredPath] -> IO (FL (PrimOf p) wR wU)
unrecordedChanges dopts lfm lfr r paths = do
  (pending :> working) <- readPendingAndWorking dopts lfm lfr r paths
  return $ sortCoalesceFL (pending +>+ working)

-- Implementation note: it is important to do things in the right order: we
-- first have to read the pending patch, then detect moves, then detect adds,
-- then detect replaces.
readPendingAndWorking :: (RepoPatch p, ApplyState p ~ Tree)
                      => (UseIndex, ScanKnown, DiffAlgorithm)
                      -> LookForMoves
                      -> LookForReplaces
                      -> Repository rt p wR wU wR
                      -> Maybe [AnchoredPath]
                      -> IO ((FL (PrimOf p) :> FL (PrimOf p)) wR wU)
readPendingAndWorking _ _ _ r _ | formatHas NoWorkingDir (repoFormat r) = do
  IsEq <- return $ workDirLessRepoWitness r
  return (NilFL :> NilFL)
readPendingAndWorking (useidx, scan, diffalg) lfm lfr repo mbpaths = do
  (pending_tree, working_tree, (pending :> moves)) <-
    readPendingAndMovesAndUnrecorded repo useidx scan lfm mbpaths
  (pending_tree_with_replaces, Sealed replaces) <-
    getReplaces lfr diffalg repo pending_tree working_tree
  ft <- filetypeFunction
  wrapped_diff <- treeDiff diffalg ft pending_tree_with_replaces working_tree
  case unFreeLeft wrapped_diff of
    Sealed diff -> do
      return $ unsafeCoercePEnd $ pending :> (moves +>+ replaces +>+ diff)

readPendingAndMovesAndUnrecorded
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wR
  -> UseIndex
  -> ScanKnown
  -> LookForMoves
  -> Maybe [AnchoredPath]
  -> IO ( Tree IO             -- pristine with (pending + moves)
        , Tree IO             -- working
        , (FL (PrimOf p) :> FL (PrimOf p)) wR wU -- pending :> moves
        )
readPendingAndMovesAndUnrecorded repo useidx scan lfm mbpaths = do
  (pending_tree, Sealed pending) <- readPending repo
  moves <- getMoves lfm repo mbpaths
  let pending' = pending +>+ moves
  relevant <- maybeRestrictSubpaths pending' repo mbpaths
  pending_tree' <-
    applyTreeFilter relevant <$> applyToTree moves pending_tree
  let useidx' = if nullFL moves then useidx else IgnoreIndex
  index <-
    applyToTree moves =<< readIndexOrPlainTree repo useidx relevant pending_tree
  working_tree <- filteredWorking repo useidx' scan relevant index pending_tree'
  return (pending_tree', working_tree, unsafeCoercePEnd (pending :> moves))

-- | @filteredWorking useidx scan relevant index pending_tree@ reads the
-- working tree and filters it according to options and @relevant@ file paths.
-- The @pending_tree@ is understood to have @relevant@ already applied and is
-- used (only) if @useidx == 'IgnoreIndex'@ and @scan /= 'ScanBoring'@ to act as
-- a guide for filtering the working tree.
filteredWorking :: Repository rt p wR wU wR
                -> UseIndex
                -> ScanKnown
                -> TreeFilter IO
                -> Tree IO
                -> Tree IO
                -> IO (Tree IO)
filteredWorking repo useidx scan relevant index pending_tree =
  applyTreeFilter restrictDarcsdir <$> applyTreeFilter relevant <$> do
    case useidx of
      UseIndex ->
        case scan of
          ScanKnown -> return index
          ScanAll -> do
            nonboring <- restrictBoring index
            plain <- applyTreeFilter nonboring <$> readPlainTree repo
            return $ plain `overlay` index
          ScanBoring -> do
            plain <- readPlainTree repo
            return $ plain `overlay` index
      IgnoreIndex ->
        case scan of
          ScanKnown -> do
            guide <- expand pending_tree
            restrict guide <$> readPlainTree repo
          ScanAll -> do
            guide <- expand pending_tree
            nonboring <- restrictBoring guide
            applyTreeFilter nonboring <$> readPlainTree repo
          ScanBoring -> readPlainTree repo

-- | Witnesses the fact that in the absence of a working tree, we
-- pretend that the working dir updates magically to the tentative state.
workDirLessRepoWitness :: Repository rt p wR wU wT -> EqCheck wU wT
workDirLessRepoWitness r
 | formatHas NoWorkingDir (repoFormat r) = unsafeCoerceP IsEq
 | otherwise                             = NotEq

-- | Obtains a Tree corresponding to the "recorded" state of the repository:
-- this is the same as the pristine cache, which is the same as the result of
-- applying all the repository's patches to an empty directory.
readRecorded :: Repository rt p wR wU wT -> IO (Tree IO)
readRecorded _repo = do
  hashed <- doesFileExist hashedInventoryPath
  if hashed
     then do inv <- B.readFile hashedInventoryPath
             let pris = peekPristineHash inv
                 hash = decodeDarcsHash $ BC.pack $ getValidHash pris
                 size = decodeDarcsSize $ BC.pack $ getValidHash pris
             when (hash == NoHash) $
                 fail $ "Bad pristine root: " ++ getValidHash pris
             readDarcsHashed pristineDirPath (size, hash)
     else do have_pristine <- doesDirectoryExist $ oldPristineDirPath
             have_current <- doesDirectoryExist $ oldCurrentDirPath
             case (have_pristine, have_current) of
               (True, _) -> PlainTree.readPlainTree $ oldPristineDirPath
               (False, True) -> PlainTree.readPlainTree $ oldCurrentDirPath
               (_, _) -> fail "No pristine tree is available!"

-- | Obtains a Tree corresponding to the "unrecorded" state of the repository:
-- the modified files of the working tree plus the "pending" patch.
-- The optional list of paths allows to restrict the query to a subtree.
--
-- Limiting the query may be more efficient, since hashes on the uninteresting
-- parts of the index do not need to go through an up-to-date check (which
-- involves a relatively expensive lstat(2) per file.
readUnrecorded :: (RepoPatch p, ApplyState p ~ Tree)
               => Repository rt p wR wU wR
               -> UseIndex
               -> Maybe [AnchoredPath]
               -> IO (Tree IO)
readUnrecorded repo useidx mbpaths = do
#if TEST_INDEX
  t1 <- expand =<< readUnrecordedFiltered repo useidx ScanKnown NoLookForMoves mbpaths
  (pending_tree, Sealed pending) <- readPending repo
  relevant <- maybeRestrictSubpaths pending repo mbpaths
  t2 <- readIndexOrPlainTree repo useidx relevant pending_tree
  assertEqualTrees "indirect" t1 "direct" t2
  return t1
#else
  expand =<< readUnrecordedFiltered repo useidx ScanKnown NoLookForMoves mbpaths
#endif

#if TEST_INDEX
assertEqualTrees :: String -> Tree m -> String -> Tree m -> IO ()
assertEqualTrees n1 t1 n2 t2 =
  unless (t1 `eqTree` t2) $
    fail $ "Trees are not equal!\n" ++ showTree n1 t1 ++ showTree n2 t2

eqTree :: Tree m -> Tree m -> Bool
eqTree t1 t2 = map fst (list t1) == map fst (list t2)

showTree :: String -> Tree m -> String
showTree name tree = unlines (name : map (("  "++) . displayPath . fst) (list tree))
#endif

readIndexOrPlainTree :: (ApplyState p ~ Tree, RepoPatch p)
                     => Repository rt p wR wU wR
                     -> UseIndex
                     -> TreeFilter IO
                     -> Tree IO
                     -> IO (Tree IO)
#if TEST_INDEX
readIndexOrPlainTree repo useidx treeFilter pending_tree = do
  indexTree <-
    I.updateIndex =<< applyTreeFilter treeFilter <$> readIndex repo
  plainTree <- do
    guide <- expand pending_tree
    expand =<< applyTreeFilter treeFilter . restrict guide <$> readPlainTree repo
  assertEqualTrees "index tree" indexTree "plain tree" plainTree
  return $
    case useidx of
      UseIndex -> indexTree
      IgnoreIndex -> plainTree
#else
readIndexOrPlainTree repo UseIndex treeFilter pending_tree =
  (I.updateIndex =<< applyTreeFilter treeFilter <$> readIndex repo)
    `catchIOError` \e -> do
      hPutStrLn stderr ("Warning, cannot access the index:\n" ++ show e)
      readIndexOrPlainTree repo IgnoreIndex treeFilter pending_tree
readIndexOrPlainTree repo IgnoreIndex treeFilter pending_tree = do
  guide <- expand pending_tree
  expand =<< applyTreeFilter treeFilter . restrict guide <$> readPlainTree repo
#endif

-- | A variant of 'readUnrecorded' that takes the UseIndex and ScanKnown
-- options into account, similar to 'readPendingAndWorking'. We are only
-- interested in the resulting tree, not the patch, so the 'DiffAlgorithm' option
-- is irrelevant.
readUnrecordedFiltered :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository rt p wR wU wR
                       -> UseIndex
                       -> ScanKnown
                       -> LookForMoves
                       -> Maybe [AnchoredPath] -> IO (Tree IO)
readUnrecordedFiltered repo useidx scan lfm mbpaths = do
  (_, working_tree, _) <-
    readPendingAndMovesAndUnrecorded repo useidx scan lfm mbpaths
  return working_tree

-- | Obtains the relevant (according to the given filter) part of the working tree.
readWorking :: TreeFilter IO -> IO (Tree IO)
readWorking relevant =
  expand =<<
  (applyTreeFilter relevant . applyTreeFilter restrictDarcsdir <$>
   PlainTree.readPlainTree ".")

-- | Obtains the recorded 'Tree' with the pending patch applied.
readRecordedAndPending :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository rt p wR wU wR -> IO (Tree IO)
readRecordedAndPending repo = fst `fmap` readPending repo

-- | Obtains the recorded 'Tree' with the pending patch applied, plus
--   the pending patch itself. The pending patch should start at the
--   recorded state (we even verify that it applies, and degrade to
--   renaming pending and starting afresh if it doesn't).
readPending :: (RepoPatch p, ApplyState p ~ Tree)
            => Repository rt p wR wU wR
            -> IO (Tree IO, Sealed (FL (PrimOf p) wR))
readPending repo = do
  pristine <- readRecorded repo
  Sealed pending <- Pending.readPending repo
  catch ((\t -> (t, seal pending)) <$> applyToTree pending pristine) $
    \(err :: IOException) -> do
       putStrLn $ "Yikes, pending has conflicts! " ++ show err
       putStrLn "Stashing the buggy pending as _darcs/patches/pending_buggy"
       renameFile (patchesDirPath </> "pending")
                  (patchesDirPath </> "pending_buggy")
       return (pristine, seal NilFL)

-- | Mark the existing index as invalid. This has to be called whenever the
-- listing of pristine changes and will cause darcs to update the index next
-- time it tries to read it. (NB. This is about files added and removed from
-- pristine: changes to file content in either pristine or working are handled
-- transparently by the index reading code.)
invalidateIndex :: t -> IO ()
invalidateIndex _ = B.writeFile indexInvalidPath B.empty

readIndex :: (RepoPatch p, ApplyState p ~ Tree)
          => Repository rt p wR wU wR -> IO I.Index
readIndex repo = do
  (invalid, exists, formatValid) <- checkIndex
  if not exists || invalid || not formatValid
     then do pris <- readRecordedAndPending repo
             idx <- I.updateIndexFrom indexPath darcsTreeHash pris
             when invalid $ removeFile indexInvalidPath
             return idx
     else I.readIndex indexPath darcsTreeHash

updateIndex :: (RepoPatch p, ApplyState p ~ Tree)
            => Repository rt p wR wU wR -> IO ()
updateIndex repo = do
  (invalid, _, _) <- checkIndex
  pris <- readRecordedAndPending repo
  _ <- I.updateIndexFrom indexPath darcsTreeHash pris
  when invalid $ removeFile indexInvalidPath

checkIndex :: IO (Bool, Bool, Bool)
checkIndex = do
  invalid <- doesFileExist $ indexInvalidPath
  exists <- doesFileExist indexPath
  formatValid <- if exists
                     then I.indexFormatValid indexPath
                     else return True
  when (exists && not formatValid) $ do
-- TODO this conditional logic (rename or delete) is mirrored in
-- Darcs.Util.Index.updateIndexFrom and should be refactored
#if mingw32_HOST_OS
    renameFile indexPath (indexPath <.> "old")
#else
    removeFile indexPath
#endif
  return (invalid, exists, formatValid)

-- |Remove any patches (+dependencies) from a sequence that
-- conflict with the recorded or unrecorded changes in a repo
filterOutConflicts
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wR     -- ^Repository itself, used for grabbing
                                  --  unrecorded changes
  -> FL (PatchInfoAnd rt p) wX wR -- ^Recorded patches from repository, starting from
                                  --  same context as the patches to filter
  -> FL (PatchInfoAnd rt p) wX wZ -- ^Patches to filter
  -> IO (Bool, Sealed (FL (PatchInfoAnd rt p) wX))
                                  -- ^True iff any patches were removed,
                                  --  possibly filtered patches
filterOutConflicts repository us them
     = do -- Note: use of anonymous is benign here since we only try to merge cleanly
          unrec <- fmap n2pia . anonymous
                     =<< unrecordedChanges (UseIndex, ScanKnown, MyersDiff)
                          NoLookForMoves NoLookForReplaces repository Nothing
          them' :> rest <-
            return $ partitionConflictingFL them (us +>+ unrec :>: NilFL)
          return (check rest, Sealed them')
  where check :: FL p wA wB -> Bool
        check NilFL = False
        check _ = True

-- | Automatically detect file moves using the index.
-- TODO: This function lies about the witnesses.
getMoves :: forall rt p wR wU wB prim.
            (RepoPatch p, ApplyState p ~ Tree, prim ~ PrimOf p)
         => LookForMoves
         -> Repository rt p wR wU wR
         -> Maybe [AnchoredPath]
         -> IO (FL prim wB wB)
getMoves NoLookForMoves _ _ = return NilFL
getMoves YesLookForMoves repository files =
    mkMovesFL <$> getMovedFiles repository files
  where
    mkMovesFL [] = NilFL
    mkMovesFL ((a,b,_):xs) = move a b :>: mkMovesFL xs

    getMovedFiles :: Repository rt p wR wU wR
                  -> Maybe [AnchoredPath]
                  -> IO [(AnchoredPath, AnchoredPath, ItemType)]
    getMovedFiles repo fs = do
        old <- sortBy (comparing snd) <$> (listFileIDs =<< readIndex repo)
        nonboring <- restrictBoring emptyTree
        let addIDs = foldM (\xs (p, it)-> do mfid <- getFileID p
                                             return $ case mfid of
                                               Nothing -> xs
                                               Just fid -> ((p, it), fid):xs) []
        new <- sortBy (comparing snd) <$>
                 (addIDs . map (\(a,b) -> (a, itemType b)) . Tree.list  =<<
                   expand =<< applyTreeFilter nonboring <$> readPlainTree repository)
        let match (x:xs) (y:ys)
              | snd x > snd y = match (x:xs) ys
              | snd x < snd y = match xs (y:ys)
              | snd (fst x) /= snd (fst y) = match xs ys
              | otherwise = (fst (fst x), fst (fst y), snd (fst x)):match xs ys
            match _ _ = []
            movedfiles = match old new
            fmovedfiles =
              case fs of
                Nothing -> movedfiles
                Just paths ->
                  filter (\(f1, f2, _) -> any (`elem` selfiles) [f1, f2]) movedfiles
                  where selfiles = paths
        return (resolve fmovedfiles)

    resolve :: [(AnchoredPath, AnchoredPath, ItemType)]
            -> [(AnchoredPath, AnchoredPath, ItemType)]
    resolve xs = fixPaths $ sortMoves $ deleteCycles xs
      where
        -- Input relation is left-and-right-unique. Makes cycle detection easier.
        deleteCycles [] = []
        deleteCycles whole@( x@(start,_,_):rest)
            = if hasCycle start whole start
                  then deleteCycles (deleteFrom start whole [])
                  else x:deleteCycles rest
           where hasCycle current ((a',b',_):rest') first
                     | a' == current = b' == first || hasCycle b' whole first
                     | otherwise     = hasCycle current rest' first 
                 hasCycle _ [] _     = False
                 deleteFrom a (y@(a',b',_):ys) seen
                   | a == a'   = deleteFrom b' (seen++ys) []
                   | otherwise = deleteFrom a ys (y:seen)
                 deleteFrom _ [] seen = seen

        sortMoves []                           = []
        sortMoves whole@(current@(_,dest,_):_) =
              smallest:sortMoves (delete smallest whole)
              where
               smallest = follow dest whole current
               follow prevDest (y@(s,d,_):ys) currentSmallest
                 -- destination is source of another move
                 | prevDest == s             = follow d whole y
                 -- parent of destination is also destination of a move
                 | d `elem` parents prevDest = follow d whole y
                 | otherwise     = follow prevDest ys currentSmallest
               follow _ [] currentSmallest = currentSmallest

        -- rewrite [d/ -> e/, .., d/f -> e/h] to [d/ -> e/, .., e/f -> e/h]
        -- and throw out moves that don't move anything (can they be in there?)
        fixPaths [] = []
        fixPaths (y@(f1,f2,t):ys)
                        | f1 == f2         = fixPaths ys -- no effect, throw out
                        | TreeType <- t    = y:fixPaths (map replacepp ys)
                        | otherwise        = y:fixPaths ys
         -- TODO why adapt only if1 here and not if2?
         --      is this a bug?
         where replacepp (if1,if2,it) = (movedirfilename f1 f2 if1, if2, it)

-- | Search for possible replaces between the recordedAndPending state
-- and the unrecorded (or working) state. Return a Sealed FL list of
-- replace patches to be applied to the recordedAndPending state.
getReplaces :: forall rt p wR wU wT
             . (RepoPatch p, ApplyState p ~ Tree)
            => LookForReplaces
            -> DiffAlgorithm
            -> Repository rt p wR wU wT
            -> Tree IO -- ^ pending tree (including possibly detected moves)
            -> Tree IO -- ^ working tree
            -> IO (Tree IO, -- new pending tree
                   Sealed (FL (PrimOf p) wU))
getReplaces NoLookForReplaces _ _ pending _ = return (pending, Sealed NilFL)
getReplaces YesLookForReplaces diffalg _repo pending working = do
    ftf <- filetypeFunction
    Sealed changes <- unFreeLeft <$> treeDiff diffalg ftf pending working
    let allModifiedTokens = concat $ mapFL modifiedTokens changes
        replaces = rmInvalidReplaces allModifiedTokens
    (patches, new_pending) <-
      flip runStateT pending $
        forM replaces $ \(path, a, b) ->
          doReplace defaultToks path (BC.unpack a) (BC.unpack b)
    return (new_pending, mapSeal concatFL $ toFL patches)
  where
    modifiedTokens :: PrimOf p wX wY -> [(AnchoredPath, B.ByteString, B.ByteString)]
    modifiedTokens p = case isHunk p of
      Just (FileHunk f _ old new) ->
        map (\(a,b) -> (f, a, b)) (concatMap checkModified $
          filter (\(a,b) -> length a == length b) -- only keep lines with same number of tokens
            $ zip (map breakToTokens old) (map breakToTokens new))
      Nothing -> []

    -- from a pair of token lists, create a pair of modified token lists
    checkModified = filter (\(a,b) -> a/=b) . uncurry zip

    rmInvalidReplaces [] = []
    rmInvalidReplaces ((f,old,new):rs)
      | any (\(f',a,b) -> f' == f && old == a && b /= new) rs =
          -- inconsistency detected
          rmInvalidReplaces $ filter (\(f'',a',_) -> f'' /= f || a' /= old) rs
    rmInvalidReplaces (r:rs) = r:rmInvalidReplaces (filter (/=r) rs)

    doReplace toks path old new = do
        pend <- get
        mpend' <- liftIO $ maybeApplyToTree replacePatch pend
        case mpend' of
          Nothing -> getForceReplace path toks old new
          Just pend' -> do
            put pend'
            return $ joinGap (:>:) (freeGap replacePatch) (emptyGap NilFL)
      where
        replacePatch = tokreplace path toks old new

    getForceReplace :: (PrimPatch prim, ApplyState prim ~ Tree)
                    => AnchoredPath -> String -> String -> String
                    -> StateT (Tree IO) IO (FreeLeft (FL prim))
    getForceReplace path toks old new = do
        -- the tree here is the "current" pending state
        tree <- get
        -- It would be nice if we could fuse the two traversals here, that is,
        -- expandPath and findFile. OTOH it is debatable whether adding a new
        -- effectful version of findFile to Darcs.Util.Tree is justified.
        expandedTree <- liftIO $ expandPath tree path
        content <- case findFile expandedTree path of
          Just blob -> liftIO $ readBlob blob
          Nothing -> error $ "getForceReplace: not in tree: " ++ show path
        let newcontent = forceTokReplace toks (BC.pack new) (BC.pack old)
                            (B.concat $ BL.toChunks content)
            tree' = modifyTree expandedTree path . Just . File $ makeBlobBS newcontent
        ftf <- liftIO $ filetypeFunction
        normaliseNewTokPatch <- liftIO $ treeDiff diffalg ftf expandedTree tree'
        -- make sure we can apply them to the pending state
        patches <- return $ joinGap (+>+) normaliseNewTokPatch $ freeGap $
            tokreplace path toks old new :>: NilFL
        mtree'' <- case unFreeLeft patches of
            Sealed ps -> liftIO $ maybeApplyToTree ps tree
        case mtree'' of
            Nothing -> error "getForceReplace: unable to apply detected force replaces"
            Just tree'' -> do
                put tree''
                return patches


-- | Add an 'FL' of patches started from the pending state to the pending patch.
-- TODO: add witnesses for pending so we can make the types precise: currently
-- the passed patch can be applied in any context, not just after pending.
addPendingDiffToPending :: (RepoPatch p, ApplyState p ~ Tree)
                        => Repository rt p wR wU wR
                        -> FreeLeft (FL (PrimOf p)) -> IO ()
addPendingDiffToPending repo newP = do
    (_, Sealed toPend) <- readPending repo
    invalidateIndex repo
    case unFreeLeft newP of
        (Sealed p) -> do
            recordedState <- readRecorded repo
            Pending.makeNewPending repo YesUpdatePending (toPend +>+ p) recordedState

-- | Add an 'FL' of patches starting from the working state to the pending patch,
-- including as much extra context as is necessary (context meaning
-- dependencies), by commuting the patches to be added past as much of the
-- changes between pending and working as is possible, and including anything
-- that doesn't commute, and the patch itself in the new pending patch.
addToPending :: (RepoPatch p, ApplyState p ~ Tree)
             => Repository rt p wR wU wR
             -> UseIndex -> FL (PrimOf p) wU wY -> IO ()
addToPending repo useidx p = do
   (toPend :> toUnrec) <- readPendingAndWorking (useidx, ScanKnown, MyersDiff)
      NoLookForMoves NoLookForReplaces repo Nothing
   invalidateIndex repo
   case genCommuteWhatWeCanRL commuteFL (reverseFL toUnrec :> p) of
       (toP' :> p'  :> _excessUnrec) -> do
           recordedState <- readRecorded repo
           Pending.makeNewPending repo YesUpdatePending
            (toPend +>+ reverseRL toP' +>+ p') recordedState

readPlainTree :: Repository rt p wR wU wT -> IO (Tree IO)
readPlainTree repo  = PlainTree.readPlainTree (repoLocation repo)
