module Darcs.Repository.Repair ( replayRepository, checkIndex,
                                 replayRepositoryInTemp,
                                 RepositoryConsistency(..) )
       where

import Darcs.Prelude

import Control.Monad ( when, unless )
import Control.Monad.Trans ( liftIO )
import Control.Exception ( catch, finally, IOException )
import Data.Maybe ( catMaybes )
import Data.List ( sort, (\\) )
import System.Directory
    ( createDirectoryIfMissing
    , getCurrentDirectory
    , removeDirectoryRecursive
    , setCurrentDirectory
    )
import System.FilePath ( (</>) )
import Darcs.Util.Path( anchorPath, AbsolutePath, ioAbsolute, toFilePath )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , WPatchInfo
    , compareWPatchInfo
    , hopefully
    , info
    , unWPatchInfo
    , winfo
    )

import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), lengthFL, reverseFL,
    mapRL, nullFL, (:||:)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), Sealed(..), unFreeLeft )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Repair ( Repair(applyAndTryToFix) )
import Darcs.Patch.Info ( displayPatchInfo )
import Darcs.Patch.Set ( Origin, PatchSet(..), patchSet2FL, patchSet2RL )
import Darcs.Patch ( RepoPatch, IsRepoType, PrimOf, isInconsistent )

import Darcs.Repository.Cache ( HashedDir( HashedPristineDir ) )
import Darcs.Repository.Diff( treeDiff )
import Darcs.Repository.Flags ( Verbosity(..), Compression, DiffAlgorithm )
import Darcs.Repository.Format
    ( identifyRepoFormat
    , RepoProperty ( HashedInventory )
    , formatHas
    )
import Darcs.Repository.HashedIO ( cleanHashdir )
import Darcs.Repository.Hashed ( readRepo, writeAndReadPatch )
import Darcs.Repository.InternalTypes ( Repository, repoCache, repoLocation )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.Pristine ( readHashedPristineRoot )
import Darcs.Repository.State
    ( readRecorded
    , readIndex
    , readRecordedAndPending
    )

import Darcs.Util.Progress
    ( beginTedious
    , debugMessage
    , endTedious
    , finishedOneIO
    , tediousSize
    )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Lock( withDelayedDir )
import Darcs.Util.Printer ( Doc, putDocLn, text, renderString )

import Darcs.Util.Hash( Hash(NoHash), encodeBase16 )
import Darcs.Util.Tree( Tree, emptyTree, list, restrict, expand, itemHash, zipTrees )
import Darcs.Util.Tree.Monad( TreeIO )
import Darcs.Util.Tree.Hashed( darcsUpdateHashes, hashedTreeIO )
import Darcs.Util.Tree.Plain( readPlainTree )
import Darcs.Util.Index( updateIndex )

import qualified Data.ByteString.Char8 as BC

replaceInFL :: FL (PatchInfoAnd rt a) wX wY
            -> [Sealed2 (WPatchInfo :||: PatchInfoAnd rt a)]
            -> FL (PatchInfoAnd rt a) wX wY
replaceInFL orig [] = orig
replaceInFL NilFL _ = error "impossible case"
replaceInFL (o:>:orig) ch@(Sealed2 (o':||:c):ch_rest)
    | IsEq <- winfo o `compareWPatchInfo` o' = c:>:replaceInFL orig ch_rest
    | otherwise = o:>:replaceInFL orig ch

applyAndFix
  :: forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wT
  -> Compression
  -> FL (PatchInfoAnd rt p) Origin wR
  -> TreeIO (FL (PatchInfoAnd rt p) Origin wR, Bool)
applyAndFix _ _ NilFL = return (NilFL, True)
applyAndFix r compr psin =
    do liftIO $ beginTedious k
       liftIO $ tediousSize k $ lengthFL psin
       (repaired, ok) <- aaf psin
       liftIO $ endTedious k
       orig <- liftIO $ patchSet2FL `fmap` readRepo r
       return (replaceInFL orig repaired, ok)
    where k = "Replaying patch"
          aaf :: FL (PatchInfoAnd rt p) wW wZ
              -> TreeIO ([Sealed2 (WPatchInfo :||: PatchInfoAnd rt p)], Bool)
          aaf NilFL = return ([], True)
          aaf (p:>:ps) = do
            mp' <- applyAndTryToFix p
            case isInconsistent . hopefully $ p of
              Just err -> liftIO $ putDocLn err
              Nothing -> return ()
            let !winfp = winfo p -- assure that 'p' can be garbage collected.
            liftIO $ finishedOneIO k $ renderString $
              displayPatchInfo $ unWPatchInfo winfp
            (ps', restok) <- aaf ps
            case mp' of
              Nothing -> return (ps', restok)
              Just (e,pp) -> liftIO $ do
                putStrLn e
                p' <- withCurrentDirectory (repoLocation r) $
                  writeAndReadPatch (repoCache r) compr pp
                return (Sealed2 (winfp :||: p'):ps', False)

data RepositoryConsistency rt p wX =
    RepositoryConsistent
  | BrokenPristine (Tree IO)
  | BrokenPatches (Tree IO) (PatchSet rt p Origin wX)

checkUniqueness :: (IsRepoType rt, RepoPatch p)
                => (Doc -> IO ()) -> (Doc -> IO ()) -> Repository rt p wR wU wT -> IO ()
checkUniqueness putVerbose putInfo repository =
    do putVerbose $ text "Checking that patch names are unique..."
       r <- readRepo repository
       case hasDuplicate $ mapRL info $ patchSet2RL r of
         Nothing -> return ()
         Just pinf -> do putInfo $ text "Error! Duplicate patch name:"
                         putInfo $ displayPatchInfo pinf
                         fail "Duplicate patches found."

hasDuplicate :: Ord a => [a] -> Maybe a
hasDuplicate li = hd $ sort li
    where hd [_] = Nothing
          hd [] = Nothing
          hd (x1:x2:xs) | x1 == x2 = Just x1
                        | otherwise = hd (x2:xs)

replayRepository'
  :: forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => DiffAlgorithm
  -> AbsolutePath
  -> Repository rt p wR wU wT
  -> Compression
  -> Verbosity
  -> IO (RepositoryConsistency rt p wR)
replayRepository' dflag whereToReplay' repo compr verbosity = do
  let whereToReplay = toFilePath whereToReplay'
      putVerbose s = when (verbosity == Verbose) $ putDocLn s
      putInfo s = unless (verbosity == Quiet) $ putDocLn s
  checkUniqueness putVerbose putInfo repo
  createDirectoryIfMissing False whereToReplay
  putVerbose $ text "Reading recorded state..."
  pris <-
    (readRecorded repo >>= expand >>= darcsUpdateHashes)
    `catch`
    \(_ :: IOException) -> return emptyTree
  putVerbose $ text "Applying patches..."
  patches <- readRepo repo
  debugMessage "Fixing any broken patches..."
  let psin = patchSet2FL patches
      repair = applyAndFix repo compr psin

  ((ps, patches_ok), newpris) <- hashedTreeIO repair emptyTree whereToReplay
  debugMessage "Done fixing broken patches..."
  let newpatches = PatchSet NilRL (reverseFL ps)

  debugMessage "Checking pristine against slurpy"
  ftf <- filetypeFunction
  is_same <- do Sealed diff <- unFreeLeft `fmap` treeDiff dflag ftf pris newpris
                  :: IO (Sealed (FL (PrimOf p) wR))
                return $ nullFL diff
              `catchall` return False
  -- TODO is the latter condition needed? Does a broken patch imply pristine
  -- difference? Why, or why not?
  return (if is_same && patches_ok
     then RepositoryConsistent
     else if patches_ok
            then BrokenPristine newpris
            else BrokenPatches newpris newpatches)

cleanupRepositoryReplay :: Repository rt p wR wU wT -> IO ()
cleanupRepositoryReplay r = do
  let c = repoCache r
  rf <- identifyRepoFormat "."
  unless (formatHas HashedInventory rf) $
         removeDirectoryRecursive $ darcsdir ++ "/pristine.hashed"
  when (formatHas HashedInventory rf) $ do
       current <- readHashedPristineRoot r
       cleanHashdir c HashedPristineDir $ catMaybes [current]

replayRepositoryInTemp
  :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => DiffAlgorithm
  -> Repository rt p wR wU wT
  -> Compression
  -> Verbosity
  -> IO (RepositoryConsistency rt p wR)
replayRepositoryInTemp dflag r compr verb = do
  repodir <- getCurrentDirectory
  {- The reason we use withDelayedDir here, instead of withTempDir, is that
  replayRepository' may return a new pristine that is read from the 
  temporary location and reading a Tree is done using lazy ByteStrings (for
  file contents). Then we check if there is a difference to our stored
  pristine, but when there are differences the check may terminate early
  and not all of the new pristine was read/evaluated. This may then cause
  does-not-exist-failures later on when the tree is evaluated further.
  -}
  withDelayedDir "darcs-check" $ \tmpDir -> do
    setCurrentDirectory repodir
    replayRepository' dflag tmpDir r compr verb

replayRepository
  :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => DiffAlgorithm
  -> Repository rt p wR wU wT
  -> Compression
  -> Verbosity
  -> (RepositoryConsistency rt p wR -> IO a)
  -> IO a
replayRepository dflag r compr verb f =
  run `finally` cleanupRepositoryReplay r
    where run = do
            createDirectoryIfMissing False $ darcsdir </> "pristine.hashed"
            hashedPristine <- ioAbsolute $ darcsdir </> "pristine.hashed"
            st <- replayRepository' dflag hashedPristine r compr verb
            f st

checkIndex
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wR
  -> Bool
  -> IO Bool
checkIndex repo quiet = do
  index <- updateIndex =<< readIndex repo
  pristine <- expand =<< readRecordedAndPending repo
  working <- expand =<< restrict pristine <$> readPlainTree "."
  working_hashed <- darcsUpdateHashes working
  let index_paths = [ p | (p, _) <- list index ]
      working_paths = [ p | (p, _) <- list working ]
      index_extra = index_paths \\ working_paths
      working_extra = working_paths \\ index_paths
      gethashes p (Just i1) (Just i2) = (p, itemHash i1, itemHash i2)
      gethashes p (Just i1) Nothing   = (p, itemHash i1, NoHash)
      gethashes p   Nothing (Just i2) = (p,      NoHash, itemHash i2)
      gethashes p   Nothing Nothing   = error $ "Bad case at " ++ show p
      mismatches =
        [miss | miss@(_, h1, h2) <- zipTrees gethashes index working_hashed, h1 /= h2]

      format paths = unlines $ map (("  " ++) . anchorPath "") paths
      mismatches_disp = unlines [ anchorPath "" p ++
                                    "\n    index: " ++ BC.unpack (encodeBase16 h1) ++
                                    "\n  working: " ++ BC.unpack (encodeBase16 h2)
                                  | (p, h1, h2) <- mismatches ]
  unless (quiet || null index_extra) $
         putStrLn $ "Extra items in index!\n" ++ format index_extra
  unless (quiet || null working_extra) $
         putStrLn $ "Missing items in index!\n" ++ format working_extra
  unless (quiet || null mismatches) $
         putStrLn $ "Hash mismatch(es)!\n" ++ mismatches_disp
  return $ null index_extra && null working_extra && null mismatches

