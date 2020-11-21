{-# LANGUAGE NamedFieldPuns #-}

{-|
License : GPL-2

The patch-index stores additional information that is extracted from
the PatchSet for the repository to speed up certain commands (namely
@log@ and @annotate@). More precisely, for every file tracked by the
repository, it stores the list of patches that touch it.

When created, patch-index lives in @_darcs\/patch_index\/@, and it
should be automatically maintained each time the set of patches of
the repository is updated.

Patch-index can also be explicitely disabled by creating a file
@_darcs\/no_patch_index@. "Explicitely disabed" means that no command
should attempt to automatically create the patch-index.

See <http://darcs.net/Internals/PatchIndex> for more information.
-}
module Darcs.Repository.PatchIndex
    ( doesPatchIndexExist
    , isPatchIndexDisabled
    , isPatchIndexInSync
    , canUsePatchIndex
    , createPIWithInterrupt
    , createOrUpdatePatchIndexDisk
    , deletePatchIndex
    , attemptCreatePatchIndex
    , PatchFilter
    , maybeFilterPatches
    , getRelevantSubsequence
    , dumpPatchIndex
    , piTest
    ) where

import Darcs.Prelude

import Control.Exception ( catch )
import Control.Monad ( forM_, unless, when )
import Control.Monad.State.Strict ( evalState, execState, State, gets, modify )

import Data.Binary ( Binary, encodeFile, decodeFileOrFail )
import qualified Data.ByteString as B
import Data.Int ( Int8 )
import Data.List ( group, mapAccumL, sort, nub, (\\) )
import Data.Maybe ( fromJust, fromMaybe, isJust )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word ( Word32 )

import System.Directory
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , removeDirectoryRecursive
    , removeFile
    , renameDirectory
    )
import System.FilePath( (</>) )
import System.IO ( openFile, IOMode(WriteMode), hClose )

import Darcs.Patch ( RepoPatch, listTouchedFiles )
import Darcs.Patch.Apply ( ApplyState(..) )
import Darcs.Patch.Index.Types
import Darcs.Patch.Index.Monad ( applyToFileMods, makePatchID )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Progress (progressFL )
import Darcs.Patch.Set ( PatchSet, patchSet2FL, Origin, patchSet2FL )
import Darcs.Patch.Witnesses.Ordered ( mapFL, RL(..), FL(..), reverseRL )
import Darcs.Patch.Witnesses.Sealed
    ( Sealed2(..)
    , Sealed(..)
    , seal
    , seal2
    , unseal
    , unseal2
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )

import Darcs.Repository.Format ( formatHas, RepoProperty( HashedInventory ) )
import Darcs.Repository.InternalTypes ( Repository, repoLocation, repoFormat )

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Hash ( sha256sum, showAsHex )
import Darcs.Util.Lock ( withPermDir )
import Darcs.Util.Path ( AnchoredPath, displayPath, toFilePath, isPrefix )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.SignalHandler ( catchInterrupt )
import Darcs.Util.Tree ( Tree(..) )

type Map = M.Map
type Set = S.Set

data FileIdSpan = FidSpan
  !FileId                   -- ^ the fileid has some fixed name in the
  !PatchId                  -- ^ span starting here
  !(Maybe PatchId)          -- ^ and (maybe) ending here
  deriving (Show, Eq, Ord)

data FilePathSpan = FpSpan
  !AnchoredPath             -- ^ the file path has some fixed fileid in the
  !PatchId                  -- ^ span starting here
  !(Maybe PatchId)          -- ^ and (maybe) ending here
  deriving (Show, Eq, Ord)

-- | info about a given fileid, e.g.. is a file or a directory
data FileInfo = FileInfo
  { isFile :: Bool
  , touching :: Set Word32  -- ^ first word of patch hash
  } deriving (Show, Eq, Ord)

-- | timespans where a certain filename corresponds to a file with a given id
type FileIdSpans = Map AnchoredPath [FileIdSpan]

-- | timespans where a file with a certain id corresponds to given filenames
type FilePathSpans = Map FileId [FilePathSpan]

-- | information file with a given ID
type InfoMap = Map FileId FileInfo

-- | the patch-index
data PatchIndex = PatchIndex
  { pids :: [PatchId]
    -- ^ all the 'PatchId's tracked by this patch index, with the most
    -- recent patch at the head of the list (note, stored in the
    -- reverse order to this on disk for backwards compatibility
    -- with an older format).
  , fidspans :: FileIdSpans
  , fpspans :: FilePathSpans
  , infom :: InfoMap
  }

-- | On-disk version of patch index
--   version 1 is the one introduced in darcs 2.10
--           2 changes the pids order to newer-to-older
--           3 changes FileName to AnchoredPath everywhere, which has
--             different Binary (and Ord) instances
version :: Int8
version = 3

type PIM a = State PatchIndex a

-- | 'applyPatchMods pmods pindex' applies a list of PatchMods to the given
--   patch index pindex
applyPatchMods :: [(PatchId, [PatchMod AnchoredPath])] -> PatchIndex -> PatchIndex
applyPatchMods pmods pindex =
  flip execState pindex $ mapM_ goList pmods
 where goList :: (PatchId, [PatchMod AnchoredPath]) -> PIM ()
       goList (pid, mods) = do
           modify (\pind -> pind{pids = pid:pids pind})
           mapM_ (curry go pid) (nubSeq mods)
       -- nubSeq handles invalid patch in darcs repo:
       --   move with identical target name "rename darcs_patcher to darcs-patcher."
       nubSeq = map head . group
       go :: (PatchId, PatchMod AnchoredPath) -> PIM ()
       go (pid, PCreateFile fn) = do
         fid <- createFidStartSpan fn pid
         startFpSpan fid fn pid
         createInfo fid True
         insertTouch fid pid
       go (pid, PCreateDir fn) = do
         fid <- createFidStartSpan fn pid
         startFpSpan fid fn pid
         createInfo fid False
         insertTouch fid pid
       go (pid, PTouch fn) = do
         fid <- lookupFid fn
         insertTouch fid pid
       go (pid, PRename oldfn newfn) = do
         fid <- lookupFid oldfn
         stopFpSpan fid pid
         startFpSpan fid newfn pid
         insertTouch fid pid
         stopFidSpan oldfn pid
         startFidSpan newfn pid fid
       go (pid, PRemove fn) = do
         fid <- lookupFid fn
         insertTouch fid pid
         stopFidSpan fn pid
         stopFpSpan fid pid
       go (_, PInvalid _) = return () -- just ignore invalid changes
       go (pid, PDuplicateTouch fn) = do
         fidm <- gets fidspans
         case M.lookup fn fidm of
           Just (FidSpan fid _ _:_) -> insertTouch fid pid
           Nothing -> return ()
           Just [] -> error $ "applyPatchMods: impossible, no entry for "++show fn
                              ++" in FileIdSpans in duplicate, empty list"

-- | create new filespan for created file
createFidStartSpan :: AnchoredPath -> PatchId -> PIM FileId
createFidStartSpan fn pstart = do
  fidspans <- gets fidspans
  case M.lookup fn fidspans of
    Nothing -> do
      let fid = FileId fn 1
      modify (\pind -> pind {fidspans=M.insert fn [FidSpan fid pstart Nothing] fidspans})
      return fid
    Just fspans -> do
      let fid = FileId fn (length fspans+1)
      modify (\pind -> pind {fidspans=M.insert fn (FidSpan fid pstart Nothing:fspans) fidspans})
      return fid

-- | start new span for name fn for file fid starting with patch pid
startFpSpan :: FileId -> AnchoredPath -> PatchId -> PIM ()
startFpSpan fid fn pstart = modify (\pind -> pind {fpspans=M.alter alt fid (fpspans pind)})
  where alt Nothing = Just [FpSpan fn pstart Nothing]
        alt (Just spans) = Just (FpSpan fn pstart Nothing:spans)

-- | stop current span for file name fn
stopFpSpan :: FileId -> PatchId -> PIM ()
stopFpSpan fid pend = modify (\pind -> pind {fpspans=M.alter alt fid (fpspans pind)})
  where alt Nothing = error $ "impossible: no span for " ++ show fid
        alt (Just []) = error $ "impossible: no span for " ++ show fid++", empty list"
        alt (Just (FpSpan fp pstart Nothing:spans)) =
          Just (FpSpan fp pstart (Just pend):spans)
        alt _ = error $ "impossible: span already ended for " ++ show fid

-- | start new span for name fn for file fid starting with patch pid
startFidSpan :: AnchoredPath -> PatchId -> FileId -> PIM ()
startFidSpan fn pstart fid = modify (\pind -> pind {fidspans=M.alter alt fn (fidspans pind)})
  where alt Nothing = Just [FidSpan fid pstart Nothing]
        alt (Just spans) = Just (FidSpan fid pstart Nothing:spans)

-- | stop current span for file name fn
stopFidSpan :: AnchoredPath -> PatchId -> PIM ()
stopFidSpan fn pend = modify (\pind -> pind {fidspans=M.alter alt fn (fidspans pind)})
  where alt Nothing = error $ "impossible: no span for " ++ show fn
        alt (Just []) = error $ "impossible: no span for " ++ show fn++", empty list"
        alt (Just (FidSpan fid pstart Nothing:spans)) =
          Just (FidSpan fid pstart (Just pend):spans)
        alt _ = error $ "impossible: span already ended for " ++ show fn

-- | insert touching patchid for given file id
createInfo :: FileId -> Bool -> PIM ()
createInfo fid isF = modify (\pind -> pind {infom=M.alter alt fid (infom pind)})
  where alt Nothing = Just (FileInfo isF S.empty)
        alt (Just _) = Just (FileInfo isF S.empty) -- forget old false positives

-- | insert touching patchid for given file id
insertTouch :: FileId -> PatchId -> PIM ()
insertTouch fid pid = modify (\pind -> pind {infom=M.alter alt fid (infom pind)})
  where alt Nothing =  error "impossible: Fileid does not exist"
        alt (Just (FileInfo isF pids)) = Just (FileInfo isF (S.insert (short pid) pids))

-- | lookup current fid of filepath
lookupFid :: AnchoredPath -> PIM FileId
lookupFid fn = do
    maybeFid <- lookupFid' fn
    case maybeFid of
        Nothing -> error $ "couldn't find " ++ displayPath fn ++ " in patch index"
        Just fid -> return fid

-- | lookup current fid of filepatch, returning a Maybe to allow failure
lookupFid' :: AnchoredPath -> PIM (Maybe FileId)
lookupFid' fn = do
   fidm <- gets fidspans
   case M.lookup fn fidm of
    Just (FidSpan fid _ _:_) -> return $ Just fid
    _ -> return Nothing


-- | lookup all the file ids of a given path
lookupFidf' :: AnchoredPath -> PIM [FileId]
lookupFidf' fn = do
   fidm <- gets fidspans
   case M.lookup fn fidm of
      Just spans -> return $ map (\(FidSpan fid _ _) -> fid) spans
      Nothing ->
         error $ "lookupFidf': no entry for " ++ show fn ++ " in FileIdSpans"

-- |  return all fids of matching subpaths
--    of the given filepath
lookupFids :: AnchoredPath -> PIM [FileId]
lookupFids fn = do
   fid_spans <- gets fidspans
   file_idss <- mapM lookupFidf' $
      filter (isPrefix fn) (fpSpans2filePaths' fid_spans)
   return $ nub $ concat file_idss

-- | returns a single file id if the given path is a file
--   if it is a directory, if returns all the file ids of all paths inside it,
--   at any point in repository history
lookupFids' :: AnchoredPath -> PIM [FileId]
lookupFids' fn = do
  info_map <- gets infom
  fps_spans <- gets fpspans
  a <- lookupFid' fn
  if isJust a then do
                let fid = fromJust a
                case M.lookup fid info_map of
                  Just (FileInfo True _) -> return [fid]
                  Just (FileInfo False _) ->
                    let file_names = map (\(FpSpan x _ _) -> x) (fps_spans M.! fid)
                    in nub . concat <$> mapM lookupFids file_names
                  Nothing -> error "lookupFids' : could not find file"
              else return []

-- | Creates patch index that corresponds to all patches in repo.
createPatchIndexDisk
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wT
  -> PatchSet rt p Origin wR
  -> IO ()
createPatchIndexDisk repository ps = do
  let patches = mapFL Sealed2 $ progressFL "Create patch index" $ patchSet2FL ps
  createPatchIndexFrom repository $ patches2patchMods patches S.empty

-- | convert patches to patchmods
patches2patchMods :: (Apply p, PatchInspect p, ApplyState p ~ Tree)
                  => [Sealed2 (PatchInfoAnd rt p)] -> Set AnchoredPath -> [(PatchId, [PatchMod AnchoredPath])]
patches2patchMods patches fns = snd $ mapAccumL go fns patches
  where
    go filenames (Sealed2 p) = (filenames', (pid, pmods_effect ++ pmods_dup))
      where pid = makePatchID . info $ p
            (filenames', pmods_effect) = applyToFileMods p filenames
            -- applyToFileMods only returns patchmods that actually modify a file,
            -- i.e., never duplicate patches
            touched pm = case pm of {PTouch f -> [f]; PRename a b -> [a,b];
                                     PCreateDir f -> [f]; PCreateFile f -> [f];
                                     PRemove f -> [f]; _ -> []}
            touched_all = listTouchedFiles p
            touched_effect = concatMap touched pmods_effect
            touched_invalid = [ f | (PInvalid f) <- pmods_effect]
            -- listTouchedFiles returns all files that touched by these
            --  patches, even if they have no effect, e.g. by duplicate patches
            pmods_dup = map PDuplicateTouch . S.elems
                            $ S.difference (S.fromList touched_all)
                                           (S.fromList touched_invalid
                                            `S.union`
                                            S.fromList touched_effect)

-- | return set of current filenames in patch index
fpSpans2fileNames :: FilePathSpans -> Set AnchoredPath
fpSpans2fileNames fpSpans =
  S.fromList [fn | (FpSpan fn _ Nothing:_)<- M.elems fpSpans]

-- | remove all patch effects of given patches from patch index.
--   assumes that the given list of patches is a suffix of the
--   patches tracked by the patch-index
removePidSuffix :: Map PatchId Int -> [PatchId] -> PatchIndex -> PatchIndex
removePidSuffix _ [] pindex = pindex
removePidSuffix pid2idx oldpids@(oldpid:_) (PatchIndex pids fidspans fpspans infom) =
    PatchIndex (pids \\ oldpids)
               (M.mapMaybe removefid fidspans)
               (M.mapMaybe removefp fpspans)
               infom -- leave hashes in infom, false positives are harmless
  where
    findIdx pid = fromMaybe (error "impossible case") (M.lookup pid pid2idx)
    oldidx = findIdx oldpid
    from `after` idx = findIdx from > idx
    mto `afterM` idx | Just to <- mto, findIdx to > idx = True
                     | otherwise = False
    removefid fidsps = if null fidsps' then Nothing else Just fidsps'
      where
        fidsps' = concatMap go fidsps
        go (FidSpan fid from mto)
          | from `after` oldidx && mto `afterM` oldidx = [FidSpan fid from mto]
          | from `after` oldidx = [FidSpan fid from Nothing]
          | otherwise = []
    removefp fpsps = if null fpsps' then Nothing else Just fpsps'
      where
        fpsps' = concatMap go fpsps
        go (FpSpan fn from mto)
          | from `after` oldidx && mto `afterM` oldidx = [FpSpan fn from mto]
          | from `after` oldidx = [FpSpan fn from Nothing]
          | otherwise = []

-- | update the patch index to the current state of the repository
updatePatchIndexDisk
    :: (RepoPatch p, ApplyState p ~ Tree)
    => Repository rt p wR wU wT
    -> PatchSet rt p Origin wR
    -> IO ()
updatePatchIndexDisk repo patches = do
    let repodir = repoLocation repo
    (_,_,pid2idx,pindex) <- loadPatchIndex repodir
    -- check that patch index is up to date
    let flpatches = progressFL "Update patch index" $ patchSet2FL patches
    let pidsrepo = mapFL (makePatchID . info) flpatches
        (oldpids,_,len_common) = uncommon (reverse $ pids pindex) pidsrepo
        pindex' = removePidSuffix pid2idx oldpids pindex
        filenames = fpSpans2fileNames (fpspans pindex')
        cdir = repodir </> indexDir
    -- reread to prevent holding onto patches for too long
    let newpatches = drop len_common $ mapFL seal2 flpatches
        newpmods = patches2patchMods newpatches filenames
    inv_hash <- getInventoryHash repodir
    storePatchIndex repodir cdir inv_hash (applyPatchMods newpmods pindex')
  where
    -- return uncommon suffixes and length of common prefix of as and bs
    uncommon = uncommon' 0
    uncommon' x (a:as) (b:bs)
      | a == b     = uncommon' (x+1) as bs
      | otherwise  =  (a:as,b:bs,x)
    uncommon' x as bs = (as,bs,x)

-- | 'createPatchIndexFrom repo pmods' creates a patch index from the given
--   patchmods.
createPatchIndexFrom :: Repository rt p wR wU wT
                     -> [(PatchId, [PatchMod AnchoredPath])] -> IO ()
createPatchIndexFrom repo pmods = do
    inv_hash <- getInventoryHash repodir
    storePatchIndex repodir cdir inv_hash (applyPatchMods pmods emptyPatchIndex)
  where repodir = repoLocation repo
        cdir = repodir </> indexDir
        emptyPatchIndex = PatchIndex [] M.empty M.empty M.empty

getInventoryHash :: FilePath -> IO String
getInventoryHash repodir = do
  inv <- B.readFile (repodir </> darcsdir </> "hashed_inventory")
  return $ sha256sum inv

-- | Load patch-index from disk along with some meta data.
loadPatchIndex :: FilePath -> IO (Int8, String, Map PatchId Int, PatchIndex)
loadPatchIndex repodir = do
  let pindex_dir = repodir </> indexDir
  (v,inv_hash) <- loadRepoState (pindex_dir </> repoStateFile)
  pids <- loadPatchIds (pindex_dir </> pidsFile)
  let pid2idx  = M.fromList $ zip pids [(1::Int)..]
  infom <- loadInfoMap (pindex_dir </> touchMapFile)
  fidspans <- loadFidMap (pindex_dir </> fidMapFile)
  fpspans <- loadFpMap (pindex_dir </> fpMapFile)
  return (v, inv_hash, pid2idx, PatchIndex pids fidspans fpspans infom)

-- | If patch-index is useful as it is now, read it. If not, create or update it, then read it.
loadSafePatchIndex :: (RepoPatch p, ApplyState p ~ Tree)
                   => Repository rt p wR wU wT
                   -> PatchSet rt p Origin wR     -- ^ PatchSet of the repository, used if we need to create the patch-index.
                   -> IO PatchIndex
loadSafePatchIndex repo ps = do
   let repodir = repoLocation repo
   can_use <- isPatchIndexInSync repo
   (_,_,_,pi) <-
     if can_use
       then loadPatchIndex repodir
       else do createOrUpdatePatchIndexDisk repo ps
               loadPatchIndex repodir
   return pi

-- | Read-only. Checks if patch-index exists for this repository
--   it works by checking if:
--
--     1. @_darcs\/patch_index\/@ and its corresponding files are all present
--     2. patch index version is the one handled by this version of Darcs
doesPatchIndexExist :: FilePath -> IO Bool
doesPatchIndexExist repodir = do
 filesArePresent <- and <$> mapM (doesFileExist . (pindex_dir </>))
                    [repoStateFile, pidsFile, touchMapFile, fidMapFile, fpMapFile]
 if filesArePresent
  then do v <- piVersion
          return (v == version)   -- consider PI only of on-disk format is the current one
  else return False
   where pindex_dir = repodir </> indexDir
         piVersion = fst <$> loadRepoState (pindex_dir </> repoStateFile)

-- | Read-only. Checks if @_darcs\/noPatchIndex@ exists, that is, if patch-index is explicitely disabled.
isPatchIndexDisabled :: FilePath -> IO Bool
isPatchIndexDisabled repodir = doesFileExist (repodir </> darcsdir  </> noPatchIndex)

-- | Create or update patch index
--
--   1. if @_darcs\/no_patch_index@ exists, delete it
--   2. if patch index exists, update it
--   3. if not, create it from scratch
createOrUpdatePatchIndexDisk :: (RepoPatch p, ApplyState p ~ Tree)
                             => Repository rt p wR wU wT -> PatchSet rt p Origin wR -> IO ()
createOrUpdatePatchIndexDisk repo ps = do
   let repodir = repoLocation repo
   removeFile (repodir </> darcsdir </> noPatchIndex) `catch` \(_ :: IOError) -> return ()
   dpie <- doesPatchIndexExist repodir
   if dpie
      then updatePatchIndexDisk repo ps
      else createPatchIndexDisk repo ps

-- | Read-only. Checks the two following things:
--
--   1. 'doesPatchIndexExist'
--   2. 'isPatchIndexDisabled'
--
-- Then only if it exists and it is not explicitely disabled, returns @True@, else returns @False@
-- (or an error if it exists and is explicitely disabled at the same time).
canUsePatchIndex :: Repository rt p wR wU wT -> IO Bool
canUsePatchIndex repo = do
     let repodir = repoLocation repo
     piExists <- doesPatchIndexExist repodir
     piDisabled <- isPatchIndexDisabled repodir
     case (piExists, piDisabled) of
        (True, False) -> return True
        (False, True) -> return False
        (True, True) -> fail "patch index exists, and patch index is disabled. run optimize enable-patch-index or disable-patch-index to rectify."
        (False, False) -> return False

-- | Creates patch-index (ignoring whether it is explicitely disabled).
--   If it is ctrl-c'ed, then aborts, delete patch-index and mark it as disabled.
createPIWithInterrupt :: (RepoPatch p, ApplyState p ~ Tree)
                      => Repository rt p wR wU wT -> PatchSet rt p Origin wR -> IO ()
createPIWithInterrupt repo ps = do
    let repodir = repoLocation repo
    putStrLn "Creating a patch index, please wait. To stop press Ctrl-C"
    (do
      createPatchIndexDisk repo ps
      putStrLn "Created patch index.") `catchInterrupt` (putStrLn "Patch Index Disabled" >> deletePatchIndex repodir)

-- | Checks if patch-index exists and is in sync with repository (more precisely with @_darcs\/hashed_inventory@).
--   That is, checks if patch-index can be used as it is now.
isPatchIndexInSync :: Repository rt p wR wU wT -> IO Bool
isPatchIndexInSync repo = do
   let repodir = repoLocation repo
   dpie <- doesPatchIndexExist repodir
   if dpie
    then do
      (_, inv_hash_pindex, _, _) <- loadPatchIndex repodir
      inv_hash <- getInventoryHash repodir
      return (inv_hash == inv_hash_pindex)
    else return False

-- | Stores patch-index on disk.
storePatchIndex :: FilePath -> FilePath -> String -> PatchIndex -> IO ()
storePatchIndex repodir cdir inv_hash (PatchIndex pids fidspans fpspans infom) = do
  createDirectory cdir `catch` \(_ :: IOError) -> return ()
  tmpdir <- withPermDir repodir $ \dir -> do
              debugMessage "About to create patch index..."
              let tmpdir = toFilePath dir
              storeRepoState (tmpdir </> repoStateFile) inv_hash
              storePatchIds (tmpdir </> pidsFile) pids
              storeInfoMap (tmpdir </> touchMapFile) infom
              storeFidMap (tmpdir </> fidMapFile) fidspans
              storeFpMap (tmpdir </> fpMapFile) fpspans
              debugMessage "Patch index created"
              return tmpdir
  removeDirectoryRecursive cdir `catch` \(_ :: IOError) -> return ()
  renameDirectory tmpdir cdir

decodeFile :: Binary a => FilePath -> IO a
decodeFile path = do
  result <- decodeFileOrFail path
  case result of
    Left (offset, msg) ->
      fail $
        "Patch index is corrupt (file "++path++" at offset "++show offset++"): "++msg++
        "\nPlease remove the corrupt file and then try again."
    Right r -> return r

storeRepoState :: FilePath -> String -> IO ()
storeRepoState fp inv_hash = encodeFile fp (version,inv_hash)

loadRepoState :: FilePath -> IO (Int8, String)
loadRepoState = decodeFile

storePatchIds :: FilePath -> [PatchId] -> IO ()
storePatchIds = encodeFile

loadPatchIds :: FilePath -> IO [PatchId]
loadPatchIds = decodeFile

storeFidMap :: FilePath -> FileIdSpans -> IO ()
storeFidMap fp fidm =
  encodeFile fp $ M.map (map (\(FidSpan a b c) -> (a, b, toIdxM c))) fidm
 where toIdxM Nothing = zero
       toIdxM (Just pid) = pid

loadFidMap :: FilePath -> IO FileIdSpans
loadFidMap fp = M.map (map (\(a,b,c) -> FidSpan a b (toPidM c))) <$> decodeFile fp
  where toPidM pid | pid == zero = Nothing
                   | otherwise   = Just pid

storeFpMap :: FilePath -> FilePathSpans -> IO ()
storeFpMap fp fidm =
  encodeFile fp $ M.map (map (\(FpSpan a b c) -> (a, b, toIdxM c))) fidm
 where toIdxM Nothing = zero
       toIdxM (Just pid) = pid

loadFpMap :: FilePath -> IO FilePathSpans
loadFpMap fp = M.map (map (\(a,b,c) -> FpSpan a b (toPidM c))) <$> decodeFile fp
  where toPidM pid | pid == zero = Nothing
                   | otherwise   = Just pid

storeInfoMap :: FilePath -> InfoMap -> IO ()
storeInfoMap fp infom =
  encodeFile fp $ M.map (\fi -> (isFile fi, touching fi)) infom

loadInfoMap :: FilePath -> IO InfoMap
loadInfoMap fp = M.map (\(isF,pids) -> FileInfo isF pids) <$> decodeFile fp

indexDir, repoStateFile, pidsFile, fidMapFile, fpMapFile,
  touchMapFile, noPatchIndex :: String
indexDir = darcsdir </> "patch_index"
repoStateFile = "repo_state"
pidsFile = "patch_ids"
fidMapFile = "fid_map"
fpMapFile = "fp_map"
touchMapFile = "touch_map"
noPatchIndex = "no_patch_index"

-- | Deletes patch-index (@_darcs\/patch_index\/@ and its contents) and mark repository as disabled (creates @_darcs\/no_patch_index@).
deletePatchIndex :: FilePath -> IO ()
deletePatchIndex repodir = do
    exists <- doesDirectoryExist indexDir
    when exists $
         removeDirectoryRecursive indexDir
            `catch` \(e :: IOError) -> fail $ "Error: Could not delete patch index\n" ++ show e
    (openFile (repodir </> darcsdir </> noPatchIndex) WriteMode >>= hClose)
            `catch` \(e :: IOError) -> fail $ "Error: Could not disable patch index\n" ++ show e

dumpRepoState :: [PatchId] -> String
dumpRepoState = unlines . map pid2string

dumpFileIdSpans :: FileIdSpans -> String
dumpFileIdSpans fidspans =
  unlines [displayPath fn++" -> "++showFileId fid++" from "++pid2string from++" to "++maybe "-" pid2string mto
           | (fn, fids) <- M.toList fidspans, FidSpan fid from mto <- fids]

dumpFilePathSpans :: FilePathSpans -> String
dumpFilePathSpans fpspans =
  unlines [showFileId fid++" -> "++ displayPath fn++" from "++pid2string from++" to "++maybe "-" pid2string mto
           | (fid, fns) <- M.toList fpspans, FpSpan fn from mto <- fns]

dumpTouchingMap :: InfoMap -> String
dumpTouchingMap infom = unlines [showFileId fid++(if isF then "" else "/")++" -> "++ showAsHex w32
                                | (fid,FileInfo isF w32s) <- M.toList infom, w32 <- S.elems w32s]

-- | return set of current filepaths in patch index
fpSpans2filePaths :: FilePathSpans -> InfoMap -> [FilePath]
fpSpans2filePaths fpSpans infom =
  sort [displayPath fn ++ (if isF then "" else "/") | (fid,FpSpan fn _ Nothing:_) <- M.toList fpSpans,
                                                let Just (FileInfo isF _) = M.lookup fid infom]

-- | return set of current filepaths in patch index, for internal use
fpSpans2filePaths' :: FileIdSpans -> [AnchoredPath]
fpSpans2filePaths' fidSpans = [fp | (fp, _)  <- M.toList fidSpans]

-- | Checks if patch index can be created and build it with interrupt.
attemptCreatePatchIndex
  :: (RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wT -> PatchSet rt p Origin wR -> IO ()
attemptCreatePatchIndex repo ps = do
  canCreate <- canCreatePI repo
  when canCreate $ createPIWithInterrupt repo ps

-- | Checks whether a patch index can (and should) be created. If we are not in
-- an old-fashioned repo, and if we haven't been told not to, then we should
-- create a patch index if it doesn't already exist.
canCreatePI :: Repository rt p wR wU wT -> IO Bool
canCreatePI repo =
    (not . or) <$> sequence [ doesntHaveHashedInventory (repoFormat repo)
                            , isPatchIndexDisabled repodir
                            , doesPatchIndexExist repodir
                            ]
  where
    repodir = repoLocation repo
    doesntHaveHashedInventory = return . not . formatHas HashedInventory

-- | Returns an RL in which the order of patches matters. Useful for the
-- @annotate@ command. If patch-index does not exist and is not explicitely
-- disabled, silently create it. (Also, if it is out-of-sync, which should not
-- happen, silently update it).
getRelevantSubsequence
    :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd rt p)
    => Sealed ((RL a) wK)
    -- ^ Sequence of patches you want to filter
    -> Repository rt p wR wU wR
    -- ^ The repository (to attempt loading patch-index from its path)
    -> PatchSet rt p Origin wR
    -- ^ PatchSet of repository (in case we need to create patch-index)
    -> [AnchoredPath]
    -- ^ File(s) about which you want patches from given sequence
    -> IO (Sealed ((RL a) Origin))
    -- ^ Filtered sequence of patches
getRelevantSubsequence pxes repository ps fns = do
    pi@(PatchIndex _ _ _ infom) <- loadSafePatchIndex repository ps
    let fids = map (\fn -> evalState (lookupFid fn) pi) fns
        pidss = map ((\(FileInfo _ a) -> a) . fromJust . (`M.lookup` infom)) fids
        pids = S.unions pidss
    let flpxes = reverseRL $ unseal unsafeCoercePEnd pxes
    return . seal $ keepElems flpxes NilRL pids
  where
    keepElems :: (RepoPatch p, ApplyState p ~ Tree, a ~ PatchInfoAnd rt p)
              => FL a wX wY -> RL a wB wX -> S.Set Word32 -> RL a wP wQ
    keepElems NilFL acc _ = unsafeCoerceP acc
    keepElems (x :>: xs) acc pids
      | short (makePatchID $ info x) `S.member` pids = keepElems xs (acc :<: x) pids
      | otherwise = keepElems (unsafeCoerceP xs) acc pids

type PatchFilter rt p = [AnchoredPath] -> [Sealed2 (PatchInfoAnd rt p)] -> IO [Sealed2 (PatchInfoAnd rt p)]

-- | If a patch index is available, returns a filter that takes a list of files and returns
--   a @PatchFilter@ that only keeps patches that modify the given list of files.
--   If patch-index cannot be used, return the original input.
--   If patch-index does not exist and is not explicitely disabled, silently create it.
--   (Also, if it is out-of-sync, which should not happen, silently update it).
maybeFilterPatches
    :: (RepoPatch p, ApplyState p ~ Tree)
    => Repository rt p wR wU wT  -- ^ The repository
    -> PatchSet rt p Origin wR   -- ^ PatchSet of patches of repository (in case patch-index needs to be created)
    -> PatchFilter rt p          -- ^ PatchFilter ready to be used by SelectChanges.
maybeFilterPatches repo ps fps ops = do
    usePI <- canUsePatchIndex repo
    if usePI
      then do
        pi@(PatchIndex _ _ _ infom) <- loadSafePatchIndex repo ps
        let fids = concatMap ((\fn -> evalState (lookupFids' fn) pi)) fps
            npids = S.unions $ map (touching.fromJust.(`M.lookup` infom)) fids
        return $ filter
          (flip S.member npids . (unseal2 (short . makePatchID . info))) ops
      else return ops

-- | Dump information in patch index. Patch-index should be checked to exist beforehand. Read-only.
dumpPatchIndex :: FilePath -> IO ()
dumpPatchIndex repodir = do
  (_,inv_hash,_,PatchIndex pids fidspans fpspans infom) <- loadPatchIndex repodir
  putStrLn $ unlines $
    [ "Inventory hash:" ++ inv_hash
    , "================="
    , "Repo state:"
    , "==========="
    , dumpRepoState pids
    , "Fileid spans:"
    , "============="
    , dumpFileIdSpans fidspans
    , "Filepath spans:"
    , "=============="
    , dumpFilePathSpans fpspans
    , "Info Map:"
    , "========="
    , dumpTouchingMap infom
    , "Files:"
    , "=============="
    ] ++ fpSpans2filePaths fpspans infom

-- | Read-only sanity check on patch-index. Patch-index should be checked to exist beforehand. It may not be in sync with repository.
piTest :: FilePath -> IO ()
piTest repodir = do
   (_,_,_,PatchIndex rpids fidspans fpspans infom) <- loadPatchIndex repodir
   let pids = reverse rpids

   -- test fidspans
   putStrLn "fidspans"
   putStrLn "==========="
   forM_ (M.toList fidspans) $ \(fn, spans) -> do
      let g :: FileIdSpan -> [PatchId]
          g (FidSpan _ x (Just y)) = [y,x]
          g (FidSpan _ x _) = [x]
          ascTs = reverse . nub . concat $ map g spans
      unless (isInOrder ascTs pids) (fail $ "In order test failed! filename: " ++ show fn)
      forM_ spans $ \(FidSpan fid _ _) -> unless (M.member fid fpspans) (fail $ "Valid file id test failed! fid: " ++ show fid)
   putStrLn "fidspans tests passed"

   -- test fpspans
   putStrLn "fpspans"
   putStrLn "==========="
   forM_ (M.toList fpspans) $ \(fid, spans) -> do
      let g :: FilePathSpan -> [PatchId]
          g (FpSpan _ x (Just y)) = [y,x]
          g (FpSpan _ x _) = [x]
          ascTs = reverse . nub . concat $ map g spans
      unless (isInOrder ascTs pids) (fail $ "In order test failed! fileid: " ++ show fid)
      forM_ spans $ \(FpSpan fn _ _) -> unless (M.member fn fidspans) (fail $ "Valid file name test failed! file name: " ++ show fn)
      let f :: FilePathSpan -> FilePathSpan -> Bool
          f (FpSpan _ x _) (FpSpan _ _ (Just y)) = x == y
          f _ _ = error "adj test of fpspans fail"
      unless (and $ zipWith f spans (tail spans)) (fail $ "Adjcency test failed! fid: " ++ show fid)
   putStrLn "fpspans tests passed"

   -- test infomap
   putStrLn "infom"
   putStrLn "==========="
   putStrLn $ "Valid fid test: " ++ (show.and $ map (`M.member` fpspans) (M.keys infom))
   putStrLn $ "Valid pid test: " ++ (show.flip S.isSubsetOf (S.fromList $ map short pids)  . S.unions . map touching . M.elems $ infom)
   where
          isInOrder :: Eq a => [a] -> [a] -> Bool
          isInOrder (x:xs) (y:ys) | x == y = isInOrder xs ys
                                  | otherwise = isInOrder (x:xs) ys
          isInOrder [] _ = True
          isInOrder _ [] = False


