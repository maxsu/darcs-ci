module Darcs.Repository.Clone
    ( cloneRepository
    , replacePristine
    ) where

import Darcs.Prelude

import Control.Exception ( catch, SomeException )
import Control.Monad ( unless, void, when )
import qualified Data.ByteString.Char8 as BC
import Data.List( intercalate )
import Data.Maybe( catMaybes )
import System.FilePath( (</>) )
import System.Directory
    ( removeFile
    , listDirectory
    )
import System.IO ( stderr )

import Darcs.Repository.Create
    ( EmptyRepository(..)
    , createRepository
    , writePristine
    )
import Darcs.Repository.State ( invalidateIndex )
import Darcs.Repository.Identify ( identifyRepositoryFor, ReadingOrWriting(..) )
import Darcs.Repository.Pristine
    ( ApplyDir(..)
    , applyToTentativePristineCwd
    , createPristineDirectoryTree
    )
import Darcs.Repository.Hashed
    ( copyHashedInventory
    , finalizeRepositoryChanges
    , finalizeTentativeChanges
    , readRepo
    , revertRepositoryChanges
    , revertTentativeChanges
    , tentativelyRemovePatches
    , writeTentativeInventory
    )
import Darcs.Repository.Working
    ( setScriptsExecutable
    , setScriptsExecutablePatches )
import Darcs.Repository.InternalTypes
    ( Repository
    , repoLocation
    , repoFormat
    , repoCache
    , modifyCache
    )
import Darcs.Repository.Job ( withUMaskFlag )
import Darcs.Repository.Cache
    ( unionRemoteCaches
    , unionCaches
    , fetchFileUsingCache
    , speculateFileUsingCache
    , HashedDir(..)
    , repo2cache
    , dropNonRepos
    )

import Darcs.Repository.ApplyPatches ( runDefault )
import Darcs.Repository.Inventory
    ( peekPristineHash
    , getValidHash
    )
import Darcs.Repository.Format
    ( RepoProperty ( HashedInventory, Darcs2, Darcs3 )
    , RepoFormat
    , formatHas
    , readProblem
    )
import Darcs.Repository.Prefs ( addRepoSource, deleteSources )
import Darcs.Repository.Match ( getOnePatchset )
import Darcs.Util.External
    ( copyFileOrUrl
    , Cachable(..)
    , gzFetchFilePS
    )
import Darcs.Repository.PatchIndex
    ( doesPatchIndexExist
    , createPIWithInterrupt
    )
import Darcs.Repository.Packs
    ( fetchAndUnpackBasic
    , fetchAndUnpackPatches
    , packsDir
    )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , patchsetConflictResolutions
    , announceConflicts
    )
import Darcs.Repository.Working ( applyToWorking )
import Darcs.Util.Lock ( appendTextFile, withNewDirectory )
import Darcs.Repository.Flags
    ( UpdatePending(..)
    , UseCache(..)
    , RemoteDarcs (..)
    , remoteDarcs
    , Compression (..)
    , CloneKind (..)
    , Verbosity (..)
    , DryRun (..)
    , UMask (..)
    , SetScriptsExecutable (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , InheritDefault (..)
    , WithWorkingDir (..)
    , ForgetParent (..)
    , WithPatchIndex (..)
    , PatchFormat (..)
    , AllowConflicts(..)
    , ExternalMerge(..)
    )

import Darcs.Patch ( RepoPatch, IsRepoType, description )
import Darcs.Patch.Depends ( findUncommon )
import Darcs.Patch.Set ( patchSet2RL
                       , patchSet2FL
                       , progressPatchSet
                       )
import Darcs.Patch.Match ( MatchFlag(..), patchSetMatch )
import Darcs.Patch.Progress ( progressRLShowTags, progressFL )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , (:\/:)(..)
    , lengthFL
    , bunchFL
    , mapFL
    , mapRL
    , lengthRL
    , nullFL
    )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, extractHash )

import Darcs.Util.Tree( Tree, emptyTree )

import Darcs.Util.Download ( maxPipelineLength )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.SignalHandler ( catchInterrupt, withSignalsBlocked )
import Darcs.Util.Printer ( Doc, ($$), hPutDocLn, hsep, putDocLn, text )
import Darcs.Util.Printer.Color ( unsafeRenderStringColored )
import Darcs.Util.Progress
    ( debugMessage
    , tediousSize
    , beginTedious
    , endTedious
    )

joinUrl :: [String] -> String
joinUrl = intercalate "/"

cloneRepository ::
    String    -- origin repository path
    -> String -- new repository name (for relative path)
    -> Verbosity -> UseCache
    -> CloneKind
    -> UMask -> RemoteDarcs
    -> SetScriptsExecutable
    -> RemoteRepos
    -> SetDefault
    -> InheritDefault
    -> [MatchFlag]
    -> RepoFormat
    -> WithWorkingDir
    -> WithPatchIndex   -- use patch index
    -> Bool   -- use packs
    -> ForgetParent
    -> IO ()
cloneRepository repourl mysimplename v useCache cloneKind um rdarcs sse remoteRepos
                setDefault inheritDefault matchFlags rfsource withWorkingDir
                usePatchIndex usePacks forget =
  withUMaskFlag um $ withNewDirectory mysimplename $ do
      let patchfmt
            | formatHas Darcs3 rfsource = PatchFormat3
            | formatHas Darcs2 rfsource = PatchFormat2
            | otherwise                 = PatchFormat1
      EmptyRepository _toRepo <-
        createRepository patchfmt withWorkingDir
          (if cloneKind == LazyClone then NoPatchIndex else usePatchIndex) useCache
      debugMessage "Finished initializing new repository."
      addRepoSource repourl NoDryRun remoteRepos setDefault inheritDefault False

      debugMessage "Identifying and copying repository..."
      fromRepo <- identifyRepositoryFor Reading _toRepo useCache repourl
      let fromLoc = repoLocation fromRepo
      let rffrom = repoFormat fromRepo
      case readProblem rffrom of
        Just e ->  fail $ "Incompatibility with repository " ++ fromLoc ++ ":\n" ++ e
        Nothing -> return ()
      debugMessage "Copying prefs..."
      copyFileOrUrl (remoteDarcs rdarcs)
        (joinUrl [fromLoc, darcsdir, "prefs", "prefs"])
        (darcsdir </> "prefs/prefs") (MaxAge 600) `catchall` return ()
      debugMessage "Copying sources..."
      cache <- unionRemoteCaches (repoCache _toRepo) (repoCache fromRepo) fromLoc
      appendTextFile (darcsdir </> "prefs/sources")
                     (show $ repo2cache fromLoc `unionCaches` dropNonRepos cache)
      debugMessage "Done copying and filtering sources."
      -- put remote source last
      _toRepo <- return $
        modifyCache (const $ cache `unionCaches` repo2cache fromLoc) _toRepo
      if formatHas HashedInventory rffrom then do
       debugMessage "Copying basic repository (hashed_inventory and pristine)"
       if usePacks && (not . isValidLocalPath) fromLoc
         then copyBasicRepoPacked    fromRepo _toRepo v rdarcs withWorkingDir
         else copyBasicRepoNotPacked fromRepo _toRepo v rdarcs withWorkingDir
       when (cloneKind /= LazyClone) $ do
         when (cloneKind /= CompleteClone) $
           putInfo v $ text "Copying patches, to get lazy repository hit ctrl-C..."
         debugMessage "Copying complete repository (inventories and patches)"
         if usePacks && (not . isValidLocalPath) fromLoc
           then copyCompleteRepoPacked    fromRepo _toRepo v cloneKind
           else copyCompleteRepoNotPacked fromRepo _toRepo v cloneKind
      else
       -- old-fashioned repositories are cloned diferently since
       -- we need to copy all patches first and then build pristine
       copyRepoOldFashioned fromRepo _toRepo v withWorkingDir
      when (sse == YesSetScriptsExecutable) setScriptsExecutable
      case patchSetMatch matchFlags of
       Nothing -> return ()
       Just psm -> do
        putInfo v $ text "Going to specified version..."
        -- the following is necessary to be able to read _toRepo's patches
        _toRepo <- revertRepositoryChanges _toRepo NoUpdatePending
        patches <- readRepo _toRepo
        Sealed context <- getOnePatchset _toRepo psm
        to_remove :\/: only_in_context <- return $ findUncommon patches context
        case only_in_context of
          NilFL -> do
            let num_to_remove = lengthFL to_remove
            putInfo v $ hsep $ map text
              [ "Unapplying"
              , show num_to_remove
              , englishNum num_to_remove (Noun "patch") ""
              ]
            invalidateIndex _toRepo
            _toRepo <-
              tentativelyRemovePatches _toRepo GzipCompression NoUpdatePending to_remove
            _toRepo <-
              finalizeRepositoryChanges _toRepo NoUpdatePending GzipCompression
            runDefault (unapply to_remove) `catch` \(e :: SomeException) ->
                fail ("Couldn't undo patch in working tree.\n" ++ show e)
            when (sse == YesSetScriptsExecutable) $ setScriptsExecutablePatches to_remove
          _ ->
            -- This can only happen if the user supplied a context file that
            -- doesn't specify a subset of the remote repo.
            fail $ unsafeRenderStringColored
              $ text "Missing patches from context:"
              $$ description only_in_context
      when (forget == YesForgetParent) deleteSources
      -- check for unresolved conflicts
      patches <- readRepo _toRepo
      let conflicts = patchsetConflictResolutions patches
      _ <- announceConflicts "clone" YesAllowConflictsAndMark NoExternalMerge conflicts
      Sealed mangled_res <- return $ mangled conflicts
      unless (nullFL mangled_res) $
        withSignalsBlocked $ void $ applyToWorking _toRepo v mangled_res

putInfo :: Verbosity -> Doc -> IO ()
putInfo Quiet _ = return ()
putInfo _ d = hPutDocLn stderr d

putVerbose :: Verbosity -> Doc -> IO ()
putVerbose Verbose d = putDocLn d
putVerbose _ _ = return ()

copyBasicRepoNotPacked  :: forall rt p wR wU wT.
                           Repository rt p wR wU wT -- remote
                        -> Repository rt p wR wU wT -- existing empty local
                        -> Verbosity
                        -> RemoteDarcs
                        -> WithWorkingDir
                        -> IO ()
copyBasicRepoNotPacked fromRepo toRepo verb rdarcs withWorkingDir = do
  putVerbose verb $ text "Copying hashed inventory from remote repo..."
  copyHashedInventory toRepo rdarcs (repoLocation fromRepo)
  putVerbose verb $ text "Writing pristine and working tree contents..."
  createPristineDirectoryTree toRepo "." withWorkingDir

copyCompleteRepoNotPacked :: forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                        => Repository rt p wR wU wT -- remote
                        -> Repository rt p wR wU wT -- existing basic local
                        -> Verbosity
                        -> CloneKind
                        -> IO ()
copyCompleteRepoNotPacked _ toRepo verb cloneKind = do
       let cleanup = putInfo verb $ text "Using lazy repository."
       allowCtrlC cloneKind cleanup $ do
         fetchPatchesIfNecessary toRepo
         pi <- doesPatchIndexExist (repoLocation toRepo)
         ps <- readRepo toRepo
         when pi $ createPIWithInterrupt toRepo ps

copyBasicRepoPacked ::
  forall rt p wR wU wT.
     Repository rt p wR wU wT -- remote
  -> Repository rt p wR wU wT -- existing empty local repository
  -> Verbosity
  -> RemoteDarcs
  -> WithWorkingDir
  -> IO ()
copyBasicRepoPacked fromRepo toRepo verb rdarcs withWorkingDir =
  do let fromLoc = repoLocation fromRepo
     let hashURL = joinUrl [fromLoc, darcsdir, packsDir, "pristine"]
     mPackHash <- (Just <$> gzFetchFilePS hashURL Uncachable) `catchall` (return Nothing)
     let hiURL = joinUrl [fromLoc, darcsdir, "hashed_inventory"]
     i <- gzFetchFilePS hiURL Uncachable
     let currentHash = BC.pack $ getValidHash $ peekPristineHash i
     let copyNormally = copyBasicRepoNotPacked fromRepo toRepo verb rdarcs withWorkingDir
     case mPackHash of
      Just packHash | packHash == currentHash
              -> ( do copyBasicRepoPacked2 fromRepo toRepo verb withWorkingDir
                      -- need to obtain a fresh copy of hashed_inventory as reference
                      putVerbose verb $ text "Copying hashed inventory from remote repo..."
                      copyHashedInventory toRepo rdarcs (repoLocation fromRepo)
                   `catch` \(e :: SomeException) ->
                               do putStrLn ("Exception while getting basic pack:\n" ++ show e)
                                  copyNormally)
      _       -> do putVerbose verb $
                      text "Remote repo has no basic pack or outdated basic pack, copying normally."
                    copyNormally

copyBasicRepoPacked2 ::
  forall rt p wR wU wT.
     Repository rt p wR wU wT -- remote
  -> Repository rt p wR wU wT -- existing empty local repository
  -> Verbosity
  -> WithWorkingDir
  -> IO ()
copyBasicRepoPacked2 fromRepo toRepo verb withWorkingDir = do
  putVerbose verb $ text "Cloning packed basic repository."
  -- unpack inventory & pristine cache
  cleanDir $ darcsdir </> "pristine.hashed"
  removeFile $ darcsdir </> "hashed_inventory"
  fetchAndUnpackBasic (repoCache toRepo) (repoLocation fromRepo)
  putInfo verb $ text "Done fetching and unpacking basic pack."
  createPristineDirectoryTree toRepo "." withWorkingDir

copyCompleteRepoPacked ::
  forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wT -- remote
  -> Repository rt p wR wU wT -- existing basic local repository
  -> Verbosity
  -> CloneKind
  -> IO ()
copyCompleteRepoPacked from to verb cloneKind =
    copyCompleteRepoPacked2 from to verb cloneKind
  `catch`
    \(e :: SomeException) -> do
      putStrLn ("Exception while getting patches pack:\n" ++ show e)
      putVerbose verb $ text "Problem while copying patches pack, copying normally."
      copyCompleteRepoNotPacked from to verb cloneKind

copyCompleteRepoPacked2 ::
  forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wT
  -> Repository rt p wR wU wT
  -> Verbosity
  -> CloneKind
  -> IO ()
copyCompleteRepoPacked2 fromRepo toRepo verb cloneKind = do
  us <- readRepo toRepo
  -- get old patches
  let cleanup = putInfo verb $ text "Using lazy repository."
  allowCtrlC cloneKind cleanup $ do
    putVerbose verb $ text "Using patches pack."
    fetchAndUnpackPatches (mapRL hashedPatchFileName $ patchSet2RL us)
      (repoCache toRepo) (repoLocation fromRepo)
    pi <- doesPatchIndexExist (repoLocation toRepo)
    when pi $ createPIWithInterrupt toRepo us -- TODO or do another readRepo?

cleanDir :: FilePath -> IO ()
cleanDir d = mapM_ (\x -> removeFile $ d </> x) =<< listDirectory d

copyRepoOldFashioned :: forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                        => Repository rt p wR wU wT  -- remote repo
                        -> Repository rt p wR wU wT  -- local empty repo
                        -> Verbosity
                        -> WithWorkingDir
                        -> IO ()
copyRepoOldFashioned fromrepository _toRepo verb withWorkingDir = do
  revertTentativeChanges
  patches <- readRepo fromrepository
  let k = "Copying patch"
  beginTedious k
  tediousSize k (lengthRL $ patchSet2RL patches)
  let patches' = progressPatchSet k patches
  writeTentativeInventory (repoCache _toRepo) GzipCompression patches'
  endTedious k
  finalizeTentativeChanges _toRepo GzipCompression
  -- apply all patches into current hashed repository
  _toRepo <- revertRepositoryChanges _toRepo NoUpdatePending
  local_patches <- readRepo _toRepo
  replacePristine _toRepo emptyTree
  let patchesToApply = progressFL "Applying patch" $ patchSet2FL local_patches
  sequence_ $ mapFL (applyToTentativePristineCwd ApplyNormal) $ bunchFL 100 patchesToApply
  _toRepo <- finalizeRepositoryChanges _toRepo NoUpdatePending GzipCompression
  putVerbose verb $ text "Writing pristine and working tree contents..."
  createPristineDirectoryTree _toRepo "." withWorkingDir

-- | This function fetches all patches that the given repository has
--   with fetchFileUsingCache.
fetchPatchesIfNecessary :: forall rt p wR wU wT. (IsRepoType rt, RepoPatch p)
                        => Repository rt p wR wU wT
                        -> IO ()
fetchPatchesIfNecessary toRepo =
  do  ps <- readRepo toRepo
      pipelineLength <- maxPipelineLength
      let patches = patchSet2RL ps
          ppatches = progressRLShowTags "Copying patches" patches
          (first, other) = splitAt (pipelineLength - 1) $ tail $ hashes patches
          speculate | pipelineLength > 1 = [] : first : map (:[]) other
                    | otherwise = []
      mapM_ fetchAndSpeculate $ zip (hashes ppatches) (speculate ++ repeat [])
  where hashes :: forall wX wY . RL (PatchInfoAnd rt p) wX wY -> [String]
        hashes = catMaybes . mapRL (either (const Nothing) Just . extractHash)
        fetchAndSpeculate :: (String, [String]) -> IO ()
        fetchAndSpeculate (f, ss) = do
          _ <- fetchFileUsingCache c HashedPatchesDir f
          mapM_ (speculateFileUsingCache c HashedPatchesDir) ss
        c = repoCache toRepo

-- | Replace the existing pristine with a new one (loaded up in a Tree object).
replacePristine :: Repository rt p wR wU wT -> Tree IO -> IO ()
replacePristine = writePristine . repoLocation

allowCtrlC :: CloneKind -> IO () -> IO () -> IO ()
allowCtrlC CompleteClone _       action = action
allowCtrlC _             cleanup action = action `catchInterrupt` cleanup

hashedPatchFileName :: PatchInfoAnd rt p wA wB -> String
hashedPatchFileName x = case extractHash x of
  Left _ -> fail "unexpected unhashed patch"
  Right h -> h
