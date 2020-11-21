-- Copyright (C) 2006-2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
{-# LANGUAGE OverloadedStrings #-}
module Darcs.Repository.Hashed
    ( revertTentativeChanges
    , revertRepositoryChanges
    , finalizeTentativeChanges
    , addToTentativeInventory
    , readRepo
    , readRepoHashed
    , readTentativeRepo
    , writeAndReadPatch
    , writeTentativeInventory
    , copyHashedInventory
    , writePatchIfNecessary
    , tentativelyAddPatch
    , tentativelyRemovePatches
    , tentativelyRemovePatches_
    , tentativelyAddPatch_
    , tentativelyAddPatches_
    , finalizeRepositoryChanges
    , reorderInventory
    , UpdatePristine(..)
    , repoXor
    , upgradeOldStyleRebase
    ) where

import Darcs.Prelude

import Control.Exception ( catch )
import Darcs.Util.Exception ( catchall )
import Control.Monad ( when, unless )
import Data.Maybe
import Data.List( foldl' )

import qualified Data.ByteString as B ( empty, readFile, append )
import qualified Data.ByteString.Char8 as BC ( pack )

import Darcs.Util.Hash( SHA1, sha1Xor, sha1zero )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Tree ( Tree )
import Darcs.Util.SignalHandler ( withSignalsBlocked )

import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , removeFile
    , renameFile
    )
import System.FilePath.Posix( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.IO ( IOMode(..), hClose, hPutStrLn, openBinaryFile, stderr )
import System.IO.Error ( catchIOError, isDoesNotExistError )

import Darcs.Util.External
    ( copyFileOrUrl
    , cloneFile
    , gzFetchFilePS
    , Cachable( Uncachable )
    )
import Darcs.Repository.Flags
    ( Compression
    , RemoteDarcs
    , UpdatePending(..)
    , Verbosity(..)
    , remoteDarcs
    )

import Darcs.Repository.Format
    ( RepoProperty( HashedInventory, RebaseInProgress, RebaseInProgress_2_16 )
    , formatHas
    , writeRepoFormat
    , addToFormat
    , removeFromFormat
    )
import Darcs.Repository.Pending
    ( tentativelyRemoveFromPending
    , revertPending
    , finalizePending
    , readTentativePending
    , writeTentativePending
    )
import Darcs.Repository.PatchIndex
    ( createOrUpdatePatchIndexDisk
    , doesPatchIndexExist
    )
import Darcs.Repository.Pristine
    ( ApplyDir(..)
    , applyToTentativePristine
    , applyToTentativePristineCwd
    )
import Darcs.Repository.Paths
import Darcs.Repository.Rebase
    ( withTentativeRebase
    , createTentativeRebase
    , readTentativeRebase
    , writeTentativeRebase
    , commuteOutOldStyleRebase
    )
import Darcs.Repository.State ( readRecorded, updateIndex )

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Lock
    ( writeBinFile
    , writeDocBinFile
    , writeAtomicFilePS
    , appendDocBinFile
    , removeFileMayNotExist
    )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..)
                       , SealedPatchSet, Origin
                       , patchSet2RL
                       )

import Darcs.Patch.Show ( ShowPatchFor(..) )
import qualified Darcs.Patch.Named.Wrapped as W
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd, PatchInfoAndG, Hopefully, patchInfoAndPatch, info
    , extractHash, createHashed, hopefully
    , fmapPIAP
    )
import Darcs.Patch ( IsRepoType, RepoPatch, showPatch
                   , commuteRL
                   , readPatch
                   , effect
                   , displayPatch
                   )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Bundle ( Bundle(..), makeBundle, interpretBundle, parseBundle )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Depends ( removeFromPatchSet, slightlyOptimizePatchset
                           , mergeThem, cleanLatestTag )
import Darcs.Patch.Info
    ( PatchInfo, displayPatchInfo, makePatchname )
import Darcs.Patch.Rebase.Suspended
    ( Suspended(..), addFixupsToSuspended, removeFixupsFromSuspended )

import Darcs.Util.Path ( ioAbsoluteOrRemote, toPath )
import Darcs.Repository.Cache
    ( Cache
    , HashedDir(..)
    , fetchFileUsingCache
    , hashedDir
    , peekInCache
    , speculateFilesUsingCache
    , writeFileUsingCache
    )
import Darcs.Repository.Inventory
import Darcs.Repository.InternalTypes
    ( Repository
    , repoCache
    , repoFormat
    , repoLocation
    , withRepoLocation
    , unsafeCoerceR
    , unsafeCoerceT
    )
import qualified Darcs.Repository.Old as Old ( readOldRepo, oldRepoFailMsg )
import Darcs.Patch.Witnesses.Ordered
    ( (+<+), FL(..), RL(..), mapRL, foldFL_M, foldrwFL, mapRL_RL
    , (:>)(..), lengthFL, (+>+)
    , reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal, unseal, mapSeal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Printer.Color ( debugDoc, ePutDocLn )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , (<+>)
    , hcat
    , renderPS
    , renderString
    , text
    )
import Darcs.Util.Progress ( beginTedious, endTedious, debugMessage, finishedOneIO )
import Darcs.Patch.Progress (progressFL)


-- |revertTentativeChanges swaps the tentative and "real" hashed inventory
-- files, and then updates the tentative pristine with the "real" inventory
-- hash.
revertTentativeChanges :: IO ()
revertTentativeChanges = do
    cloneFile hashedInventoryPath tentativeHashedInventoryPath
    i <- gzReadFilePS hashedInventoryPath
    writeBinFile tentativePristinePath $
        B.append pristineName $ BC.pack $ getValidHash $ peekPristineHash i

-- |finalizeTentativeChanges trys to atomically swap the tentative
-- inventory/pristine pointers with the "real" pointers; it first re-reads the
-- inventory to optimize it, presumably to take account of any new tags, and
-- then writes out the new tentative inventory, and finally does the atomic
-- swap. In general, we can't clean the pristine cache at the same time, since
-- a simultaneous get might be in progress.
finalizeTentativeChanges :: (IsRepoType rt, RepoPatch p)
                         => Repository rt p wR wU wT -> Compression -> IO ()
finalizeTentativeChanges r compr = do
    debugMessage "Optimizing the inventory..."
    -- Read the tentative patches
    ps <- readTentativeRepo r "."
    writeTentativeInventory (repoCache r) compr ps
    i <- gzReadFilePS tentativeHashedInventoryPath
    p <- gzReadFilePS tentativePristinePath
    -- Write out the "optimised" tentative inventory.
    writeDocBinFile tentativeHashedInventoryPath $ pokePristineHash (peekPristineHash p) i
    -- Atomically swap.
    renameFile tentativeHashedInventoryPath hashedInventoryPath

-- | Add (append) a patch to a specific inventory file.
-- | Warning: this allows to add any arbitrary patch!
addToSpecificInventory :: RepoPatch p => String -> Cache -> Compression
                       -> PatchInfoAnd rt p wX wY -> IO ()
addToSpecificInventory invPath c compr p = do
    let invFile = makeDarcsdirPath invPath
    hash <- snd <$> writePatchIfNecessary c compr p
    appendDocBinFile invFile $ showInventoryEntry (info p, hash)

-- | Add (append) a patch to the tentative inventory.
-- | Warning: this allows to add any arbitrary patch! Used by convert import.
addToTentativeInventory :: RepoPatch p => Cache -> Compression
                        -> PatchInfoAnd rt p wX wY -> IO ()
addToTentativeInventory = addToSpecificInventory tentativeHashedInventory

-- |writeHashFile takes a Doc and writes it as a hash-named file, returning the
-- filename that the contents were written to.
writeHashFile :: Cache -> Compression -> HashedDir -> Doc -> IO String
writeHashFile c compr subdir d = do
    debugMessage $ "Writing hash file to " ++ hashedDir subdir
    writeFileUsingCache c compr subdir $ renderPS d

-- |readRepo returns the "current" repo patchset.
readRepoHashed :: (IsRepoType rt, RepoPatch p) => Repository rt p wR wU wT
               -> String -> IO (PatchSet rt p Origin wR)
readRepoHashed = readRepoUsingSpecificInventory hashedInventory

-- |readRepo returns the tentative repo patchset.
readTentativeRepo :: (IsRepoType rt, PatchListFormat p, ReadPatch p)
                  => Repository rt p wR wU wT -> String
                  -> IO (PatchSet rt p Origin wT)
readTentativeRepo = readRepoUsingSpecificInventory tentativeHashedInventory

-- |readRepoUsingSpecificInventory uses the inventory at @invPath@ to read the
-- repository @repo@.
readRepoUsingSpecificInventory :: (IsRepoType rt, PatchListFormat p, ReadPatch p)
                               => String -> Repository rt p wR wU wT
                               -> String -> IO (PatchSet rt p Origin wS)
readRepoUsingSpecificInventory invPath repo dir = do
    realdir <- toPath <$> ioAbsoluteOrRemote dir
    Sealed ps <- readRepoPrivate (repoCache repo) realdir invPath
                 `catch` \e -> do
                     hPutStrLn stderr ("Invalid repository: " ++ realdir)
                     ioError e
    return $ unsafeCoerceP ps
  where
    readRepoPrivate :: (IsRepoType rt, PatchListFormat p, ReadPatch p)
                    => Cache -> FilePath
                    -> FilePath -> IO (SealedPatchSet rt p Origin)
    readRepoPrivate cache d iname = do
      inventory <- readInventoryPrivate (d </> darcsdir </> iname)
      readRepoFromInventoryList cache inventory

-- | Read a 'PatchSet' from the repository (assumed to be located at the
-- current working directory) by following the chain of 'Inventory's, starting
-- with the given one. The 'Cache' parameter is used to locate patches and parent
-- inventories, since not all of them need be present inside the current repo.
readRepoFromInventoryList
  :: (IsRepoType rt, PatchListFormat p, ReadPatch p)
  => Cache
  -> Inventory
  -> IO (SealedPatchSet rt p Origin)
readRepoFromInventoryList cache = parseInv
  where
    parseInv :: (IsRepoType rt, PatchListFormat p, ReadPatch p)
             => Inventory
             -> IO (SealedPatchSet rt p Origin)
    parseInv (Inventory Nothing ris) =
        mapSeal (PatchSet NilRL) <$> readPatchesFromInventory cache ris
    parseInv (Inventory (Just h) []) =
        -- TODO could be more tolerant and create a larger PatchSet
        error $ "bad inventory " ++ getValidHash h ++ " (no tag) in parseInv!"
    parseInv (Inventory (Just h) (t : ris)) = do
        Sealed ts <- unseal seal <$> unsafeInterleaveIO (read_ts t h)
        Sealed ps <- unseal seal <$>
                        unsafeInterleaveIO (readPatchesFromInventory cache ris)
        return $ seal $ PatchSet ts ps

    read_ts :: (IsRepoType rt, PatchListFormat p, ReadPatch p) => InventoryEntry
            -> InventoryHash -> IO (Sealed (RL (Tagged rt p) Origin))
    read_ts tag0 h0 = do
        contents <- unsafeInterleaveIO $ readTaggedInventory h0
        let is = case contents of
                    (Inventory (Just _) (_ : ris0)) -> ris0
                    (Inventory Nothing ris0) -> ris0
                    (Inventory (Just _) []) -> error "inventory without tag!"
        Sealed ts <- unseal seal <$>
                         unsafeInterleaveIO
                            (case contents of
                                 (Inventory (Just h') (t' : _)) -> read_ts t' h'
                                 (Inventory (Just _) []) -> error "inventory without tag!"
                                 (Inventory Nothing _) -> return $ seal NilRL)
        Sealed ps <- unseal seal <$>
            unsafeInterleaveIO (readPatchesFromInventory cache is)
        Sealed tag00 <- read_tag tag0
        return $ seal $ ts :<: Tagged tag00 (Just (getValidHash h0)) ps

    read_tag :: (PatchListFormat p, ReadPatch p) => InventoryEntry
             -> IO (Sealed (PatchInfoAnd rt p wX))
    read_tag (i, h) =
        mapSeal (patchInfoAndPatch i) <$> createValidHashed h (readSinglePatch cache i)

    readTaggedInventory :: InventoryHash -> IO Inventory
    readTaggedInventory invHash = do
        (fileName, pristineAndInventory) <-
            fetchFileUsingCache cache HashedInventoriesDir (getValidHash invHash)
        case parseInventory pristineAndInventory of
          Right r -> return r
          Left e -> fail $ unlines [unwords ["parse error in file", fileName],e]

readPatchesFromInventory :: ReadPatch np
                         => Cache
                         -> [InventoryEntry]
                         -> IO (Sealed (RL (PatchInfoAndG rt np) wX))
readPatchesFromInventory cache ris = read_patches (reverse ris)
  where
    read_patches [] = return $ seal NilRL
    read_patches allis@((i1, h1) : is1) =
        lift2Sealed (\p rest -> rest :<: i1 `patchInfoAndPatch` p) (rp is1)
                    (createValidHashed h1 (const $ speculateAndParse h1 allis i1))
      where
        rp [] = return $ seal NilRL
        rp [(i, h), (il, hl)] =
            lift2Sealed (\p rest -> rest :<: i `patchInfoAndPatch` p)
                        (rp [(il, hl)])
                        (createValidHashed h
                            (const $ speculateAndParse h (reverse allis) i))
        rp ((i, h) : is) =
            lift2Sealed (\p rest -> rest :<: i `patchInfoAndPatch` p)
                        (rp is)
                        (createValidHashed h (readSinglePatch cache i))

    lift2Sealed :: (forall wY wZ . q wY wZ -> p wX wY -> r wX wZ)
                -> IO (Sealed (p wX))
                -> (forall wB . IO (Sealed (q wB)))
                -> IO (Sealed (r wX))
    lift2Sealed f iox ioy = do
        Sealed x <- unseal seal <$> unsafeInterleaveIO iox
        Sealed y <- unseal seal <$> unsafeInterleaveIO ioy
        return $ seal $ f y x

    speculateAndParse h is i = speculate h is >> readSinglePatch cache i h

    speculate :: PatchHash -> [InventoryEntry] -> IO ()
    speculate pHash is = do
        already_got_one <- peekInCache cache HashedPatchesDir (getValidHash pHash)
        unless already_got_one $
            speculateFilesUsingCache cache HashedPatchesDir (map (getValidHash . snd) is)

readSinglePatch :: ReadPatch p
                => Cache
                -> PatchInfo -> PatchHash -> IO (Sealed (p wX))
readSinglePatch cache i h = do
    debugDoc $ text "Reading patch file:" <+> displayPatchInfo i
    (fn, ps) <- fetchFileUsingCache cache HashedPatchesDir (getValidHash h)
    case readPatch ps of
        Right p -> return p
        Left e -> fail $ unlines
            [ "Couldn't parse file " ++ fn
            , "which is patch"
            , renderString $ displayPatchInfo i
            , e
            ]

-- | Read an inventory from a file. Fails with an error message if
-- file is not there or cannot be parsed.
readInventoryPrivate :: FilePath -> IO Inventory
readInventoryPrivate path = do
    inv <- skipPristineHash <$> gzFetchFilePS path Uncachable
    case parseInventory inv of
      Right r -> return r
      Left e -> fail $ unlines [unwords ["parse error in file", path],e]

-- |Copy the hashed inventory from the given location to the given repository,
-- possibly using the given remote darcs binary.
copyHashedInventory :: Repository rt p wR wU wT -> RemoteDarcs -> String -> IO ()
copyHashedInventory outrepo rdarcs inloc | remote <- remoteDarcs rdarcs = do
    let outloc = repoLocation outrepo
    createDirectoryIfMissing False (outloc ++ "/" ++ inventoriesDirPath)
    copyFileOrUrl remote (inloc </> hashedInventoryPath)
                         (outloc </> hashedInventoryPath)
                  Uncachable -- no need to copy anything but hashed_inventory!
    debugMessage "Done copying hashed inventory."

-- |writeAndReadPatch makes a patch lazy, by writing it out to disk (thus
-- forcing it), and then re-reads the patch lazily.
writeAndReadPatch :: RepoPatch p => Cache -> Compression
                  -> PatchInfoAnd rt p wX wY -> IO (PatchInfoAnd rt p wX wY)
writeAndReadPatch c compr p = do
    (i, h) <- writePatchIfNecessary c compr p
    unsafeInterleaveIO $ readp h i
  where
    parse i h = do
        debugDoc $ text "Rereading patch file:" <+> displayPatchInfo i
        (fn, ps) <- fetchFileUsingCache c HashedPatchesDir (getValidHash h)
        case readPatch ps of
            Right x -> return x
            Left e -> fail $ unlines
                [ "Couldn't parse patch file " ++ fn
                , "which is"
                , renderString $ displayPatchInfo i
                , e
                ]

    readp h i = do Sealed x <- createValidHashed h (parse i)
                   return . patchInfoAndPatch i $ unsafeCoerceP x

createValidHashed :: PatchHash
                  -> (PatchHash -> IO (Sealed (a wX)))
                  -> IO (Sealed (Darcs.Patch.PatchInfoAnd.Hopefully a wX))
createValidHashed h f = createHashed (getValidHash h) (f . mkValidHash)

-- | writeTentativeInventory writes @patchSet@ as the tentative inventory.
writeTentativeInventory :: RepoPatch p => Cache -> Compression
                        -> PatchSet rt p Origin wX -> IO ()
writeTentativeInventory cache compr patchSet = do
    debugMessage "in writeTentativeInventory..."
    createDirectoryIfMissing False inventoriesDirPath
    beginTedious tediousName
    hsh <- writeInventoryPrivate $ slightlyOptimizePatchset patchSet
    endTedious tediousName
    debugMessage "still in writeTentativeInventory..."
    case hsh of
        Nothing -> writeBinFile (makeDarcsdirPath tentativeHashedInventory) B.empty
        Just h -> do
            content <- snd <$> fetchFileUsingCache cache HashedInventoriesDir h
            writeAtomicFilePS (makeDarcsdirPath tentativeHashedInventory) content
  where
    tediousName = "Writing inventory"
    writeInventoryPrivate :: RepoPatch p => PatchSet rt p Origin wX
                          -> IO (Maybe String)
    writeInventoryPrivate (PatchSet NilRL NilRL) = return Nothing
    writeInventoryPrivate (PatchSet NilRL ps) = do
        inventory <- sequence $ mapRL (writePatchIfNecessary cache compr) ps
        let inventorylist = showInventoryPatches (reverse inventory)
        hash <- writeHashFile cache compr HashedInventoriesDir inventorylist
        return $ Just hash
    writeInventoryPrivate
        (PatchSet xs@(_ :<: Tagged t _ _) x) = do
        resthash <- write_ts xs
        finishedOneIO tediousName $ fromMaybe "" resthash
        inventory <- sequence $ mapRL (writePatchIfNecessary cache compr)
                                    (NilRL :<: t +<+ x)
        let inventorylist = hcat (map showInventoryEntry $ reverse inventory)
            inventorycontents =
                case resthash of
                    Just h -> text ("Starting with inventory:\n" ++ h) $$
                                  inventorylist
                    Nothing -> inventorylist
        hash <- writeHashFile cache compr HashedInventoriesDir inventorycontents
        return $ Just hash
      where
        -- | write_ts writes out a tagged patchset. If it has already been
        -- written, we'll have the hash, so we can immediately return it.
        write_ts :: RepoPatch p => RL (Tagged rt p) Origin wX
                 -> IO (Maybe String)
        write_ts (_ :<: Tagged _ (Just h) _) = return (Just h)
        write_ts (tts :<: Tagged _ Nothing pps) =
            writeInventoryPrivate $ PatchSet tts pps
        write_ts NilRL = return Nothing

-- |writeHashIfNecessary writes the patch and returns the resulting info/hash,
-- if it has not already been written. If it has been written, we have the hash
-- in the PatchInfoAnd, so we extract and return the info/hash.
writePatchIfNecessary :: RepoPatch p => Cache -> Compression
                      -> PatchInfoAnd rt p wX wY -> IO InventoryEntry
writePatchIfNecessary c compr hp = infohp `seq`
    case extractHash hp of
        Right h -> return (infohp, mkValidHash h)
        Left p -> do
          h <- writeHashFile c compr HashedPatchesDir (showPatch ForStorage p)
          return (infohp, mkValidHash h)
  where
    infohp = info hp

tentativelyAddPatch :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository rt p wR wU wT
                    -> Compression
                    -> Verbosity
                    -> UpdatePending
                    -> PatchInfoAnd rt p wT wY
                    -> IO (Repository rt p wR wU wY)
tentativelyAddPatch = tentativelyAddPatch_ UpdatePristine

data UpdatePristine = UpdatePristine 
                    | DontUpdatePristine
                    | DontUpdatePristineNorRevert deriving Eq

tentativelyAddPatches_ :: (RepoPatch p, ApplyState p ~ Tree)
                       => UpdatePristine
                       -> Repository rt p wR wU wT
                       -> Compression
                       -> Verbosity
                       -> UpdatePending
                       -> FL (PatchInfoAnd rt p) wT wY
                       -> IO (Repository rt p wR wU wY)
tentativelyAddPatches_ upr r c v upe ps =
    foldFL_M (\r' p -> tentativelyAddPatch_ upr r' c v upe p) r ps

tentativelyAddPatch_ :: (RepoPatch p, ApplyState p ~ Tree)
                     => UpdatePristine
                     -> Repository rt p wR wU wT
                     -> Compression
                     -> Verbosity
                     -> UpdatePending
                     -> PatchInfoAnd rt p wT wY
                     -> IO (Repository rt p wR wU wY)
tentativelyAddPatch_ upr r compr verb upe p = do
    let r' = unsafeCoerceT r
    withTentativeRebase r r' (removeFixupsFromSuspended $ hopefully p)
    withRepoLocation r $ do
       addToTentativeInventory (repoCache r) compr p
       when (upr == UpdatePristine) $ do
          debugMessage "Applying to pristine cache..."
          applyToTentativePristine r ApplyNormal verb p
       when (upe == YesUpdatePending) $ do
          debugMessage "Updating pending..."
          tentativelyRemoveFromPending r' (effect p)
       return r'

tentativelyRemovePatches :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                         => Repository rt p wR wU wT
                         -> Compression
                         -> UpdatePending
                         -> FL (PatchInfoAnd rt p) wX wT
                         -> IO (Repository rt p wR wU wX)
tentativelyRemovePatches = tentativelyRemovePatches_ UpdatePristine

newtype Dup p wX = Dup { unDup :: p wX wX }

foldrwFL' :: (forall wA wB. p wA wB -> s wB wB -> s wA wA)
          -> FL p wX wY -> s wY wY -> s wX wX
foldrwFL' f ps = unDup . foldrwFL (\p -> (Dup . f p . unDup)) ps . Dup

tentativelyRemovePatches_ :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                          => UpdatePristine
                          -> Repository rt p wR wU wT
                          -> Compression
                          -> UpdatePending
                          -> FL (PatchInfoAnd rt p) wX wT
                          -> IO (Repository rt p wR wU wX)
tentativelyRemovePatches_ upr r compr upe ps
  | formatHas HashedInventory (repoFormat r) = do
      withRepoLocation r $ do
        unless (upr == DontUpdatePristineNorRevert) $ removeFromUnrevertContext r ps
        Sealed pend <- readTentativePending r
        debugMessage "Removing changes from tentative inventory..."
        r' <- removeFromTentativeInventory r compr ps
        withTentativeRebase r r'
          (foldrwFL' (addFixupsToSuspended . hopefully) ps)
        when (upr == UpdatePristine) $
          applyToTentativePristineCwd ApplyInverted $
            progressFL "Applying inverse to pristine" ps
        when (upe == YesUpdatePending) $ do
          debugMessage "Adding changes to pending..."
          writeTentativePending r' $ effect ps +>+ pend
        return r'
  | otherwise = fail Old.oldRepoFailMsg

-- | Attempt to remove an FL of patches from the tentative inventory.
--
-- Precondition: it must be possible to remove the patches, i.e.
--
-- * the patches are in the repository
--
-- * any necessary commutations will succeed
removeFromTentativeInventory :: forall rt p wR wU wT wX. (IsRepoType rt, RepoPatch p)
                             => Repository rt p wR wU wT
                             -> Compression
                             -> FL (PatchInfoAnd rt p) wX wT
                             -> IO (Repository rt p wR wU wX)
removeFromTentativeInventory repo compr to_remove = do
    debugMessage $ "Start removeFromTentativeInventory"
    allpatches :: PatchSet rt p Origin wT <- readTentativeRepo repo "."
    remaining :: PatchSet rt p Origin wX <-
      case removeFromPatchSet to_remove allpatches of
        Nothing -> error "Hashed.removeFromTentativeInventory: precondition violated"
        Just r -> return r
    writeTentativeInventory (repoCache repo) compr remaining
    debugMessage $ "Done removeFromTentativeInventory"
    return (unsafeCoerceT repo)

-- | Atomically copy the tentative state to the recorded state,
-- thereby committing the tentative changes that were made so far.
-- This includes inventories, pending, and the index.
finalizeRepositoryChanges :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                          => Repository rt p wR wU wT
                          -> UpdatePending
                          -> Compression
                          -> IO (Repository rt p wT wU wT)
finalizeRepositoryChanges r updatePending compr
    | formatHas HashedInventory (repoFormat r) =
        withRepoLocation r $ do
            debugMessage "Finalizing changes..."
            withSignalsBlocked $ do
                renameFile tentativeRebasePath rebasePath
                finalizeTentativeChanges r compr
                recordedState <- readRecorded r
                finalizePending r updatePending recordedState
            let r' = unsafeCoerceR r
            debugMessage "Done finalizing changes..."
            ps <- readRepo r'
            doesPatchIndexExist (repoLocation r') >>= (`when` createOrUpdatePatchIndexDisk r' ps)
            updateIndex r'
            return r'
    | otherwise = fail Old.oldRepoFailMsg

-- TODO: rename this and document the transaction protocol (revert/finalize)
-- clearly.
-- |Slightly confusingly named: as well as throwing away any tentative
-- changes, revertRepositoryChanges also re-initialises the tentative state.
-- It's therefore used before makign any changes to the repo.
revertRepositoryChanges :: RepoPatch p
                        => Repository rt p wR wU wT
                        -> UpdatePending
                        -> IO (Repository rt p wR wU wR)
revertRepositoryChanges r upe
  | formatHas HashedInventory (repoFormat r) =
      withRepoLocation r $ do
        checkIndexIsWritable
          `catchIOError` \e -> fail (unlines ["Cannot write index", show e])
        revertPending r upe
        revertTentativeChanges
        let r' = unsafeCoerceT r
        revertTentativeRebase r'
        return r'
  | otherwise = fail Old.oldRepoFailMsg

revertTentativeRebase :: RepoPatch p => Repository rt p wR wU wR -> IO ()
revertTentativeRebase repo =
  copyFile rebasePath tentativeRebasePath
  `catchIOError` \e ->
    if isDoesNotExistError e then
      createTentativeRebase repo
    else
      fail $ show e

checkIndexIsWritable :: IO ()
checkIndexIsWritable = do
    checkWritable indexInvalidPath
    checkWritable indexPath
  where
    checkWritable path = do
      exists <- doesFileExist path
      touchFile path
      unless exists $ removeFile path
    touchFile path = openBinaryFile path AppendMode >>= hClose

removeFromUnrevertContext :: forall rt p wR wU wT wX
                           . (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                          => Repository rt p wR wU wT
                          -> FL (PatchInfoAnd rt p) wX wT
                          -> IO ()
removeFromUnrevertContext _ NilFL = return () -- nothing to do
removeFromUnrevertContext r ps = do
  Sealed bundle <- unrevert_patch_bundle `catchall` return (seal (Bundle (NilFL :> NilFL)))
  remove_from_unrevert_context_ bundle
  where unrevert_impossible =
            do confirmed <- promptYorn "This operation will make unrevert impossible!\nProceed?"
               if confirmed then removeFileMayNotExist unrevertPath
                            else fail "Cancelled."
        unrevert_patch_bundle :: IO (Sealed (Bundle rt p wB))
        unrevert_patch_bundle = do pf <- B.readFile unrevertPath
                                   case parseBundle pf of
                                     Right foo -> return foo
                                     Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
        remove_from_unrevert_context_ :: Bundle rt p wA wB -> IO ()
        remove_from_unrevert_context_ bundle =
         do debugMessage "Adjusting the context of the unrevert changes..."
            debugMessage $ "Removing "++ show (lengthFL ps) ++
                                  " patches in removeFromUnrevertContext!"
            ref <- readTentativeRepo r (repoLocation r)
            let withSinglet :: Sealed (FL ppp wXxx)
                            -> (forall wYyy . ppp wXxx wYyy -> IO ()) -> IO ()
                withSinglet (Sealed (x :>: NilFL)) j = j x
                withSinglet _ _ = return ()
            Sealed bundle_ps <- bundle_to_patchset ref bundle
            withSinglet (mergeThem ref bundle_ps) $ \h_us ->
                  case commuteRL (reverseFL ps :> h_us) of
                    Nothing -> unrevert_impossible
                    Just (us' :> _) ->
                      case removeFromPatchSet ps ref of
                      Nothing -> unrevert_impossible
                      Just common ->
                          do debugMessage "Have now found the new context..."
                             bundle' <- makeBundle Nothing common (hopefully us':>:NilFL)
                             writeDocBinFile unrevertPath bundle'
            debugMessage "Done adjusting the context of the unrevert changes!"

        bundle_to_patchset :: PatchSet rt p Origin wT
                           -> Bundle rt p wA wB
                           -> IO (SealedPatchSet rt p Origin)
        bundle_to_patchset ref bundle =
          either fail (return . Sealed) $ interpretBundle ref bundle

-- | Writes out a fresh copy of the inventory that minimizes the
-- amount of inventory that need be downloaded when people pull from
-- the repository.
--
-- Specifically, it breaks up the inventory on the most recent tag.
-- This speeds up most commands when run remotely, both because a
-- smaller file needs to be transfered (only the most recent
-- inventory).  It also gives a guarantee that all the patches prior
-- to a given tag are included in that tag, so less commutation and
-- history traversal is needed.  This latter issue can become very
-- important in large repositories.
reorderInventory :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                 => Repository rt p wR wU wR
                 -> Compression
                 -> IO ()
reorderInventory r compr
  | formatHas HashedInventory (repoFormat r) = do
      cleanLatestTag `fmap` readRepo r >>=
        writeTentativeInventory (repoCache r) compr
      withSignalsBlocked $ finalizeTentativeChanges r compr
  | otherwise = fail Old.oldRepoFailMsg

-- | Read inventories and patches from a repository and return them as a
-- 'PatchSet'. Note that patches and inventories are read lazily.
readRepo :: (IsRepoType rt, RepoPatch p)
         => Repository rt p wR wU wT
         -> IO (PatchSet rt p Origin wR)
readRepo r
    | formatHas HashedInventory (repoFormat r) = readRepoHashed r (repoLocation r)
    | otherwise = do Sealed ps <- Old.readOldRepo (repoLocation r)
                     return $ unsafeCoerceP ps

-- | XOR of all hashes of the patches' metadata.
-- It enables to quickly see whether two repositories
-- have the same patches, independently of their order.
-- It relies on the assumption that the same patch cannot
-- be present twice in a repository.
-- This checksum is not cryptographically secure,
-- see http://robotics.stanford.edu/~xb/crypto06b/ .
repoXor :: (IsRepoType rt, RepoPatch p)
        => Repository rt p wR wU wR -> IO SHA1
repoXor repo = do
  hashes <- mapRL (makePatchname . info) . patchSet2RL <$> readRepo repo
  return $ foldl' sha1Xor sha1zero hashes

-- | Upgrade a possible old-style rebase in progress to the new style.
upgradeOldStyleRebase :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                      => Repository rt p wR wU wT -> Compression -> IO ()
upgradeOldStyleRebase repo compr = do
  PatchSet ts _ <- readTentativeRepo repo (repoLocation repo)
  Inventory _ invEntries <- readInventoryPrivate tentativeHashedInventoryPath
  Sealed wps <- readPatchesFromInventory (repoCache repo) invEntries
  case commuteOutOldStyleRebase wps of
    Nothing ->
      ePutDocLn $ text "Rebase is already in new style, no upgrade needed."
    Just (wps' :> wr) -> do
      -- FIXME inlining this action below where it is used
      -- results in lots of ambiguous type variable errors
      -- which is rather strange behavior of ghc IMHO
      let update_repo =
            -- low-level call, must not try to update an existing rebase patch,
            -- nor update anything else beside the inventory
            writeTentativeInventory
              (repoCache repo)
              compr
              (PatchSet ts (mapRL_RL (fmapPIAP W.fromRebasing) wps'))
      -- double check if we really have a rebase patch
      case hopefully wr of
        W.NormalP wtf ->
          error $ renderString $
            "internal error: expected rebase patch but found normal patch:"
            $$ displayPatch wtf
        W.RebaseP _ r -> do
          update_repo
          Items old_r <- readTentativeRebase (unsafeCoerceT repo)
          case old_r of
            NilFL -> do
              writeTentativeRebase (unsafeCoerceT repo) r
              _ <- finalizeRepositoryChanges repo NoUpdatePending compr
              writeRepoFormat
                ( addToFormat RebaseInProgress_2_16
                $ removeFromFormat RebaseInProgress
                $ repoFormat repo)
                formatPath
              return ()
            _ -> do
              ePutDocLn
                $  "A new-style rebase is already in progress, not overwriting it."
                $$ "This should not have happened! This is the old-style rebase I found"
                $$ "and removed from the repository:"
                $$ displayPatch wr
