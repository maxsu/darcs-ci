{-# LANGUAGE OverloadedStrings #-}

-- | Repository model
module Darcs.Test.Patch.FileUUIDModel
  ( FileUUIDModel
  , Object(..)
  , repoApply
  , emptyFile
  , emptyDir
  , nullRepo
  , root, rootId
  , repoObjects, repoIds
  , aFilename, aDirname
  , aLine, aContent
  , aFile, aDir
  , aRepo
  , anUUID
  ) where


import Prelude ()
import Darcs.Prelude

import Darcs.Test.Util.QuickCheck ( alpha, uniques, bSized )
import Darcs.Test.Patch.RepoModel

import Darcs.Patch.Apply( applyToState )
import Darcs.Patch.Prim.FileUUID.Core( UUID(..), Object(..) )
import Darcs.Patch.Prim.FileUUID.Apply( ObjectMap(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed, seal )
import Darcs.Patch.Witnesses.Show

import Darcs.Util.Path ( Name, makeName )
import Darcs.Util.Hash( Hash(..) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen, choose, vectorOf, frequency, oneof )
-- import Text.Show.Pretty ( ppShow )

----------------------------------------------------------------------
-- * Model definition

newtype FileUUIDModel wX = FileUUIDModel { _repoMap :: ObjectMap Fail }

----------------------------------------
-- Instances

instance Show (Object Fail) where
  show (Directory l) = show l
  show (Blob c _) = show c

deriving instance Eq (Object Fail)

instance Show (FileUUIDModel wX) where
  show repo = "FileUUIDModel " ++ show (repoObjects repo)

instance Show1 FileUUIDModel

----------------------------------------------------------------------
-- * Constructors

objectMap :: (Monad m) => M.Map UUID (Object m) -> ObjectMap m
objectMap m = ObjectMap { getObject = get, putObject = put, listObjects = list }
  where list = return $ M.keys m
        put k o = return $ objectMap (M.insert k o m)
        get k = return $ M.lookup k m

{-
emptyRepo :: FileUUIDModel wX
emptyRepo = FileUUIDModel (objectMap $ M.singleton rootId emptyDir)
-}

emptyFile :: (Monad m) => Object m
emptyFile = Blob (return B.empty) NoHash

emptyDir :: Object m
emptyDir = Directory M.empty

----------------------------------------------------------------------
-- * Queries

nullRepo :: FileUUIDModel wX -> Bool
nullRepo repo = repoIds repo == [rootId]

rootId :: UUID
rootId = UUID "ROOT"

-- | The root directory of a repository.
root :: FileUUIDModel wX -> (UUID, Object Fail)
root (FileUUIDModel repo) = (rootId, fromJust $ unFail $ getObject repo rootId)

repoObjects :: FileUUIDModel wX -> [(UUID, Object Fail)]
repoObjects (FileUUIDModel repo) =
    [(uuid, obj uuid) | uuid <- unFail $ listObjects repo]
  where
    obj uuid = fromJust $ unFail $ getObject repo uuid

repoIds :: FileUUIDModel wX -> [UUID]
repoIds = map fst . repoObjects

-- | @isEmpty file@ <=> file content is empty
--   @isEmpty dir@  <=> dir has no child
isEmpty :: Object Fail -> Bool
isEmpty (Directory d) = M.null d
isEmpty (Blob f _) = B.null $ unFail f

nonEmptyRepoObjects :: FileUUIDModel wX -> [(UUID, Object Fail)]
nonEmptyRepoObjects = filter (not . isEmpty . snd) . repoObjects

----------------------------------------------------------------------
-- * Comparing repositories

----------------------------------------------------------------------
-- * QuickCheck generators

-- Testing code assumes that aFilename and aDirname generators 
-- will always be able to generate a unique name given a list of
-- existing names. This should be OK as long as the number of possible
-- file/dirnames is much bigger than the number of files/dirs per repository.

-- 'Arbitrary' 'FileUUIDModel' instance is based on the 'aSmallRepo' generator.


-- | Files are distinguish by ending their names with ".txt".
aFilename :: Gen Name
aFilename = do
  len <- choose (1,3)
  name <- vectorOf len alpha
  return $ either error id . makeName $ name ++ ".txt"

aDirname :: Gen Name
aDirname = do
  len <- choose (1,3)
  name <- vectorOf len alpha
  return $ either error id . makeName $ name

aWord :: Gen B.ByteString
aWord = do c <- alpha
           return $ BC.pack[c]

aLine :: Gen B.ByteString
aLine = do wordsNo <- choose (1,2)
           ws <- vectorOf wordsNo aWord
           return $ BC.unwords ws

aContent :: Gen B.ByteString
aContent = bSized 0 0.5 80 $ \k ->
             do n <- choose (0,k)
                BC.intercalate "\n" <$> vectorOf n aLine

aFile :: (Monad m) => Gen (Object m)
aFile = aContent >>= \c -> return $ Blob (return c) NoHash

aDir :: (Monad m) => [UUID] -> [UUID] -> Gen [(UUID, Object m)]
aDir [] _ = return []
aDir (dirid:dirids) fileids =
  do dirsplit <- choose (1, length dirids)
     filesplit <- choose (1, length fileids)
     let ids = take filesplit fileids
     files <- vectorOf filesplit aFile
     names <- vectorOf filesplit aFilename
     dirnames <- vectorOf dirsplit aDirname
     dirs <- subdirs (take dirsplit dirids)
                     (drop dirsplit dirids)
                     (drop filesplit fileids)
     return $ (dirid, Directory $ M.fromList $ names `zip` ids ++ dirnames `zip` dirids)
            : (fileids `zip` files) ++ dirs
  where subdirs [] _ _ = return []
        subdirs tomake dirs files = do
          dirsplit <- choose (1, length dirs)
          filesplit <- choose (1, length files)
          dir <- aDir (head tomake : take dirsplit dirs) (take filesplit files)
          remaining <- subdirs (tail tomake) (drop dirsplit dirs) (drop filesplit files)
          return $ dir ++ remaining


anUUID :: Gen UUID
anUUID = UUID . BC.pack <$> vectorOf 4 (oneof $ map return "0123456789")

-- | @aRepo filesNo dirsNo@ produces repositories with *at most* 
-- @filesNo@ files and @dirsNo@ directories. 
-- The structure of the repository is aleatory.
aRepo :: Int                    -- ^ Maximum number of files
      -> Int                    -- ^ Maximum number of directories
      -> Gen (FileUUIDModel wX)
aRepo maxFiles maxDirs = do
  ids <- uniques (maxFiles+maxDirs) anUUID
  let minFiles = if maxDirs == 0 && maxFiles > 0 then 1 else 0
  filesNo <- choose (minFiles,maxFiles)
  let minDirs = if filesNo == 0 && maxDirs > 0 then 1 else 0
  dirsNo <- choose (minDirs,maxDirs)
  let (dirids, ids') = splitAt dirsNo ids
      fileids = take filesNo ids'
  objectmap <- aDir (rootId : dirids) fileids
  return $ FileUUIDModel $ objectMap $ M.fromList objectmap

-- | Generate small repositories.
-- Small repositories help generating (potentially) conflicting patches.
instance RepoModel FileUUIDModel where
  type RepoState FileUUIDModel = ObjectMap
  aSmallRepo = do filesNo <- frequency [(3, return 1), (1, return 2)]
                  dirsNo <- frequency [(3, return 1), (1, return 0)]
                  aRepo filesNo dirsNo
  repoApply (FileUUIDModel state) patch = FileUUIDModel <$> applyToState patch state
  showModel = show -- ppShow
  eqModel r1 r2 = nonEmptyRepoObjects r1 == nonEmptyRepoObjects r2

instance Arbitrary (Sealed FileUUIDModel) where
  arbitrary = seal <$> aSmallRepo
