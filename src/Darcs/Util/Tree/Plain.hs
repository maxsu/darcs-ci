--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3
-- | The plain format implementation resides in this module. The plain format
-- does not use any hashing and basically just wraps a normal filesystem tree
-- in the hashed-storage API.
--
-- NB. The 'read' function on Blobs coming from a plain tree is susceptible to
-- file content changes. Since we use mmap in 'read', this will break
-- referential transparency and produce unexpected results. Please always make
-- sure that all parallel access to the underlying filesystem tree never
-- mutates files. Unlink + recreate is fine though (in other words, the
-- 'writePlainTree' implemented in this module is safe in this respect).
module Darcs.Util.Tree.Plain
    ( -- * Obtaining Trees.
    --
    -- | Please note that Trees obtained this way will contain Stub
    -- items. These need to be executed (they are IO actions) in order to be
    -- accessed. Use 'expand' to do this. However, many operations are
    -- perfectly fine to be used on a stubbed Tree (and it is often more
    -- efficient to do everything that can be done before expanding a Tree).
      readPlainTree

    -- * Writing trees.
    , writePlainTree
    ) where

import Control.Monad ( forM )
import Data.Maybe( catMaybes )
import qualified Data.ByteString.Lazy as BL
import System.FilePath( (</>) )
import System.Directory ( listDirectory, createDirectoryIfMissing )
import System.Posix.Files
    ( getSymbolicLinkStatus, isDirectory, isRegularFile, FileStatus )

import Darcs.Prelude

import Darcs.Util.Path
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.ByteString ( readSegment )
import Darcs.Util.Hash( Hash( NoHash) )
import Darcs.Util.Tree( Tree(), TreeItem(..)
                          , Blob(..), makeTree
                          , list, readBlob, expand )

readPlainDir :: FilePath -> IO [(FilePath, FileStatus)]
readPlainDir dir =
  withCurrentDirectory dir $ do
    items <- listDirectory "."
    forM items $ \s -> do
      st <- getSymbolicLinkStatus s
      return (s, st)

readPlainTree :: FilePath -> IO (Tree IO)
readPlainTree dir = do
  items <- readPlainDir dir
  let subs = catMaybes [
       let name = either error id $ makeName name'
        in case status of
             _ | isDirectory status -> Just (name, Stub (readPlainTree (dir </> name')) NoHash)
             _ | isRegularFile status -> Just (name, File $ Blob (readBlob' name') NoHash)
             _ -> Nothing
            | (name', status) <- items ]
  return $ makeTree subs
    where readBlob' name = readSegment (dir </> name, Nothing)

-- | Write out /full/ tree to a plain directory structure. If you instead want
-- to make incremental updates, refer to "Darcs.Util.Tree.Monad".
writePlainTree :: Tree IO -> FilePath -> IO ()
writePlainTree t dir = do
  createDirectoryIfMissing True dir
  expand t >>= mapM_ write . list
    where write (p, File b) = write' p b
          write (p, SubTree _) =
              createDirectoryIfMissing True (anchorPath dir p)
          write _ = return ()
          write' p b = readBlob b >>= BL.writeFile (anchorPath dir p)

