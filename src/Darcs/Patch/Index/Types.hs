-- Copyright (C) 2009-2010 Benedikt Schmidt
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
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Patch.Index.Types where

import Darcs.Prelude

import Darcs.Util.Hash( SHA1, sha1short, sha1zero )
import Darcs.Util.Path ( anchorPath, AnchoredPath )
import Data.Binary ( Binary(..) )
import Data.Word ( Word32 )

-- | The FileId for a file consists of the FilePath (creation name)
--   and an index. The index denotes how many files
--   with the same name have been added before (and subsequently
--   deleted or moved)
data FileId = FileId {cname::AnchoredPath,count::Int}
  deriving (Eq,Show,Ord)

instance Binary FileId where
  put (FileId rfp i) = put (rfp,i)
  get = do
   (rfp,cnt) <- get
   return $ FileId rfp cnt

-- | Convert FileId to string
showFileId :: FileId -> String
showFileId (FileId fn i) = show i++"#"++anchorPath "." fn

-- | The PatchId identifies a patch and can be created from a PatchInfo with makePatchname
newtype PatchId = PID {patchId :: SHA1}
  deriving (Binary,Show,Ord,Eq)

pid2string :: PatchId -> String
pid2string = show . patchId

-- | This is used to track changes to files
data PatchMod a
  = PTouch a
  | PCreateFile a
  | PCreateDir a
  | PRename a
            a
  | PRemove a
  | PInvalid a
    -- ^ This is an invalid patch
    --   e.g. there is a patch 'Move Autoconf.lhs Autoconf.lhs.in'
    --   where there is no Autoconf.lhs in the darcs repo
  | PDuplicateTouch a
    -- ^ this is used for duplicate patches that don't
    --   have any effect, but we still want to keep
    --   track of them
  deriving (Show, Eq, Functor)

short :: PatchId -> Word32
short (PID sha1) = sha1short sha1

zero :: PatchId
zero = PID sha1zero

