-- Copyright (C) 2011 Petr Rockai
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

module Darcs.Patch.Prim.FileUUID.ObjectMap
    ( UUID(..), Location(..), Object(..)
    , ObjectMap(..), DirContent, FileContent
    , isBlob, isDirectory
    , Name -- re-export
    ) where

import Darcs.Prelude

import Darcs.Util.Hash ( Hash )
import Darcs.Util.Path ( Name )
import qualified Data.ByteString as B (ByteString)
import qualified Data.Map as M

type FileContent = B.ByteString

newtype UUID = UUID B.ByteString deriving (Eq, Ord, Show)

-- | An object is located by giving the 'UUID' of the parent
-- 'Directory' and a 'Name'.
data Location = L !UUID !Name
  deriving (Eq, Show)

-- TODO use HashMap instead?
type DirContent = M.Map Name UUID

data Object (m :: * -> *)
  = Directory DirContent
  | Blob (m FileContent) !Hash

isBlob :: Object m -> Bool
isBlob Blob{} = True
isBlob Directory{} = False

isDirectory :: Object m -> Bool
isDirectory Directory{} = True
isDirectory Blob{} = False

data ObjectMap (m :: * -> *) = ObjectMap
  { getObject :: UUID -> m (Maybe (Object m))
  , putObject :: UUID -> Object m -> m (ObjectMap m)
  , listObjects :: m [UUID]
  }
