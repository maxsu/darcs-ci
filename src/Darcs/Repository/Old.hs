--  Copyright (C) 2002-2005,2007-2008 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.Repository.Old ( readOldRepo,
                              oldRepoFailMsg ) where

import Darcs.Prelude

import Control.Applicative ( many )
import Darcs.Util.Progress ( debugMessage, beginTedious, endTedious, finishedOneIO )
import Darcs.Util.Path ( ioAbsoluteOrRemote, toPath )
import System.IO ( hPutStrLn, stderr )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.FilePath.Posix ( (</>) )
import Darcs.Patch.PatchInfoAnd ( Hopefully, PatchInfoAnd,
                         patchInfoAndPatch,
                         actually, unavailable )

import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( break, pack, unpack )

import Darcs.Patch ( RepoPatch, Named, readPatch )
import qualified Darcs.Util.Parser as P ( parse )
import Darcs.Patch.Witnesses.Ordered ( RL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, unseal, mapSeal )
import Darcs.Patch.Info ( PatchInfo(..), makePatchname, readPatchInfo, displayPatchInfo )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, Origin )
import Darcs.Util.External
    ( gzFetchFilePS
    , Cachable(..)
    )
import Darcs.Util.Printer ( renderString )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Hash ( sha1PS )
import Darcs.Util.IsoDate ( readUTCDateOldFashioned, showIsoDateTime )

import Control.Exception ( catch, IOException )

readOldRepo :: RepoPatch p => String -> IO (SealedPatchSet rt p Origin)
readOldRepo repo_dir = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote repo_dir
  let task = "Reading inventory of repository "++repo_dir
  beginTedious task
  readRepoPrivate task realdir "inventory" `catch`
                        (\e -> do hPutStrLn stderr ("Invalid repository:  " ++ realdir)
                                  ioError e)

readRepoPrivate :: RepoPatch p
                => String -> FilePath -> FilePath -> IO (SealedPatchSet rt p Origin)
readRepoPrivate task repo_dir inventory_name = do
    inventory <- gzFetchFilePS (repo_dir </> darcsdir </> inventory_name) Uncachable
    finishedOneIO task inventory_name
    let parse inf = parse2 inf $ repo_dir </> darcsdir </> "patches" </> makeFilename inf
    (mt, is) <- readInventory inventory
    Sealed ts <- unseal seal `fmap` unsafeInterleaveIO (read_ts parse mt)
    Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches parse is)
    return $ seal (PatchSet ts ps)
    where read_ts :: RepoPatch p =>
                     (forall wB . PatchInfo -> IO (Sealed (PatchInfoAnd rt p wB)))
                  -> Maybe PatchInfo -> IO (Sealed (RL (Tagged rt p) Origin))
          read_ts _ Nothing = do endTedious task
                                 return $ seal NilRL
          read_ts parse (Just tag0) =
              do debugMessage $ "Looking for inventory for:\n"++ renderString (displayPatchInfo tag0)
                 i <- unsafeInterleaveIO $
                      do x <- gzFetchFilePS (repo_dir </> darcsdir </> "inventories" </> makeFilename tag0) Uncachable
                         finishedOneIO task (renderString (displayPatchInfo tag0))
                         return x
                 (mt, is) <- readInventory i
                 Sealed ts <- fmap (unseal seal) $ unsafeInterleaveIO $ read_ts parse mt
                 Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches parse is)
                 Sealed tag00 <-  parse tag0 `catch`
                                  \(e :: IOException) ->
                                        return $ seal $
                                        patchInfoAndPatch tag0 $ unavailable $ show e
                 return $ seal $ ts :<: Tagged tag00 Nothing ps
          parse2 :: RepoPatch p
                 => PatchInfo -> FilePath
                 -> IO (Sealed (PatchInfoAnd rt p wX))
          parse2 i fn = do ps <- unsafeInterleaveIO $ gzFetchFilePS fn Cachable
                           return $ patchInfoAndPatch i
                             `mapSeal` hopefullyNoParseError (toPath fn) (readPatch ps)
          hopefullyNoParseError :: String -> Either String (Sealed (Named a1dr wX))
                                -> Sealed (Hopefully (Named a1dr) wX)
          hopefullyNoParseError _ (Right (Sealed x)) = seal $ actually x
          hopefullyNoParseError s (Left e) =
              seal $ unavailable $ unlines ["Couldn't parse file " ++ s, e]
          read_patches :: RepoPatch p =>
                          (forall wB . PatchInfo -> IO (Sealed (PatchInfoAnd rt p wB)))
                       -> [PatchInfo] -> IO (Sealed (RL (PatchInfoAnd rt p) wX))
          read_patches _ [] = return $ seal NilRL
          read_patches parse (i:is) =
              lift2Sealed (flip (:<:))
                          (read_patches parse is)
                          (parse i `catch` \(e :: IOException) ->
                           return $ seal $ patchInfoAndPatch i $ unavailable $ show e)
          lift2Sealed :: (forall wY wZ . q wY wZ -> pp wY -> r wZ)
                      -> IO (Sealed pp) -> (forall wB . IO (Sealed (q wB))) -> IO (Sealed r)
          lift2Sealed f iox ioy = do Sealed x <- unseal seal `fmap` unsafeInterleaveIO iox
                                     Sealed y <- unseal seal `fmap` unsafeInterleaveIO ioy
                                     return $ seal $ f y x

oldRepoFailMsg :: String
oldRepoFailMsg = "ERROR: repository upgrade required, try `darcs optimize upgrade`\n"
              ++ "See http://wiki.darcs.net/OF for more details."

-- | This makes darcs-1 (non-hashed repos) filenames.
--
-- The name consists of three segments:
--
--  * timestamp (ISO8601-compatible yyyymmmddHHMMSS;
--    note that the old-fashioned (non-hashed) format expects this date to
--    be exactly as in the patch, /ignoring/ any timezone info,
--    which is why we use 'readUTCDateOldFashioned' here)
--
--  * SHA1 hash of the author
--
--  * SHA1 hash of the patch name, author, date, log, and \"inverted\"
--    flag.
makeFilename :: PatchInfo -> String
makeFilename pi = showIsoDateTime d++"-"++sha1_a++"-"++ (show $ makePatchname pi) ++ ".gz"
    where d = readUTCDateOldFashioned $ BC.unpack $ _piDate pi
          sha1_a = take 5 $ show $ sha1PS $ _piAuthor pi

readPatchInfos :: B.ByteString -> IO [PatchInfo]
readPatchInfos inv =
    case P.parse (many readPatchInfo) inv of
        Right (r, _) -> return r
        Left e -> fail $ unlines ["cannot parse inventory:", e]

readInventory :: B.ByteString -> IO (Maybe PatchInfo, [PatchInfo])
readInventory inv =
    case BC.break ('\n' ==) inv of
        (swt,pistr) | swt == BC.pack "Starting with tag:" -> do
            infos <- readPatchInfos pistr
            case infos of
                (t:ids) -> return (Just t, reverse ids)
                [] -> fail $ unlines ["empty parent inventory:", BC.unpack pistr]
        _ -> do
            infos <- readPatchInfos inv
            return (Nothing, reverse infos)
