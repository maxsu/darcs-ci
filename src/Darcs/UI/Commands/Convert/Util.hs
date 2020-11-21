module Darcs.UI.Commands.Convert.Util
    ( Marks
    , emptyMarks
    , addMark
    , getMark
    , lastMark
    , readMarks
    , writeMarks
    -- misc
    , patchHash
    , updatePending
    ) where

import Darcs.Prelude

import Darcs.Util.Exception ( catchall )

import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap as M

import System.Directory ( removeFile )

import Darcs.Patch.Info ( makePatchname )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )

import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.UI.Options ( (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Flags ( DarcsFlag )

-- marks support

type Marks = M.IntMap BC.ByteString

emptyMarks :: Marks
emptyMarks = M.empty

lastMark :: Marks -> Int
lastMark m = if M.null m then 0 else fst $ M.findMax m

getMark :: Marks -> Int -> Maybe BC.ByteString
getMark marks key = M.lookup key marks

addMark :: Marks -> Int -> BC.ByteString -> Marks
addMark marks key value = M.insert key value marks

readMarks :: FilePath -> IO Marks
readMarks p = do lines' <- BC.split '\n' `fmap` BC.readFile p
                 return $ foldl merge M.empty lines'
               `catchall` return emptyMarks
  where merge set line = case BC.split ':' line of
          [i, hash] -> M.insert (read $ BC.unpack i) (BC.dropWhile (== ' ') hash) set
          _ -> set -- ignore, although it is maybe not such a great idea...

writeMarks :: FilePath -> Marks -> IO ()
writeMarks fp m = do removeFile fp `catchall` return () -- unlink
                     BC.writeFile fp marks
  where marks = BC.concat $ map format $ M.assocs m
        format (k, s) = BC.concat [BC.pack $ show k, BC.pack ": ", s, BC.pack "\n"]

-- misc shared functions

patchHash :: PatchInfoAnd rt p cX cY -> BC.ByteString
patchHash p = BC.pack $ show $ makePatchname (info p)

updatePending :: [DarcsFlag] -> UpdatePending
updatePending opts =
  case O.withWorkingDir ? opts of
    O.WithWorkingDir -> YesUpdatePending
    O.NoWorkingDir -> NoUpdatePending
