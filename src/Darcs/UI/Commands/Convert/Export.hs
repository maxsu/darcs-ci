--  Copyright (C) 2002-2014 David Roundy, Petr Rockai, Owen Stephens
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

{-# LANGUAGE OverloadedStrings #-}

module Darcs.UI.Commands.Convert.Export ( convertExport ) where

import Darcs.Prelude hiding ( readFile, lex )

import Control.Exception (finally)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.State.Strict (gets)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (isSpace)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (catMaybes, fromJust)
import System.Time (toClockTime)

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch ( RepoPatch, apply, effect, listTouchedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Effect ( Effect )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , nullFL
    )
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal(..)
    , flipSeal
    , unsealFlipped
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Darcs.Patch.Info
    ( PatchInfo
    , isTag
    , piAuthor
    , piDate
    , piLog
    , piName
    )
import Darcs.Patch.RepoType ( IsRepoType(..) )
import Darcs.Patch.Set ( patchSet2FL, inOrderTags )

import Darcs.Repository
    ( RepoJob(..)
    , Repository
    , readRepo
    , repoCache
    , withRepository
    )
import Darcs.Repository.Cache (HashedDir(HashedPristineDir))
import Darcs.Repository.Pristine (readHashedPristineRoot)
import Darcs.Repository.HashedIO (cleanHashdir)
import Darcs.Repository.Paths (pristineDirPath)

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInRepository
    , nodefaults
    , withStdOpts
    )
import Darcs.UI.Commands.Convert.Util
    ( Marks
    , addMark
    , emptyMarks
    , getMark
    , lastMark
    , readMarks
    , writeMarks
    , patchHash
    )
import Darcs.UI.Completion (noArgs)
import Darcs.UI.Flags ( DarcsFlag , useCache )
import Darcs.UI.Options
    ( (?)
    , (^)
    , defaultFlags
    , ocheck
    , odesc
    , parseFlags
    )
import qualified Darcs.UI.Options.All as O

import Darcs.Util.DateTime ( formatDateTime, fromClockTime )
import Darcs.Util.Path
    ( AbsolutePath
    , AnchoredPath(..)
    , anchorPath
    , appendPath
    )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree
    ( Tree
    , emptyTree
    , findTree
    , listImmediate
    )
import Darcs.Util.Tree.Hashed ( hashedTreeIO )

import Darcs.Util.Tree.Monad ( TreeIO )
import qualified Darcs.Util.Tree.Monad as T
    ( directoryExists
    , fileExists
    , readFile
    , tree
    )


convertExportHelp :: Doc
convertExportHelp = text $ unlines
 [ "This command enables you to export darcs repositories into git."
 , ""
 , "For a one-time export you can use the recipe:"
 , ""
 , "    $ cd repo"
 , "    $ git init ../mirror"
 , "    $ darcs convert export | (cd ../mirror && git fast-import)"
 , ""
 , "For incremental export using marksfiles:"
 , ""
 , "    $ cd repo"
 , "    $ git init ../mirror"
 , "    $ touch ../mirror/git.marks"
 , "    $ darcs convert export --read-marks darcs.marks --write-marks darcs.marks"
 , "       | (cd ../mirror && git fast-import --import-marks=git.marks --export-marks=git.marks)"
 , ""
 , "In the case of incremental export, be careful to never amend, delete or"
 , "reorder patches in the source darcs repository."
 , ""
 , "Also, be aware that exporting a darcs repo to git will not be exactly"
 , "faithful in terms of history if the darcs repository contains conflicts."
 , ""
 , "Limitations:"
 , ""
 , "  * Empty directories are not supported by the fast-export protocol."
 , "  * Unicode filenames are currently not correctly handled."
 , "    See http://bugs.darcs.net/issue2359 ."
 ]

convertExport :: DarcsCommand
convertExport = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "export"
    , commandHelp = convertExportHelp
    , commandDescription = "Export a darcs repository to a git-fast-import stream"
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = fastExport
    , commandPrereq = amInRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc convertExportAdvancedOpts
    , commandBasicOptions = odesc convertExportBasicOpts
    , commandDefaults = defaultFlags convertExportOpts
    , commandCheckOptions = ocheck convertExportOpts
    }
  where
    convertExportBasicOpts = O.repoDir ^ O.marks
    convertExportAdvancedOpts = O.network
    convertExportOpts = convertExportBasicOpts `withStdOpts` convertExportAdvancedOpts

fastExport :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
fastExport _ opts _ = do
  marks <- case parseFlags O.readMarks opts of
    Nothing -> return emptyMarks
    Just f  -> readMarks f
  newMarks <-
    withRepository (useCache ? opts) $ RepoJob $ \repo -> fastExport' repo marks
  case parseFlags O.writeMarks opts of
    Nothing -> return ()
    Just f  -> writeMarks f newMarks

fastExport' :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
            => Repository rt p r u r -> Marks -> IO Marks
fastExport' repo marks = do
  putStrLn "progress (reading repository)"
  patchset <- readRepo repo
  marksref <- newIORef marks
  let patches = patchSet2FL patchset
      tags = inOrderTags patchset
      mark :: (PatchInfoAnd rt p) x y -> Int -> TreeIO ()
      mark p n = liftIO $ do putStrLn $ "mark :" ++ show n
                             modifyIORef marksref $ \m -> addMark m n (patchHash p)
      -- apply a single patch to build the working tree of the last exported version
      checkOne :: (RepoPatch p, ApplyState p ~ Tree)
               => Int -> (PatchInfoAnd rt p) x y -> TreeIO ()
      checkOne n p = do apply p
                        unless (inOrderTag tags p ||
                                (getMark marks n == Just (patchHash p))) $
                          fail $ "FATAL: Marks do not correspond: expected " ++
                                 show (getMark marks n) ++ ", got " ++ BC.unpack (patchHash p)
      -- build the working tree of the last version exported by convert --export
      check :: (RepoPatch p, ApplyState p ~ Tree)
            => Int -> FL (PatchInfoAnd rt p) x y -> TreeIO (Int,  FlippedSeal( FL (PatchInfoAnd rt p)) y) 
      check _ NilFL = return (1, flipSeal NilFL)
      check n allps@(p:>:ps)
        | n <= lastMark marks = checkOne n p >> check (next tags n p) ps
        | n > lastMark marks = return (n, flipSeal allps)
        | lastMark marks == 0 = return (1, flipSeal allps)
        | otherwise = undefined
  ((n, patches'), tree') <- hashedTreeIO (check 1 patches) emptyTree pristineDirPath
  let patches'' = unsealFlipped unsafeCoerceP patches'
  void $ hashedTreeIO (dumpPatches tags mark n patches'') tree' pristineDirPath
  readIORef marksref
 `finally` do
  putStrLn "progress (cleaning up)"
  current <- readHashedPristineRoot repo
  cleanHashdir (repoCache repo) HashedPristineDir $ catMaybes [current]
  putStrLn "progress done"

dumpPatches ::  (RepoPatch p, ApplyState p ~ Tree)
            =>  [PatchInfo]
            -> (forall p0 x0 y0 . (PatchInfoAnd rt p0) x0 y0 -> Int -> TreeIO ())
            -> Int -> FL (PatchInfoAnd rt p) x y -> TreeIO ()
dumpPatches _ _ _ NilFL = liftIO $ putStrLn "progress (patches converted)"
dumpPatches tags mark n (p:>:ps) = do
  apply p
  if inOrderTag tags p && n > 0
     then dumpTag p n
     else do dumpPatch mark p n
             dumpFiles $ listTouchedFiles p
  dumpPatches tags mark (next tags n p) ps

dumpTag :: (PatchInfoAnd rt p) x y  -> Int -> TreeIO () 
dumpTag p n =
  dumpBits [ BLU.fromString $ "progress TAG " ++ cleanTagName p
           , BLU.fromString $ "tag " ++ cleanTagName p -- FIXME is this valid?
           , BLU.fromString $ "from :" ++ show (n - 1)
           , BLU.fromString $ unwords ["tagger", patchAuthor p, patchDate p]
           -- -3 == (-4 for "TAG " and +1 for newline)
           , BLU.fromString $ "data "
                 ++ show (BL.length (patchMessage p) - 3)
           , BL.drop 4 $ patchMessage p ]
   where
     -- FIXME forbidden characters and subsequences in tags:
     -- https://www.kernel.org/pub/software/scm/git/docs/git-check-ref-format.html
     cleanTagName = map cleanup . drop 4 . piName . info
         where cleanup x | x `elem` bad = '_'
                         | otherwise = x
               bad :: String
               bad = " ~^:"

dumpFiles :: [AnchoredPath] -> TreeIO ()
dumpFiles files = forM_ files $ \file -> do
  let quotedPath = quotePath $ anchorPath "" file
  isfile <- T.fileExists file
  isdir <- T.directoryExists file
  when isfile $ do bits <- T.readFile file
                   dumpBits [ BLU.fromString $ "M 100644 inline " ++ quotedPath
                            , BLU.fromString $ "data " ++ show (BL.length bits)
                            , bits ]
  when isdir $ do -- Always delete directory before dumping its contents. This fixes
                  -- a corner case when a same patch moves dir1 to dir2, and creates
                  -- another directory dir1.
                  -- As we always dump its contents anyway this is not more costly.
                  liftIO $ putStrLn $ "D " ++ quotedPath
                  tt <- gets T.tree -- ick
                  let subs = [ file `appendPath` n | (n, _) <-
                                  listImmediate $ fromJust $ findTree tt file ]
                  dumpFiles subs
  when (not isfile && not isdir) $ liftIO $ putStrLn $ "D " ++ quotedPath
  where
    -- |quotePath escapes and quotes paths containing newlines, double-quotes
    -- or backslashes.
    quotePath :: FilePath -> String
    quotePath path = case foldr escapeChars ("", False) path of
        (_, False) -> path
        (path', True) -> quote path'

    quote str = "\"" ++ str ++ "\""

    escapeChars c (processed, haveEscaped) = case escapeChar c of
        (escaped, didEscape) ->
            (escaped ++ processed, didEscape || haveEscaped)

    escapeChar c = case c of
        '\n' -> ("\\n", True)
        '\r' -> ("\\r", True)
        '"'  -> ("\\\"", True)
        '\\' -> ("\\\\", True)
        _    -> ([c], False)


dumpPatch ::  (forall p0 x0 y0 . (PatchInfoAnd rt p0) x0 y0 -> Int -> TreeIO ())
          -> (PatchInfoAnd rt p) x y -> Int
          -> TreeIO ()
dumpPatch mark p n =
  do dumpBits [ BLU.fromString $ "progress " ++ show n ++ ": " ++ piName (info p)
              , "commit refs/heads/master" ]
     mark p n
     dumpBits [ BLU.fromString $ "committer " ++ patchAuthor p ++ " " ++ patchDate p
              , BLU.fromString $ "data " ++ show (BL.length $ patchMessage p)
              , patchMessage p ]
     when (n > 1) $ dumpBits [ BLU.fromString $ "from :" ++ show (n - 1) ]

dumpBits :: [BL.ByteString] -> TreeIO ()
dumpBits = liftIO . BLC.putStrLn . BL.intercalate "\n"

-- patchAuthor attempts to fixup malformed author strings
-- into format: "Name <Email>"
-- e.g.
-- <john@home>      -> john <john@home>
-- john@home        -> john <john@home>
-- john <john@home> -> john <john@home>
-- john <john@home  -> john <john@home>
-- <john>           -> john <unknown>
patchAuthor :: (PatchInfoAnd rt p) x y -> String
patchAuthor p
 | null author = unknownEmail "unknown"
 | otherwise = case span (/='<') author of
               -- No name, but have email (nothing spanned)
               ("", email) -> case span (/='@') (tail email) of
                   -- Not a real email address (no @).
                   (n, "") -> case span (/='>') n of
                       (name, _) -> unknownEmail name
                   -- A "real" email address.
                   (user, rest) -> case span (/= '>') (tail rest) of
                       (dom, _) -> mkAuthor user $ emailPad (user ++ "@" ++ dom)
               -- No email (everything spanned)
               (_, "") -> case span (/='@') author of
                   (n, "") -> unknownEmail n
                   (name, _) -> mkAuthor name $ emailPad author
               -- Name and email
               (n, rest) -> case span (/='>') $ tail rest of
                   (email, _) -> n ++ emailPad email
 where
   author = dropWhile isSpace $ piAuthor (info p)
   unknownEmail = flip mkAuthor "<unknown>"
   emailPad email = "<" ++ email ++ ">"
   mkAuthor name email = name ++ " " ++ email

patchDate :: (PatchInfoAnd rt p) x y -> String
patchDate = formatDateTime "%s +0000" . fromClockTime . toClockTime .
  piDate . info

patchMessage :: (PatchInfoAnd rt p) x y -> BLU.ByteString
patchMessage p = BL.concat [ BLU.fromString (piName $ info p)
                           , case unlines . piLog $ info p of
                                 "" -> BL.empty
                                 plog -> BLU.fromString ("\n\n" ++ plog)
                           ]

inOrderTag :: (Effect p) => [PatchInfo] -> PatchInfoAnd rt p wX wZ -> Bool
inOrderTag tags p = isTag (info p) && info p `elem` tags && nullFL (effect p)

next :: (Effect p) => [PatchInfo] -> Int ->  PatchInfoAnd rt p x y -> Int
next tags n p = if inOrderTag tags p then n else n + 1

