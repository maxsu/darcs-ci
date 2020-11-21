--  Copyright (C) 2003-2004 David Roundy
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

module Darcs.UI.Commands.Diff ( diffCommand ) where

import Darcs.Prelude hiding ( all )

import Control.Monad ( unless, when )
import Data.Maybe ( fromMaybe )
import Data.Maybe ( isJust )
import System.Directory ( copyFile, createDirectory, findExecutable, listDirectory )
import System.FilePath.Posix ( takeFileName, (</>) )

import Darcs.Patch ( listTouchedFiles )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Patch.Info ( displayPatchInfo )
import Darcs.Patch.Match ( matchFirstPatchset, matchSecondPatchset, secondMatch )
import Darcs.Patch.Named ( anonymous )
import Darcs.Patch.PatchInfoAnd ( info, n2pia )
import Darcs.Patch.Set ( patchSetSnoc )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal )
import Darcs.Repository ( RepoJob(..), readRepo, withRepository )
import Darcs.Repository.Flags ( DiffAlgorithm(MyersDiff), WantGuiPause(..) )
import Darcs.Repository.Paths ( pristineDirPath )
import Darcs.Repository.State
    ( ScanKnown(..)
    , applyTreeFilter
    , readRecorded
    , restrictSubpaths
    , unrecordedChanges
    )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , withStdOpts
    )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.External ( diffProgram )
import Darcs.UI.Flags ( DarcsFlag, pathSetFromArgs, useCache, wantGuiPause )
import Darcs.UI.Options ( defaultFlags, ocheck, odesc, parseFlags, (?), (^) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.CommandLine ( parseCmd )
import Darcs.Util.Exec ( execInteractive )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Lock ( withTempDir )
import Darcs.Util.Path ( AbsolutePath, AnchoredPath, isPrefix, toFilePath )
import Darcs.Util.Printer ( Doc, putDoc, text, vcat )
import Darcs.Util.Prompt ( askEnter )
import Darcs.Util.Tree.Hashed ( hashedTreeIO )
import Darcs.Util.Tree.Plain ( writePlainTree )
import Darcs.Util.Workaround ( getCurrentDirectory )

diffDescription :: String
diffDescription = "Create a diff between two versions of the repository."

diffHelp :: Doc
diffHelp = text $
 "The `darcs diff` command compares two versions of the working tree of\n" ++
 "the current repository.  Without options, the pristine (recorded) and\n" ++
 "unrecorded working trees are compared.  This is lower-level than\n" ++
 "the `darcs whatsnew` command, since it outputs a line-by-line diff,\n" ++
 "and it is also slower.  As with `darcs whatsnew`, if you specify\n" ++
 "files or directories, changes to other files are not listed.\n" ++
 "The command always uses an external diff utility.\n" ++
 "\n" ++
 "With the `--patch` option, the comparison will be made between working\n" ++
 "trees with and without that patch.  Patches *after* the selected patch\n" ++
 "are not present in either of the compared working trees.  The\n" ++
 "`--from-patch` and `--to-patch` options allow the set of patches in the\n" ++
 "`old' and `new' working trees to be specified separately.\n" ++
 "\n" ++
 "The associated tag and match options are also understood, e.g. `darcs\n" ++
 "diff --from-tag 1.0 --to-tag 1.1`.  All these options assume an\n" ++
 "ordering of the patch set, so results may be affected by operations\n" ++
 "such as `darcs optimize reorder`.\n" ++
 "\n" ++
 "diff(1) is always called with the arguments `-rN` and by default also\n" ++
 "with `-u` to show the differences in unified format. This can be turned\n" ++
 "off by passing `--no-unified`. An additional argument can be passed\n" ++
 "using `--diff-opts`, such as `--diff-opts=-ud` or `--diff-opts=-wU9`.\n" ++
 "\n" ++
 "The `--diff-command` option can be used to specify an alternative\n" ++
 "utility. Arguments may be included, separated by whitespace.  The value\n" ++
 "is not interpreted by a shell, so shell constructs cannot be used.  The\n" ++
 "arguments %1 and %2 MUST be included, these are substituted for the two\n" ++
 "working trees being compared. For instance:\n" ++
 "\n" ++
 "    darcs diff -p . --diff-command \"meld %1 %2\"\n" ++
 "\n" ++
 "If this option is used, `--diff-opts` is ignored.\n"

diffCommand :: DarcsCommand
diffCommand = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "diff"
    , commandHelp = diffHelp
    , commandDescription = diffDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = diffCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc diffAdvancedOpts
    , commandBasicOptions = odesc diffBasicOpts
    , commandDefaults = defaultFlags diffOpts
    , commandCheckOptions = ocheck diffOpts
    }
  where
    diffBasicOpts
      = O.matchOneOrRange
      ^ O.extDiff
      ^ O.repoDir
      ^ O.storeInMemory
    diffAdvancedOpts = O.pauseForGui ^ O.useIndex
    diffOpts = diffBasicOpts `withStdOpts` diffAdvancedOpts

diffCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
diffCmd fps opts args
  | not (null (O.matchLast ? opts)) &&
      not (null (O.matchFrom ? opts)) =
        fail $ "using --patch and --last at the same time with the 'diff'" ++
          " command doesn't make sense. Use --from-patch to create a diff" ++
          " from this patch to the present, or use just '--patch' to view" ++
          " this specific patch."
  | otherwise = doDiff opts =<< pathSetFromArgs fps args

doDiff :: [DarcsFlag] -> Maybe [AnchoredPath] ->  IO ()
doDiff opts mpaths = withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  patchset <- readRepo repository
  -- We pass @mpaths@ here which means we get only the changes that affect the
  -- given paths (if any).
  unrecorded <- unrecordedChanges (O.useIndex ? opts, ScanKnown, MyersDiff)
    O.NoLookForMoves O.NoLookForReplaces
    repository mpaths
  -- Use of 'anonymous' is unproblematic here as we don't store any patches.
  -- But we must take care not to show its fake patch info to the user.
  unrecorded' <- n2pia `fmap` anonymous unrecorded

  let matchFlags = parseFlags O.matchOneOrRange opts

  -- If no secondMatch (--to-xxx) is specified, include unrecorded changes
  Sealed all <- return $
    if secondMatch matchFlags
      then seal patchset
      else seal $ patchSetSnoc patchset unrecorded'
  -- Note how this differs from how firstMatch defaults for log command
  Sealed ctx <- return $
    fromMaybe (seal patchset) $ matchFirstPatchset matchFlags patchset
  Sealed match <- return $
    fromMaybe (seal all) $ matchSecondPatchset matchFlags patchset

  (_ :> todiff) <- return $ findCommonWithThem match ctx
  (_ :> tounapply) <- return $ findCommonWithThem all match

  Sealed logmatch <- return $
    if secondMatch matchFlags
      then seal match
      else seal patchset
  -- Same as @todiff@ but without trailing @unrecorded'@ changes
  (_ :> tolog) <- return $ findCommonWithThem logmatch ctx

  let touched = listTouchedFiles todiff
      files = case mpaths of
               Nothing -> touched
               Just paths ->
                  concatMap (\path -> filter (isPrefix path) touched) paths
  relevant <- restrictSubpaths repository files

  formerdir <- getCurrentDirectory
  let thename = takeFileName formerdir
  withTempDir "darcs-diff" $ \tmpdir -> do
      getCurrentDirectory >>= debugMessage . ("doDiff: I am now in "++)
      let pdir = toFilePath tmpdir </> ("pristine.hashed-"++thename)
      createDirectory pdir
      let odir = toFilePath tmpdir </> ("old-"++thename)
      createDirectory odir
      let ndir = toFilePath tmpdir </> ("new-"++thename)
      createDirectory ndir

      -- Prepare the (plain) trees we want to compare. Since we need to access
      -- our repository, we have to restore the working directory.
      withCurrentDirectory formerdir $ do
        -- Make a temporary copy of pristineDirPath where we have write access.
        -- The result (@pdirpath@) serves as our storage for hashed 'Tree' items
        -- during the 'apply' and 'unapply' operations below.
        let pdirpath = toFilePath pdir
        pfiles <- listDirectory pristineDirPath
        let copy srcdir destdir name = copyFile (srcdir</>name) (destdir</>name)
        mapM_ (copy pristineDirPath pdirpath) pfiles

        pristine <- readRecorded repository

        -- @base@ will be like our working tree, /except/ that it contains only
        -- the unrecorded changes that affect the given file paths, see comment
        -- above when we called 'unrecordedChanges'.
        base <- if secondMatch matchFlags
                 then return pristine
                 else snd <$> hashedTreeIO (apply unrecorded') pristine pdirpath

        newtree <- snd <$> hashedTreeIO (unapply tounapply) base pdirpath
        -- @todiff@ may have our @unrecorded'@ changes as its last element. If
        -- we used our full working tree as @base@, then we would now unapply
        -- filtered changes from an unfiltered 'Tree', so the result would be
        -- the pristine Tree with the filtered-out unrecorded changes /still
        -- applied/. Unapplying the (unfiltered) recorded changes that touch
        -- paths that we filtered out would then fail (issue2639).
        -- We cannot use 'readUnrecorded' and pass it @mpaths@ because that
        -- would filter the whole Tree, so again unapplying recorded changes
        -- that touch irrelevant paths would fail.
        -- A valid alternative solution would be to not pre-filter unrecorded
        -- changes at all, since we filter the resulting Trees anyway (see
        -- below). But that may be less efficient if there are many unrecorded
        -- changes but we are interested in just a small subset of the affected
        -- paths.
        oldtree <- snd <$> hashedTreeIO (unapply todiff) newtree pdirpath

        writePlainTree (applyTreeFilter relevant oldtree) (toFilePath odir)
        writePlainTree (applyTreeFilter relevant newtree) (toFilePath ndir)
        -- Display patch info for (only) the recorded patches that we diff
        putDoc $ vcat $ map displayPatchInfo $ reverse $ mapFL info tolog

      -- Call the external diff program. Note we are now back in our
      -- temporary directory.
      cmd <- diffProgram
      let old = takeFileName $ toFilePath odir
          new = takeFileName $ toFilePath ndir
      case getDiffCmdAndArgs cmd opts old new of
        Left err -> fail err
        Right (d_cmd, d_args) -> do
          cmdExists <- findExecutable d_cmd
          unless (isJust cmdExists) $
            fail $ d_cmd ++ " is not an executable in --diff-command"
          let pausingForGui = (wantGuiPause opts == YesWantGuiPause)
              cmdline = unwords (d_cmd : d_args)
          when pausingForGui $ putStrLn $ "Running command '" ++ cmdline ++ "'"
          _ <- execInteractive cmdline Nothing
          when pausingForGui $ askEnter "Hit return to move on..."

-- | Returns the command we should use for diff as a tuple (command, arguments).
-- This will either be whatever the user specified via --diff-command  or the
-- default 'diffProgram'.  Note that this potentially involves parsing the
-- user's diff-command, hence the possibility for failure.
getDiffCmdAndArgs :: String -> [DarcsFlag] -> String -> String
                  -> Either String (String, [String])
getDiffCmdAndArgs cmd opts f1 f2 = helper (O.extDiff ? opts) where
  helper extDiff =
    case O.diffCmd extDiff of
      Just c ->
        case parseCmd [ ('1', f1) , ('2', f2) ] c of
          Left err      -> Left $ show err
          Right ([],_)  -> error "parseCmd should never return empty list"
          Right (cmd':args,_)
            | length (filter (== f1) args) == 1
            , length (filter (== f2) args) == 1 -> Right (cmd',args)
            | otherwise -> Left $ "Invalid argument (%1 or %2) in --diff-command"
      Nothing -> -- if no command specified, use 'diff'
        Right (cmd, "-rN":getDiffOpts extDiff++[f1,f2])

getDiffOpts :: O.ExternalDiff -> [String]
getDiffOpts O.ExternalDiff {O.diffOpts=os,O.diffUnified=u} = addUnified os where
  addUnified = if u then ("-u":) else id
