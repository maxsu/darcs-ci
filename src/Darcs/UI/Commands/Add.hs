--  Copyright (C) 2002-2004 David Roundy
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

-- |
-- Module      : Darcs.UI.Commands.Add
-- Copyright   : 2002-2004 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.Add ( add ) where

import Darcs.Prelude

import Control.Exception ( catch, IOException )
import Control.Monad ( when, unless )
import Data.List ( (\\), nub )
import Data.List.Ordered ( nubSort )
import Data.Maybe ( fromMaybe, isNothing, maybeToList )
import Darcs.Util.Printer ( Doc, text, vcat )
import Darcs.Util.Tree ( Tree, findTree, expand, explodePaths )
import qualified Darcs.Util.Tree as Tree
import Darcs.Util.Path
    ( AbsolutePath
    , AnchoredPath
    , displayPath
    , filterPaths
    , parent
    , parents
    , realPath
    )
import System.Posix.Files ( isRegularFile, isDirectory, isSymbolicLink )
import System.Directory ( getPermissions, readable )

import qualified System.FilePath.Windows as WindowsFilePath

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, putInfo, putWarning, putVerboseWarning
    , nodefaults, amInHashedRepository)
import Darcs.UI.Commands.Util.Tree ( treeHas, treeHasDir, treeHasAnycase )
import Darcs.UI.Commands.Util ( doesDirectoryReallyExist )
import Darcs.UI.Completion ( unknownFileArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , includeBoring, allowCaseDifferingFilenames, allowWindowsReservedFilenames, useCache, dryRun, umask
    , pathsFromArgs )
import Darcs.UI.Options
    ( (^), odesc, ocheck, defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.Patch ( PrimPatch, applyToTree, addfile, adddir, listTouchedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Repository.State
    ( TreeFilter(..)
    , readRecordedAndPending
    , readWorking
    , updateIndex
    )
import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    )
import Darcs.Repository.Prefs ( isBoring )
import Darcs.Util.File ( getFileStatus )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+), nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), Gap(..), FreeLeft, unFreeLeft )

addDescription :: String
addDescription = "Add new files to version control."


addHelp :: Doc
addHelp = text $
    "Generally the working tree contains both files that should be version\n" ++
    "controlled (such as source code) and files that Darcs should ignore\n" ++
    "(such as executables compiled from the source code).  The `darcs add`\n" ++
    "command is used to tell Darcs which files to version control.\n" ++
    "\n" ++
    "When an existing project is first imported into a Darcs repository, it\n" ++
    "is common to run `darcs add -r *` or `darcs record -l` to add all\n" ++
    "initial source files into darcs.\n"++
    "\n" ++
    "Adding symbolic links (symlinks) is not supported.\n\n"


addHelp' :: Doc
addHelp' = text $
    "Darcs will ignore all files and folders that look \"boring\".  The\n" ++
    "`--boring` option overrides this behaviour.\n" ++
    "\n" ++
    "Darcs will not add file if another file in the same folder has the\n" ++
    "same name, except for case.  The `--case-ok` option overrides this\n" ++
    "behaviour.  Windows and OS X usually use filesystems that do not allow\n" ++
    "files a folder to have the same name except for case (for example,\n" ++
    "`ReadMe` and `README`).  If `--case-ok` is used, the repository might be\n" ++
    "unusable on those systems!\n\n"

add :: DarcsCommand
add = DarcsCommand
    { commandProgramName          = "darcs"
    , commandName                 = "add"
    , commandHelp                 = addHelp <> addHelp'
    , commandDescription          = addDescription
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = [ "<FILE or DIRECTORY> ..." ]
    , commandCommand              = addCmd
    , commandPrereq               = amInHashedRepository
    , commandCompleteArgs  = unknownFileArgs
    , commandArgdefaults          = nodefaults
    , commandAdvancedOptions      = odesc addAdvancedOpts
    , commandBasicOptions         = odesc addBasicOpts
    , commandDefaults             = defaultFlags addOpts
    , commandCheckOptions         = ocheck addOpts
    }
  where
    addBasicOpts
      = O.includeBoring
      ^ O.allowProblematicFilenames
      ^ O.recursive
      ^ O.repoDir
      ^ O.dryRun
    addAdvancedOpts = O.umask
    addOpts = withStdOpts addBasicOpts addAdvancedOpts


addCmd :: (AbsolutePath, AbsolutePath)
       -> [DarcsFlag]
       -> [String]
       -> IO ()
addCmd fps opts args
  | null args = putStrLn $ "Nothing specified, nothing added." ++
      "Maybe you wanted to say `darcs add --recursive .'?"
  | otherwise = do
      paths <- pathsFromArgs fps args
      case paths of
        [] -> fail "No valid repository paths were given"
        _ -> addFiles opts paths

addFiles :: [DarcsFlag] -> [AnchoredPath] -> IO ()
addFiles opts paths =
  withRepoLock (dryRun ? opts) (useCache ? opts) YesUpdatePending (umask ? opts) $
  RepoJob $ \repository -> do
    -- TODO do not expand here, and use findM/findIO or such later
    -- (needs adding to hashed-storage first though)
    cur <- expand =<< readRecordedAndPending repository
    let parent_paths = notInTreeParents cur paths
    -- (1) note, readWorking already filters out darcsdir paths
    -- (2) note, filterPaths matches if path is parent /or/ child 
    working <- readWorking (TreeFilter (Tree.filter (filterPaths paths)))
    -- we first get the boring paths, too, so we can report dropping them
    let all_paths = nubSort $ parent_paths ++
                      (if parseFlags O.recursive opts
                        then explodePaths working
                        else id) paths
        all_orig_paths = map displayPath all_paths
    boring <- isBoring
    let nboring s = if includeBoring opts then id else filter (not . boring . s)
    mapM_ (putWarning opts . text . ((msgSkipping msgs ++ " boring file ")++)) $
        all_orig_paths \\ nboring id all_orig_paths
    Sealed ps <- fmap unFreeLeft $ addp msgs opts cur $ nboring realPath all_paths
    when (nullFL ps && not (null paths)) $
        fail "No files were added"
    unless gotDryRun $
      do addToPending repository (O.useIndex ? opts) ps
         updateIndex repository
         putInfo opts $ vcat $ map text $ ["Finished adding:"] ++
            map displayPath (listTouchedFiles ps)
  where
    gotDryRun = dryRun ? opts == O.YesDryRun
    msgs | gotDryRun = dryRunMessages
         | otherwise = normalMessages

addp :: forall prim . (PrimPatch prim, ApplyState prim ~ Tree)
     => AddMessages
     -> [DarcsFlag]
     -> Tree IO
     -> [AnchoredPath]
     -> IO (FreeLeft (FL prim))
addp msgs opts cur0 files = do
    (ps, dups) <-
        foldr
             (\f rest cur accPS accDups -> do
                    addResult <- addp' cur f
                    case addResult of
                        -- If a single file fails to add, stop further processing.
                        (_, Nothing, Nothing) -> return ([], [])
                        (cur', mp, mdup) -> rest cur' (maybeToList mp ++ accPS) (maybeToList mdup ++ accDups))
            (\_ ps dups -> return (reverse ps, dups))
            files
            cur0 [] []
    let uniq_dups = nub dups
        caseMsg =
            if gotAllowCaseOnly then ":"
                else ";\nnote that to ensure portability we don't allow\n" ++
                     "files that differ only in case. Use --case-ok to override this:"
    unless (null dups) $ do
       dupMsg <-
         case uniq_dups of
         [f] -> do
           isDir <- doesDirectoryReallyExist (realPath f)
           if isDir
             then return $
               "The following directory " ++
               msgIs msgs ++ " already in the repository"
             else return $
               "The following file " ++
               msgIs msgs ++ " already in the repository"
         fs   -> do
           areDirs <- mapM (doesDirectoryReallyExist . realPath) fs
           if and areDirs
             then return $
               "The following directories " ++
               msgAre msgs ++ " already in the repository"
             else
               (if or areDirs
                  then return $
                    "The following files and directories " ++
                    msgAre msgs ++ " already in the repository"
                  else return $
                    "The following files " ++
                    msgAre msgs ++ " already in the repository")
       putWarning opts . text $ "WARNING: Some files were not added because they are already in the repository."
       putVerboseWarning opts . text $ dupMsg ++ caseMsg
       mapM_ (putVerboseWarning opts . text . displayPath) uniq_dups
    return $ foldr (joinGap (+>+)) (emptyGap NilFL) ps
  where
    addp' :: Tree IO
          -> AnchoredPath
          -> IO (Tree IO, Maybe (FreeLeft (FL prim)), Maybe AnchoredPath)
    addp' cur f = do
      already_has <- (if gotAllowCaseOnly then treeHas else treeHasAnycase) cur f
      mstatus <- getFileStatus (realPath f)
      case (already_has, is_badfilename, mstatus) of
        (True, _, _) -> return (cur, Nothing, Just f)
        (_, True, _) -> do
            putWarning opts . text $
              "The filename " ++ displayPath f ++ " is invalid on Windows.\n" ++
              "Use --reserved-ok to allow it."
            return add_failure
        (_, _, Just s)
            | isDirectory s    -> trypatch $ freeGap (adddir f :>: NilFL)
            | isRegularFile s  -> trypatch $ freeGap (addfile f :>: NilFL)
            | isSymbolicLink s -> do
                putWarning opts . text $
                    "Sorry, file " ++ displayPath f ++
                    " is a symbolic link, which is unsupported by darcs."
                return add_failure
        _ -> do
            putWarning opts . text $ "File "++ displayPath f ++" does not exist!"
            return add_failure
        where
          is_badfilename = not (gotAllowWindowsReserved || WindowsFilePath.isValid (realPath f))
          add_failure = (cur, Nothing, Nothing)
          trypatch :: FreeLeft (FL prim)
                   -> IO (Tree IO, Maybe (FreeLeft (FL prim)), Maybe AnchoredPath)
          trypatch p = do
              perms <- getPermissions (realPath f)
              if not $ readable perms
                then do
                    putWarning opts . text $
                        msgSkipping msgs ++ " '" ++ displayPath f ++ "': permission denied "
                    return (cur, Nothing, Nothing)
                else trypatch' p
          trypatch' p = do
              Sealed p' <- return $ unFreeLeft p
              ok <- treeHasDir cur parentdir
              if ok
                then do
                    tree <- applyToTree p' cur
                    putInfo opts . text $
                        msgAdding msgs ++ " '" ++ displayPath f ++ "'"
                    return (tree, Just p, Nothing)
                else do
                    putWarning opts . text $
                        msgSkipping msgs ++ " '" ++ displayPath f ++
                            "' ... couldn't add parent directory '" ++
                            displayPath parentdir ++ "' to repository"
                    return (cur, Nothing, Nothing)
              `catch` \(e :: IOException) -> do
                  putWarning opts . text $
                      msgSkipping msgs ++ " '" ++ displayPath f ++ "' ... " ++ show e
                  return (cur, Nothing, Nothing)
          parentdir = fromMaybe (error "cannot take parent of root path") $ parent f
              
    gotAllowCaseOnly = allowCaseDifferingFilenames ? opts
    gotAllowWindowsReserved = allowWindowsReservedFilenames ? opts


data AddMessages = AddMessages
    {
      msgSkipping  :: String
    , msgAdding    :: String
    , msgIs        :: String
    , msgAre       :: String
    }


normalMessages :: AddMessages
normalMessages = AddMessages
    {
      msgSkipping  = "Skipping"
    , msgAdding    = "Adding"
    , msgIs        = "is"
    , msgAre       = "are"
    }


dryRunMessages :: AddMessages
dryRunMessages = AddMessages
    {
      msgSkipping  = "Would skip"
    , msgAdding    = "Would add"
    , msgIs        = "would be"
    , msgAre       = "would be"
    }


notInTreeParents :: Tree IO -> [AnchoredPath] -> [AnchoredPath]
notInTreeParents cur = filter (isNothing . findTree cur) . concatMap parents
