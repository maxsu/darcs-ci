--  Copyright (C) 2002-2005 David Roundy
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
module Darcs.UI.Commands.Repair ( repair, check ) where

import Darcs.Prelude

import Control.Monad ( when, unless )
import Control.Exception ( catch, IOException )
import System.Exit ( ExitCode(..), exitWith )
import System.Directory( renameFile )
import System.FilePath ( (<.>) )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults
    , putInfo, putWarning, amInHashedRepository
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag, verbosity, umask, useIndex
    , useCache, compress, diffAlgorithm, quiet
    )
import Darcs.UI.Options
    ( DarcsOption, (^), oid
    , odesc, ocheck, defaultFlags, (?)
    )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository.Flags ( UpdatePending (..) )
import Darcs.Repository.Paths ( indexPath )
import Darcs.Repository.Repair
    ( replayRepository, checkIndex, replayRepositoryInTemp
    , RepositoryConsistency(..)
    )
import Darcs.Repository
    ( Repository, withRepository, readRecorded, RepoJob(..)
    , withRepoLock, replacePristine, repoCache
    )
import qualified Darcs.Repository.Hashed as HashedRepo
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Repository.Diff( treeDiff )

import Darcs.Patch ( RepoPatch, PrimOf, displayPatch )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft )

import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree ( Tree, expand )
import Darcs.Util.Tree.Hashed ( darcsUpdateHashes )


repairDescription :: String
repairDescription = "Repair a corrupted repository."

repairHelp :: Doc
repairHelp = text $
  "The `darcs repair` command attempts to fix corruption in the current\n\
  \repository.\n\
  \It works by successively applying all patches in the repository to an\n\
  \empty tree, each time checking that the patch can be cleanly applied\n\
  \to the current pristine tree. If we detect a problem, we try to repair\n\
  \the patch. Finally we compare the existing pristine with the newly\n\
  \reconstructed one and if they differ, replace the existing one.\n\
  \Any problem encountered is reported.\n\
  \The flag `--dry-run` makes this operation read-only and causes it to\n\
  \exit unsuccessfully (with a non-zero exit status) in case any problems\n\
  \are enountered.\n"

commonBasicOpts :: DarcsOption a
                   (Maybe String -> O.UseIndex -> O.DiffAlgorithm -> a)
commonBasicOpts = O.repoDir ^ O.useIndex ^ O.diffAlgorithm

repair :: DarcsCommand
repair = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "repair"
    , commandHelp = repairHelp
    , commandDescription = repairDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = withFpsAndArgs repairCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , ..
    }
  where
    basicOpts = commonBasicOpts ^ O.dryRun
    advancedOpts = O.umask
    allOpts = basicOpts `withStdOpts` advancedOpts
    commandAdvancedOptions = odesc advancedOpts
    commandBasicOptions = odesc basicOpts
    commandDefaults = defaultFlags allOpts
    commandCheckOptions = ocheck allOpts

withFpsAndArgs :: (b -> d) -> a -> b -> c -> d
withFpsAndArgs cmd _ opts _ = cmd opts

repairCmd :: [DarcsFlag] -> IO ()
repairCmd opts
  | O.yes (O.dryRun ? opts) = checkCmd opts
  | otherwise =
    withRepoLock O.NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repo -> do
      replayRepository
        (diffAlgorithm ? opts)
        repo
        (compress ? opts)
        (verbosity ? opts) $ \state ->
        case state of
          RepositoryConsistent ->
            putInfo opts "The repository is already consistent, no changes made."
          BrokenPristine tree -> do
            putInfo opts "Fixing pristine tree..."
            replacePristine repo tree
          BrokenPatches tree newps -> do
            putInfo opts "Writing out repaired patches..."
            HashedRepo.writeTentativeInventory (repoCache repo) (compress ? opts) newps
            HashedRepo.finalizeTentativeChanges repo (compress ? opts)
            putInfo opts "Fixing pristine tree..."
            replacePristine repo tree
      index_ok <- checkIndex repo (quiet opts)
      unless index_ok $ do
        renameFile indexPath (indexPath <.> "bad")
        putInfo opts "Bad index discarded."

-- |check is an alias for repair, with implicit DryRun flag.
check :: DarcsCommand
check = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "check"
    , commandHelp = "See `darcs repair` for details."
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = withFpsAndArgs checkCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , ..
    }
  where
    basicOpts = commonBasicOpts
    advancedOpts = oid
    allOpts = basicOpts `withStdOpts` advancedOpts
    commandAdvancedOptions = odesc advancedOpts
    commandBasicOptions = odesc basicOpts
    commandDefaults = defaultFlags allOpts
    commandCheckOptions = ocheck allOpts
    commandDescription = "Alias for `darcs " ++ commandName repair ++ " --dry-run'."

checkCmd :: [DarcsFlag] -> IO ()
checkCmd opts = withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  state <- replayRepositoryInTemp (diffAlgorithm ? opts) repository (compress ? opts) (verbosity ? opts)
  failed <-
    case state of
      RepositoryConsistent -> do
        putInfo opts "The repository is consistent!"
        return False
      BrokenPristine newpris -> do
        brokenPristine opts repository newpris
        return True
      BrokenPatches newpris _ -> do
        brokenPristine opts repository newpris
        putInfo opts "Found broken patches."
        return True
  bad_index <- if useIndex ? opts == O.IgnoreIndex
                 then return False
                 else not <$> checkIndex repository (quiet opts)
  when bad_index $ putInfo opts "Bad index."
  exitWith $ if failed || bad_index then ExitFailure 1 else ExitSuccess

brokenPristine
  :: forall rt p wR wU wT . (RepoPatch p)
  => [DarcsFlag] -> Repository rt p wR wU wT -> Tree IO -> IO ()
brokenPristine opts repository newpris = do
  putInfo opts "Looks like we have a difference..."
  mc' <-
    (Just `fmap` (readRecorded repository >>= expand >>= darcsUpdateHashes))
      `catch` (\(_ :: IOException) -> return Nothing)
  case mc' of
    Nothing -> do
      putWarning opts $ "Unable to read the recorded state, try repair."
    Just mc -> do
      ftf <- filetypeFunction
      Sealed (diff :: FL (PrimOf p) wR wR2)
        <- unFreeLeft `fmap` treeDiff (diffAlgorithm ? opts) ftf newpris mc :: IO (Sealed (FL (PrimOf p) wR))
      putInfo opts $ case diff of
        NilFL -> "Nothing"
        patch -> displayPatch patch
