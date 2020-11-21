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

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Util
    ( announceFiles
    , filterExistingPaths
    , testTentativeAndMaybeExit
    , printDryRunMessageAndExit
    , getUniqueRepositoryName
    , getUniqueDPatchName
    , doesDirectoryReallyExist
    , checkUnrelatedRepos
    , preselectPatches
    , getLastPatches
    , matchRange
    , historyEditHelp
    ) where

import Control.Monad ( when, unless )

import Darcs.Prelude

import Data.Char ( isAlpha, toLower, isDigit, isSpace )
import Data.Maybe ( fromMaybe )

import System.Exit ( ExitCode(..), exitWith, exitSuccess )
import System.Posix.Files ( isDirectory )

import Darcs.Patch ( IsRepoType, RepoPatch, xmlSummary )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Depends
    ( areUnrelatedRepos
    , findCommonWithThem
    , patchSetUnion
    )
import Darcs.Patch.Info ( toXml )
import Darcs.Patch.Match
    ( MatchFlag
    , MatchableRP
    , firstMatch
    , matchFirstPatchset
    , matchSecondPatchset
    , matchingHead
    )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, hopefullyM )
import Darcs.Patch.Set ( PatchSet, SealedPatchSet, Origin, emptyPatchSet )
import Darcs.Patch.Witnesses.Ordered ( FL, (:>)(..), mapFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), Sealed2(..) )

import Darcs.Repository
    ( ReadingOrWriting(..)
    , Repository
    , identifyRepositoryFor
    , readRecorded
    , readRepo
    , testTentative
    )
import Darcs.Repository.Prefs ( getDefaultRepo, globalPrefsDirDoc )
import Darcs.Repository.State ( readUnrecordedFiltered )

import Darcs.UI.Commands ( putInfo )
import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.PrintPatch ( showFriendly )
import Darcs.UI.Options ( (?) )
import Darcs.UI.Options.All
    ( Verbosity(..), SetScriptsExecutable, TestChanges (..)
    , RunTest(..), LeaveTestDir(..), UseIndex, ScanKnown(..)
    , WithSummary(..), DryRun(..), XmlOutput(..), LookForMoves
    )
import qualified Darcs.UI.Options.All as O

import Darcs.Util.English ( anyOfClause, itemizeVertical )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.File ( getFileStatus )
import Darcs.Util.Path ( AnchoredPath, displayPath, getUniquePathName )
import Darcs.Util.Printer
    ( Doc, formatWords, ($+$), text, (<+>), hsep, ($$), vcat, vsep
    , putDocLn, insertBeforeLastline, prefix
    , putDocLnWith, pathlist
    )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Prompt ( PromptConfig(..), promptChar, promptYorn )
import Darcs.Util.Tree.Monad ( virtualTreeIO, exists )
import Darcs.Util.Tree ( Tree )


announceFiles :: Verbosity -> Maybe [AnchoredPath] -> String -> IO ()
announceFiles Quiet _ _ = return ()
announceFiles _ (Just paths) message = putDocLn $
    text message <> text ":" <+> pathlist (map displayPath paths)
announceFiles _ _ _ = return ()

testTentativeAndMaybeExit :: Repository rt p wR wU wT
                          -> Verbosity
                          -> TestChanges
                          -> SetScriptsExecutable
                          -> Bool
                          -> String
                          -> String -> Maybe String -> IO ()
testTentativeAndMaybeExit repo verb test sse interactive failMessage confirmMsg withClarification = do
    let (rt,ltd) = case test of
          NoTestChanges    -> (NoRunTest, YesLeaveTestDir)
          YesTestChanges x -> (YesRunTest, x)
    testResult <- testTentative repo rt ltd sse verb
    unless (testResult == ExitSuccess) $ do
        let doExit = maybe id (flip clarifyErrors) withClarification $
                        exitWith testResult
        unless interactive doExit
        putStrLn $ "Looks like " ++ failMessage
        let prompt = "Shall I " ++ confirmMsg ++ " anyway?"
        yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
        unless (yn == 'y') doExit

-- | @'printDryRunMessageAndExit' action flags patches@ prints a string
-- representing the action that would be taken if the @--dry-run@ option had
-- not been passed to darcs. Then darcs exits successfully.  @action@ is the
-- name of the action being taken, like @\"push\"@ @flags@ is the list of flags
-- which were sent to darcs @patches@ is the sequence of patches which would be
-- touched by @action@.
printDryRunMessageAndExit :: RepoPatch p
                          => String
                          -> Verbosity -> WithSummary -> DryRun -> XmlOutput
                          -> Bool -- interactive
                          -> FL (PatchInfoAnd rt p) wX wY
                          -> IO ()
printDryRunMessageAndExit action v s d x interactive patches = do
    when (d == YesDryRun) $ do
        putInfoX $ hsep [ "Would", text action, "the following changes:" ]
        putDocLnWith fancyPrinters put_mode
        putInfoX $ text ""
        putInfoX $ text "Making no changes: this is a dry run."
        exitSuccess
    when (not interactive && s == YesSummary) $ do
        putInfoX $ hsep [ "Will", text action, "the following changes:" ]
        putDocLn put_mode
  where
    put_mode = if x == YesXml
                   then text "<patches>" $$
                        vcat (mapFL (indent . xml_info s) patches) $$
                        text "</patches>"
                   else vsep $ mapFL (showFriendly v s) patches

    putInfoX = if x == YesXml then const (return ()) else putDocLn

    xml_info YesSummary = xml_with_summary
    xml_info NoSummary  = toXml . info

    xml_with_summary hp
        | Just p <- hopefullyM hp = insertBeforeLastline (toXml $ info hp)
                                        (indent $ xmlSummary p)
    xml_with_summary hp = toXml (info hp)

    indent = prefix "    "

-- | Given a repository and two common command options, classify the given list
-- of paths according to whether they exist in the pristine or working tree.
-- Paths which are neither in working nor pristine are reported and dropped.
-- The result is a pair of path lists: those that exist only in the working tree,
-- and those that exist in pristine or working.
filterExistingPaths :: (RepoPatch p, ApplyState p ~ Tree)
                    => Repository rt p wR wU wR
                    -> Verbosity
                    -> UseIndex
                    -> ScanKnown
                    -> LookForMoves
                    -> [AnchoredPath]
                    -> IO ([AnchoredPath],[AnchoredPath])
filterExistingPaths repo verb useidx scan lfm paths = do
      pristine <- readRecorded repo
      working <- readUnrecordedFiltered repo useidx scan lfm (Just paths)
      let check = virtualTreeIO $ mapM exists paths
      (in_pristine, _) <- check pristine
      (in_working, _) <- check working
      let paths_with_info       = zip3 paths in_pristine in_working
          paths_in_neither      = [ p | (p,False,False) <- paths_with_info ]
          paths_only_in_working = [ p | (p,False,True) <- paths_with_info ]
          paths_in_either       = [ p | (p,inp,inw) <- paths_with_info, inp || inw ]
          or_not_added          = if scan == ScanKnown then " or not added " else " "
      unless (verb == Quiet || null paths_in_neither) $ putDocLn $
        "Ignoring non-existing" <> or_not_added <> "paths:" <+>
        pathlist (map displayPath paths_in_neither)
      return (paths_only_in_working, paths_in_either)

getUniqueRepositoryName :: Bool -> FilePath -> IO FilePath
getUniqueRepositoryName talkative name = getUniquePathName talkative buildMsg buildName
  where
    buildName i = if i == -1 then name else name++"_"++show i
    buildMsg n = "Directory or file '"++ name ++
                 "' already exists, creating repository as '"++
                 n ++"'"

getUniqueDPatchName :: FilePath -> IO FilePath
getUniqueDPatchName name = getUniquePathName False (const "") buildName
  where
    buildName i =
      if i == -1 then patchFilename name else patchFilename $ name++"_"++show i

-- |patchFilename maps a patch description string to a safe (lowercased, spaces
-- removed and ascii-only characters) patch filename.
patchFilename :: String -> String
patchFilename the_summary = name ++ ".dpatch"
  where
    name = map safeFileChar the_summary
    safeFileChar c | isAlpha c = toLower c
                   | isDigit c = c
                   | isSpace c = '-'
    safeFileChar _ = '_'

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = maybe False isDirectory `fmap` getFileStatus f

checkUnrelatedRepos :: RepoPatch p
                    => Bool
                    -> PatchSet rt p Origin wX
                    -> PatchSet rt p Origin wY
                    -> IO ()
checkUnrelatedRepos allowUnrelatedRepos us them =
    when ( not allowUnrelatedRepos && areUnrelatedRepos us them ) $
         do confirmed <- promptYorn "Repositories seem to be unrelated. Proceed?"
            unless confirmed $ putStrLn "Cancelled." >> exitSuccess

-- | Get the union of the set of patches in each specified location
remotePatches :: (IsRepoType rt, RepoPatch p)
              => [DarcsFlag]
              -> Repository rt p wX wU wT -> [O.NotInRemote]
              -> IO (SealedPatchSet rt p Origin)
remotePatches opts repository nirs = do
    nirsPaths <- mapM getNotInRemotePath nirs
    putInfo opts $
      "Determining patches not in" <+>
      anyOfClause nirsPaths $$ itemizeVertical 2 nirsPaths
    patchSetUnion `fmap` mapM readNir nirsPaths
  where
    readNir n = do
        r <- identifyRepositoryFor Reading repository (O.useCache ? opts) n
        rps <- readRepo r
        return (Sealed rps)

    getNotInRemotePath :: O.NotInRemote -> IO String
    getNotInRemotePath (O.NotInRemotePath p) = return p
    getNotInRemotePath O.NotInDefaultRepo = do
        defaultRepo <- getDefaultRepo
        let err = fail $ "No default push/pull repo configured, please pass a "
                         ++ "repo name to --" ++ O.notInRemoteFlagName
        maybe err return defaultRepo

getLastPatches :: RepoPatch p
               => [O.MatchFlag] -> PatchSet rt p Origin wR
               -> (PatchSet rt p :> FL (PatchInfoAnd rt p)) Origin wR
getLastPatches matchFlags ps =
  case matchFirstPatchset matchFlags ps of
    Just (Sealed p1s) -> findCommonWithThem ps p1s
    Nothing -> error "precondition: getLastPatches requires a firstMatch"

preselectPatches
  :: (IsRepoType rt, RepoPatch p)
  => [DarcsFlag]
  -> Repository rt p wR wU wT
  -> IO ((PatchSet rt p :> FL (PatchInfoAnd rt p)) Origin wR)
preselectPatches opts repo = do
  allpatches <- readRepo repo
  let matchFlags = O.matchSeveralOrLast ? opts
  case O.notInRemote ? opts of
    [] -> do
      return $
        if firstMatch matchFlags
          then getLastPatches matchFlags allpatches
          else matchingHead matchFlags allpatches
    -- FIXME what about match options when we have --not-in-remote?
    -- It looks like they are simply ignored.
    nirs -> do
      (Sealed thems) <-
        remotePatches opts repo nirs
      return $ findCommonWithThem allpatches thems

matchRange :: MatchableRP p
           => [MatchFlag]
           -> PatchSet rt p Origin wY
           -> Sealed2 (FL (PatchInfoAnd rt p))
matchRange matchFlags ps =
  case (sp1s, sp2s) of
    (Sealed p1s, Sealed p2s) ->
      case findCommonWithThem p2s p1s of
        _ :> us -> Sealed2 us
  where
    sp1s = fromMaybe (Sealed emptyPatchSet) $ matchFirstPatchset matchFlags ps
    sp2s = fromMaybe (Sealed ps) $ matchSecondPatchset matchFlags ps

historyEditHelp :: Doc
historyEditHelp = formatWords
  [ "Note that this command edits the history of your repo. It is"
  , "primarily intended to be used on patches that you authored yourself"
  , "and did not yet publish. Using it for patches that are already"
  , "published, or even ones you did not author yourself, may cause"
  , "confusion and can disrupt your own and other people's work-flow."
  , "This depends a lot on how your project is organized, though, so"
  , "there may be valid exceptions to this rule."
  ]
  $+$ formatWords
  [ "Using the `--not-in-remote` option is a good way to guard against"
  , "accidentally editing published patches. Without arguments, this"
  , "deselects any patches that are also present in the `defaultrepo`."
  , "If you work in a clone of some publically hosted repository,"
  , "then your `defaultrepo` will be that public repo. You can also"
  , "give the option an argument which is a path or URL of some other"
  , "repository; you can use the option multiple times with"
  , "different repositories, which has the effect of treating all"
  , "of them as \"upstream\", that is, it prevents you from selecting"
  , "a patch that is contained in any of these repos."
  ]
  $+$ formatWords
  [ "You can also guard only against editing another developer's patch"
  , "by using an appropriate `--match` option with the `author` keyword."
  , "For instance, you could add something like `<cmd> match Your Name`"
  , "to your `" ++ globalPrefsDirDoc ++ "defaults`."
  ]
