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
module Darcs.UI.Commands.Push ( push ) where

import Darcs.Prelude

import System.Exit ( exitWith, ExitCode( ExitSuccess, ExitFailure ), exitSuccess )
import Control.Monad ( when, unless )
import Data.Maybe ( isJust )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putVerbose
    , putInfo
    , putFinished
    , abortRun
    , setEnvDarcsPatches
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Commands.Clone ( otherHelpInheritDefault )
import Darcs.UI.Commands.Util ( printDryRunMessageAndExit, checkUnrelatedRepos )
import Darcs.UI.Completion ( prefArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , isInteractive, verbosity, withContext
    , xmlOutput, selectDeps, applyAs, remoteDarcs
    , changesReverse, dryRun, useCache, remoteRepos, setDefault, fixUrl )
import Darcs.UI.Options
    ( (^), odesc, ocheck
    , defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( DryRun (..) )
import qualified Darcs.Repository.Flags as R ( remoteDarcs )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Repository
    ( RepoJob(..)
    , Repository
    , identifyRepositoryFor
    , ReadingOrWriting(..)
    , readRepo
    , withRepository
    )
import Darcs.Patch ( IsRepoType, RepoPatch, description )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), RL, FL, nullRL,
    nullFL, reverseFL, mapFL_FL, mapRL )
import Darcs.Repository.Prefs ( addRepoSource, getPreflist )
import Darcs.UI.External ( signString, darcsProgram
                         , pipeDoc, pipeDocSSH )
import Darcs.Util.Exception ( die )
import Darcs.Util.URL ( isHttpUrl, isValidLocalPath
                      , isSshUrl, splitSshUrl, SshFilePath(..) )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , selectionConfig
    , runSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Patch.Bundle ( makeBundle )
import Darcs.Patch.Show( ShowPatch )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , ($+$)
    , (<+>)
    , empty
    , formatWords
    , quoted
    , text
    , vcat
    )
import Darcs.UI.Email ( makeEmail )
import Darcs.Util.English (englishNum, Noun(..))
import Darcs.Util.Workaround ( getCurrentDirectory )
import Darcs.Util.Tree( Tree )


pushDescription :: String
pushDescription =
 "Copy and apply patches from this repository to another one."

pushHelp :: Doc
pushHelp =
  formatWords
    [ "Push is the opposite of pull.  Push allows you to copy patches from the"
    , "current repository into another repository."
    ]
  $+$ formatWords
    [ "If you give the `--apply-as` flag, darcs will use `sudo` to apply the"
    , "patches as a different user.  This can be useful if you want to set up a"
    , "system where several users can modify the same repository, but you don't"
    , "want to allow them full write access.  This isn't secure against skilled"
    , "malicious attackers, but at least can protect your repository from clumsy,"
    , "inept or lazy users."
    ]
  $+$ formatWords
    [ "`darcs push` will compress the patch data before sending it to a remote"
    , "location via ssh. This works as long as the remote darcs is not older"
    , "than version 2.5. If you get errors that indicate a corrupt patch bundle,"
    , "you should try again with the `--no-compress` option."
    ]
  $+$
  otherHelpInheritDefault

push :: DarcsCommand
push = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "push"
    , commandHelp = pushHelp
    , commandDescription = pushDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[REPOSITORY]"]
    , commandCommand = pushCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = prefArgs "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc pushAdvancedOpts
    , commandBasicOptions = odesc pushBasicOpts
    , commandDefaults = defaultFlags pushOpts
    , commandCheckOptions = ocheck pushOpts
    }
  where
    pushBasicOpts
      = O.matchSeveral
      ^ O.selectDeps
      ^ O.interactive
      ^ O.sign
      ^ O.dryRunXml
      ^ O.withSummary
      ^ O.repoDir
      ^ O.setDefault
      ^ O.inheritDefault
      ^ O.allowUnrelatedRepos
    pushAdvancedOpts
      = O.applyAs
      ^ O.remoteRepos
      ^ O.changesReverse
      ^ O.compress
      ^ O.network
    pushOpts = pushBasicOpts `withStdOpts` pushAdvancedOpts

pushCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
pushCmd (_, o) opts [unfixedrepodir] = do
  repodir <- fixUrl o unfixedrepodir
  here <- getCurrentDirectory
  checkOptionsSanity opts repodir
  -- make sure we aren't trying to push to the current repo
  when (repodir == here) $ die "Cannot push from repository to itself."
  bundle <-
    withRepository (useCache ? opts) $ RepoJob $ prepareBundle opts repodir
  sbundle <- signString (parseFlags O.sign opts) bundle
  let body =
        if isValidLocalPath repodir
          then sbundle
          else makeEmail repodir [] Nothing Nothing sbundle Nothing
  rval <- remoteApply opts repodir body
  case rval of
    ExitFailure ec -> do
      ePutDocLn (text "Apply failed!")
      exitWith (ExitFailure ec)
    ExitSuccess -> putFinished opts "pushing"
pushCmd _ _ [] = die "No default repository to push to, please specify one."
pushCmd _ _ _ = die "Cannot push to more than one repo."

prepareBundle :: forall rt p wR wU wT. (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> String -> Repository rt p wR wU wT -> IO Doc
prepareBundle opts repodir repository = do
  old_default <- getPreflist "defaultrepo"
  when (old_default == [repodir]) $
       let pushing = if dryRun ? opts == YesDryRun then "Would push" else "Pushing"
       in  putInfo opts $ text pushing <+> "to" <+> quoted repodir <> "..."
  them <- identifyRepositoryFor Writing repository (useCache ? opts) repodir >>= readRepo
  addRepoSource repodir (dryRun ? opts) (remoteRepos ? opts)
      (setDefault False opts) (O.inheritDefault ? opts) (isInteractive True opts)
  us <- readRepo repository
  common :> us' <- return $ findCommonWithThem us them
  prePushChatter opts us (reverseFL us') them
  let direction = if changesReverse ? opts then FirstReversed else First
      selection_config = selectionConfig direction "push" (pushPatchSelOpts opts) Nothing Nothing
  runSelection us' selection_config
                   >>= bundlePatches opts common

prePushChatter :: forall rt p a wX wY wT . (RepoPatch p, ShowPatch a) =>
                 [DarcsFlag] -> PatchSet rt p Origin wX ->
                 RL a wT wX -> PatchSet rt p Origin wY -> IO ()
prePushChatter opts us us' them = do
  checkUnrelatedRepos (parseFlags O.allowUnrelatedRepos opts) us them
  let num_to_pull = snd $ countUsThem us them
      pull_reminder = if num_to_pull > 0
                      then text $ "The remote repository has " ++ show num_to_pull
                      ++ " " ++ englishNum num_to_pull (Noun "patch") " to pull."
                      else empty
  putVerbose opts $ text "We have the following patches to push:" $$ vcat (mapRL description us')
  unless (nullRL us') $ putInfo opts pull_reminder
  when (nullRL us') $ do putInfo opts $ text "No recorded local patches to push!"
                         exitSuccess

bundlePatches :: forall t rt p wZ wW wA. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet rt p wA wZ
              -> (FL (PatchInfoAnd rt p) :> t) wZ wW
              -> IO Doc
bundlePatches opts common (to_be_pushed :> _) =
    do
      setEnvDarcsPatches to_be_pushed
      printDryRunMessageAndExit "push"
        (verbosity ? opts)
        (O.withSummary ? opts)
        (dryRun ? opts)
        (xmlOutput ? opts)
        (isInteractive True opts)
        to_be_pushed
      when (nullFL to_be_pushed) $ do
          putInfo opts $
            text "You don't want to push any patches, and that's fine with me!"
          exitSuccess
      makeBundle Nothing common (mapFL_FL hopefully to_be_pushed)

checkOptionsSanity :: [DarcsFlag] -> String -> IO ()
checkOptionsSanity opts repodir =
  if isHttpUrl repodir then do
       when (isJust $ applyAs ? opts) $
           abortRun opts $ text "Cannot --apply-as when pushing to URLs"
       let lprot = takeWhile (/= ':') repodir
           msg = text ("Pushing to "++lprot++" URLs is not supported.")
       abortRun opts msg
   else when (parseFlags O.sign opts /= O.NoSign) $
        abortRun opts $ text "Signing doesn't make sense for local repositories or when pushing over ssh."


pushPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
pushPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    , S.withContext = withContext ? flags
    }

remoteApply :: [DarcsFlag] -> String -> Doc -> IO ExitCode
remoteApply opts repodir bundle
    = case applyAs ? opts of
        Nothing
            | isSshUrl repodir -> applyViaSsh opts (splitSshUrl repodir) bundle
            | otherwise -> applyViaLocal opts repodir bundle
        Just un
            | isSshUrl repodir -> applyViaSshAndSudo opts (splitSshUrl repodir) un bundle
            | otherwise -> applyViaSudo un repodir bundle

applyViaSudo :: String -> String -> Doc -> IO ExitCode
applyViaSudo user repo bundle =
    darcsProgram >>= \darcs ->
    pipeDoc "sudo" ["-u",user,darcs,"apply","--all","--repodir",repo] bundle

applyViaLocal :: [DarcsFlag] -> String -> Doc -> IO ExitCode
applyViaLocal opts repo bundle =
    darcsProgram >>= \darcs ->
    pipeDoc darcs ("apply":"--all":"--repodir":repo:applyopts opts) bundle

applyViaSsh :: [DarcsFlag] -> SshFilePath -> Doc -> IO ExitCode
applyViaSsh opts repo =
    pipeDocSSH (parseFlags O.compress opts) repo
           [R.remoteDarcs (remoteDarcs opts) ++" apply --all "++unwords (applyopts opts)++
                     " --repodir '"++sshRepo repo++"'"]

applyViaSshAndSudo :: [DarcsFlag] -> SshFilePath -> String -> Doc -> IO ExitCode
applyViaSshAndSudo opts repo username =
    pipeDocSSH (parseFlags O.compress opts) repo
           ["sudo -u "++username++" "++R.remoteDarcs (remoteDarcs opts)++
                     " apply --all --repodir '"++sshRepo repo++"'"]

applyopts :: [DarcsFlag] -> [String]
applyopts opts = if parseFlags O.debug opts then ["--debug"] else []
