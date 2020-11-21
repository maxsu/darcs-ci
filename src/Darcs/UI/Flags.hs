-- Copyright (C) 2002-2004 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}
-- | Helper functions to access option contents. Some of them are here only to
-- ease the transition from the legacy system where we manually parsed the flag
-- list to the new(er) option system. At some point this module should be
-- renamed and the re-exports from "Darcs.UI.Options.All" removed.
module Darcs.UI.Flags
    ( F.DarcsFlag
    , remoteDarcs
    , diffingOpts
    , diffOpts
    , scanKnown
    , wantGuiPause
    , isInteractive
    , willRemoveLogFile
    , includeBoring
    , lookForAdds
    , lookForMoves
    , lookForReplaces
    , setDefault
    , allowConflicts
    , hasXmlOutput
    , hasLogfile
    , quiet
    , verbose
    , enumeratePatches

    , fixRemoteRepos
    , fixUrl
    , pathsFromArgs
    , pathSetFromArgs
    , getRepourl
    , getAuthor
    , promptAuthor
    , getEasyAuthor
    , getSendmailCmd
    , fileHelpAuthor
    , environmentHelpEmail
    , getSubject
    , getInReplyTo
    , getCc
    , environmentHelpSendmail
    , getOutput
    , getDate
    , workRepo
    , withNewRepo

    -- * Re-exports
    , O.compress
    , O.diffAlgorithm
    , O.reorder
    , O.minimize
    , O.editDescription
    , O.externalMerge
    , O.maxCount
    , O.matchAny
    , O.withContext
    , O.allowCaseDifferingFilenames
    , O.allowWindowsReservedFilenames
    , O.changesReverse
    , O.usePacks
    , O.onlyToFiles
    , O.amendUnrecord
    , O.verbosity
    , O.useCache
    , O.useIndex
    , O.umask
    , O.dryRun
    , O.runTest
    , O.testChanges
    , O.setScriptsExecutable
    , O.withWorkingDir
    , O.leaveTestDir
    , O.remoteRepos
    , O.cloneKind
    , O.patchIndexNo
    , O.patchIndexYes
    , O.xmlOutput
    , O.selectDeps
    , O.author
    , O.patchFormat
    , O.charset
    , O.siblings
    , O.applyAs
    , O.enumPatches
    ) where

import Darcs.Prelude

import Data.List ( intercalate )
import Data.List.Ordered ( nubSort )
import Data.Maybe
    ( isJust
    , maybeToList
    , isNothing
    , catMaybes
    )
import Control.Monad ( unless )
import System.Directory ( doesDirectoryExist, createDirectory )
import System.FilePath.Posix ( (</>) )
import System.Environment ( lookupEnv )

-- Use of RemoteRepo data constructor is harmless here, if not ideal.
-- See haddocks for fixRemoteRepos below for details.
import qualified Darcs.UI.Options.Flags as F ( DarcsFlag(RemoteRepo) )
import Darcs.UI.Options ( Config, (?), (^), oparse, parseFlags, unparseOpt )
import qualified Darcs.UI.Options.All as O

import Darcs.Util.Exception ( catchall )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Prompt
    ( askUser
    , askUserListItem
    )
import Darcs.Util.Lock ( writeTextFile )
import Darcs.Repository.Flags ( WorkRepo(..) )
import Darcs.Repository.Prefs
    ( getPreflist
    , getGlobal
    , globalPrefsDirDoc
    , globalPrefsDir
    , prefsDirPath
    )
import Darcs.Util.IsoDate ( getIsoDateTime, cleanLocalDate )
import Darcs.Util.Path
    ( AbsolutePath
    , AbsolutePathOrStd
    , toFilePath
    , makeSubPathOf
    , ioAbsolute
    , makeAbsoluteOrStd
    , AnchoredPath
    , floatSubPath
    , inDarcsdir
    )
import Darcs.Util.Printer ( pathlist, putDocLn, text, ($$), (<+>) )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.URL ( isValidLocalPath )

verbose :: Config -> Bool
verbose = (== O.Verbose) . parseFlags O.verbosity

quiet :: Config -> Bool
quiet = (== O.Quiet) . parseFlags O.verbosity

remoteDarcs :: Config -> O.RemoteDarcs
remoteDarcs = O.remoteDarcs . parseFlags O.network

enumeratePatches :: Config -> Bool
enumeratePatches = (== O.YesEnumPatches) . parseFlags O.enumPatches

diffOpts :: O.UseIndex -> O.LookForAdds -> O.IncludeBoring -> O.DiffAlgorithm -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffOpts use_index look_for_adds include_boring diff_alg =
    (use_index, scanKnown look_for_adds include_boring, diff_alg)

-- | Non-trivial interaction between options.
scanKnown :: O.LookForAdds -> O.IncludeBoring -> O.ScanKnown
scanKnown O.NoLookForAdds _ = O.ScanKnown
scanKnown O.YesLookForAdds O.NoIncludeBoring = O.ScanAll
scanKnown O.YesLookForAdds O.YesIncludeBoring = O.ScanBoring

diffingOpts :: Config -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffingOpts flags = diffOpts (O.useIndex ? flags) (lookForAdds flags)
    (parseFlags O.includeBoring flags) (O.diffAlgorithm ? flags)

-- | This will become dis-entangled as soon as we inline these functions.
wantGuiPause :: Config -> O.WantGuiPause
wantGuiPause fs =
  if (hasDiffCmd fs || hasExternalMerge fs) && hasPause fs
    then O.YesWantGuiPause
    else O.NoWantGuiPause
  where
    hasDiffCmd = isJust . O.diffCmd . parseFlags O.extDiff
    hasExternalMerge = (/= O.NoExternalMerge) . parseFlags O.externalMerge
    hasPause = (== O.YesWantGuiPause) . parseFlags O.pauseForGui

-- | Non-trivial interaction between options. Explicit @-i@ or @-a@ dominates,
-- else @--count@, @--xml@, or @--dry-run@ imply @-a@, else use the def argument.
isInteractive :: Bool -> Config -> Bool
isInteractive def = oparse (O.dryRunXml ^ O.changesFormat ^ O.interactive) decide
  where
    decide :: O.DryRun -> O.XmlOutput -> Maybe O.ChangesFormat -> Maybe Bool -> Bool
    decide _           _        _                     (Just True)  = True
    decide _           _        _                     (Just False) = False
    decide _           _        (Just O.CountPatches) Nothing      = False
    decide _           O.YesXml _                     Nothing      = False
    decide O.YesDryRun _        _                     Nothing      = False
    decide _           _        _                     Nothing      = def

willRemoveLogFile :: Config -> Bool
willRemoveLogFile = O._rmlogfile . parseFlags O.logfile

includeBoring :: Config -> Bool
includeBoring cfg = case parseFlags O.includeBoring cfg of
  O.NoIncludeBoring -> False
  O.YesIncludeBoring -> True

lookForAdds :: Config -> O.LookForAdds
lookForAdds = O.adds . parseFlags O.lookfor

lookForReplaces :: Config -> O.LookForReplaces
lookForReplaces = O.replaces . parseFlags O.lookfor

lookForMoves :: Config -> O.LookForMoves
lookForMoves = O.moves . parseFlags O.lookfor

setDefault :: Bool -> Config -> O.SetDefault
setDefault defYes = maybe def noDef . parseFlags O.setDefault where
  def = if defYes then O.YesSetDefault False else O.NoSetDefault False
  noDef yes = if yes then O.YesSetDefault True else O.NoSetDefault True

allowConflicts :: Config -> O.AllowConflicts
allowConflicts = maybe O.NoAllowConflicts id . parseFlags O.conflictsNo

-- | Ugly. The alternative is to put the remoteRepos accessor into the IO monad,
-- which is hardly better.
-- However, accessing the flag list directly here is benign, as we only map
-- over the list and don't change the order.
fixRemoteRepos :: AbsolutePath -> Config -> IO Config
fixRemoteRepos d = mapM fixRemoteRepo where
  fixRemoteRepo (F.RemoteRepo p) = F.RemoteRepo `fmap` fixUrl d p
  fixRemoteRepo f = return f

-- | 'fixUrl' takes a String that may be a file path or a URL.
-- It returns either the URL, or an absolute version of the path.
fixUrl :: AbsolutePath -> String -> IO String
fixUrl d f = if isValidLocalPath f
                then toFilePath `fmap` withCurrentDirectory d (ioAbsolute f)
                else return f

-- TODO move the following four functions somewhere else,
-- they have nothing to do with flags

-- | Used by commands that expect arguments to be paths in the current repo.
-- Invalid paths are dropped and a warning is issued. This may leave no valid
-- paths to return. Although these commands all fail if there are no remaining
-- valid paths, they do so in various different ways, issuing error messages
-- tailored to the command.
pathsFromArgs :: (AbsolutePath, AbsolutePath) -> [String] -> IO [AnchoredPath]
pathsFromArgs fps args = catMaybes <$> maybeFixSubPaths fps args

-- | Used by commands that interpret a set of optional path arguments as
-- "restrict to these paths", which affects patch selection (e.g. in log
-- command) or selection of subtrees (e.g. in record). Because of the special
-- meaning of "no arguments", we must distinguish it from "no valid arguments".
-- A result of 'Nothing' here means "no restriction to the set of paths". If
-- 'Just' is returned, the set is guaranteed to be non-empty.
pathSetFromArgs :: (AbsolutePath, AbsolutePath)
                -> [String]
                -> IO (Maybe [AnchoredPath])
pathSetFromArgs _ [] = return Nothing
pathSetFromArgs fps args = do
  pathSet <- nubSort . catMaybes <$> maybeFixSubPaths fps args
  case pathSet of
    [] -> fail "No valid arguments were given."
    _ -> return $ Just pathSet

-- | @maybeFixSubPaths (repo_path, orig_path) file_paths@ tries to turn
-- @file_paths@ into 'SubPath's, taking into account the repository path and
-- the original path from which darcs was invoked.
--
-- A 'SubPath' is a path /under/ (or inside) the repo path. This does /not/
-- mean it must exist as a file or directory, nor that the path has been added
-- to the repository; it merely means that it /could/ be added.
--
-- When converting a relative path to an absolute one, this function first tries
-- to interpret the relative path with respect to the current working directory.
-- If that fails, it tries to interpret it with respect to the repository
-- directory. Only when that fails does it put a @Nothing@ in the result at the
-- position of the path that cannot be converted.
--
-- It is intended for validating file arguments to darcs commands.
maybeFixSubPaths :: (AbsolutePath, AbsolutePath)
                 -> [String]
                 -> IO [Maybe AnchoredPath]
maybeFixSubPaths (r, o) fs = do
  fixedFs <- mapM (fmap dropInDarcsdir . fixit) fs
  let bads = snd . unzip . filter (isNothing . fst) $ zip fixedFs fs
  unless (null bads) $
    ePutDocLn $ text "Ignoring invalid repository paths:" <+> pathlist bads
  return fixedFs
 where
    dropInDarcsdir (Just p) | inDarcsdir p = Nothing
    dropInDarcsdir mp = mp
    -- special case here because fixit otherwise converts
    -- "" to (SubPath "."), which is a valid path
    fixit "" = return Nothing
    fixit p = do ap <- withCurrentDirectory o $ ioAbsolute p
                 case makeSubPathOf r ap of
                   Just sp -> return $ Just $ floatSubPath sp
                   Nothing -> do
                     absolutePathByRepodir <- withCurrentDirectory r $ ioAbsolute p
                     return $ floatSubPath <$> makeSubPathOf r absolutePathByRepodir

-- | 'getRepourl' takes a list of flags and returns the url of the
-- repository specified by @Repodir \"directory\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--repodir=DIRECTORY@
getRepourl :: Config -> Maybe String
getRepourl fs = case parseFlags O.possiblyRemoteRepo fs of
  Nothing -> Nothing
  Just d -> if not (isValidLocalPath d) then Just d else Nothing

fileHelpAuthor :: [String]
fileHelpAuthor = [
 "Each patch is attributed to its author, usually by email address (for",
 "example, `Fred Bloggs <fred@example.net>`).  Darcs looks in several",
 "places for this author string: the `--author` option, the files",
 "`_darcs/prefs/author` (in the repository) and `" ++ globalPrefsDirDoc ++ "author` (in your",
 "home directory), and the environment variables `$DARCS_EMAIL` and",
 "`$EMAIL`.  If none of those exist, Darcs will prompt you for an author",
 "string and write it to `" ++ globalPrefsDirDoc ++ "author`.  Note that if you have more",
 "than one email address, you can put them all in `" ++ globalPrefsDirDoc ++ "author`,",
 "one author per line.  Darcs will still prompt you for an author, but it",
 "allows you to select from the list, or to type in an alternative."
 ]

environmentHelpEmail :: ([String], [String])
environmentHelpEmail = (["DARCS_EMAIL","EMAIL"], fileHelpAuthor)

-- | 'getAuthor' takes a list of flags and returns the author of the
-- change specified by @Author \"Leo Tolstoy\"@ in that list of flags, if any.
-- Otherwise, if @Pipe@ is present, asks the user who is the author and
-- returns the answer. If neither are present, try to guess the author,
-- from repository or global preference files or environment variables,
-- and if it's not possible, ask the user.
getAuthor :: Maybe String -> Bool -> IO String
getAuthor (Just author) _ = return author
getAuthor Nothing pipe =
  if pipe
    then askUser "Who is the author? "
    else promptAuthor True False

-- | 'promptAuthor' try to guess the author, from repository or
-- global preference files or environment variables, and
-- if it's not possible or alwaysAsk parameter is true, ask the user.
-- If store parameter is true, the new author is added into
-- @_darcs/prefs@.
promptAuthor :: Bool -- Store the new author
             -> Bool -- Author selection even if already stored
             -> IO String
promptAuthor store alwaysAsk = do
  as <- getEasyAuthor
  case as of
    [a] -> if alwaysAsk then
             askForAuthor False (fancyPrompt as) (fancyPrompt as)
           else return a
    []  -> askForAuthor True shortPrompt longPrompt
    _   -> askForAuthor False (fancyPrompt as) (fancyPrompt as)
 where
  shortPrompt = askUser "What is your email address? "
  longPrompt  = askUser "What is your email address (e.g. Fred Bloggs <fred@example.net>)? "
  fancyPrompt xs =
    do putDocLn $ text "" $$
                  text "You have saved the following email addresses to your global settings:"
       str <- askUserListItem "Please select an email address for this repository: " (xs ++ ["Other"])
       if str == "Other"
          then longPrompt
          else return str
  askForAuthor storeGlobal askfn1 askfn2 = do
      aminrepo <- doesDirectoryExist prefsDirPath
      if aminrepo && store then do
          prefsdir <- if storeGlobal
                         then tryGlobalPrefsDir
                         else return prefsDirPath
          putDocLn $
            text "Each patch is attributed to its author, usually by email address (for" $$
            text "example, `Fred Bloggs <fred@example.net>').  Darcs could not determine" $$
            text "your email address, so you will be prompted for it." $$
            text "" $$
            text ("Your address will be stored in " ++ prefsdir)
          if prefsdir /= prefsDirPath then
            putDocLn $
              text "It will be used for all patches you record in ALL repositories." $$
              text ("If you move that file to " ++ prefsDirPath </> "author" ++ ", it will") $$
              text "be used for patches recorded in this repository only."
          else
            putDocLn $
              text "It will be used for all patches you record in this repository only." $$
              text ("If you move that file to " ++ globalPrefsDirDoc ++ "author, it will") $$
              text "be used for all patches recorded in ALL repositories."
          add <- askfn1
          writeTextFile (prefsdir </> "author") $
                          unlines ["# " ++ line | line <- fileHelpAuthor] ++ "\n" ++ add
          return add
        else askfn2
  tryGlobalPrefsDir = do
    maybeprefsdir <- globalPrefsDir
    case maybeprefsdir of
      Nothing -> do
        putStrLn "WARNING: Global preference directory could not be found."
        return prefsDirPath
      Just dir -> do exists <- doesDirectoryExist dir
                     unless exists $ createDirectory dir
                     return dir

-- | 'getEasyAuthor' tries to get the author name first from the repository
-- preferences, then from global preferences, then from environment variables.
-- Returns @[]@ if it could not get it. Note that it may only return multiple
-- possibilities when reading from global preferences.
getEasyAuthor :: IO [String]
getEasyAuthor =
  firstNotNullIO [ (take 1 . nonblank) `fmap` getPreflist "author"
                 , nonblank    `fmap` getGlobal "author"
                 , maybeToList `fmap` lookupEnv "DARCS_EMAIL"
                 , maybeToList `fmap` lookupEnv "EMAIL"
                 ]
 where
  nonblank = filter (not . null)
  -- this could perhaps be simplified with Control.Monad
  -- but note that we do NOT want to concatenate the results
  firstNotNullIO [] = return []
  firstNotNullIO (e:es) = do
    v <- e `catchall` return []
    if null v then firstNotNullIO es else return v

getDate :: Bool -> IO String
getDate hasPipe = if hasPipe then cleanLocalDate =<< askUser "What is the date? "
                  else getIsoDateTime

environmentHelpSendmail :: ([String], [String])
environmentHelpSendmail = (["SENDMAIL"], [
 "On Unix, the `darcs send` command relies on sendmail(8).  The",
 "`--sendmail-command` or $SENDMAIL environment variable can be used to",
 "provide an explicit path to this program; otherwise the standard",
 "locations /usr/sbin/sendmail and /usr/lib/sendmail will be tried."])
-- FIXME: mention the following also:
-- * sendmail(8) is not sendmail-specific;
-- * nowadays, desktops often have no MTA or an unconfigured MTA --
--   which is awful, because it accepts mail but doesn't relay it;
-- * in this case, can be a sendmail(8)-emulating wrapper on top of an
--   MUA that sends mail directly to a smarthost; and
-- * on a multi-user system without an MTA and on which you haven't
--   got root, can be msmtp.

-- |'getSendmailCmd' takes a list of flags and returns the sendmail command
-- to be used by @darcs send@. Looks for a command specified by
-- @SendmailCmd \"command\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--sendmail-command=COMMAND@
-- Alternatively the user can set @$S@@ENDMAIL@ which will be used as a
-- fallback if present.
getSendmailCmd :: Config -> IO (Maybe String)
getSendmailCmd fs =
  case parseFlags O.sendmailCmd fs of
    Nothing -> lookupEnv "SENDMAIL"
    justcmd -> return justcmd

-- | Accessor for output option
getOutput :: Config -> FilePath -> Maybe AbsolutePathOrStd
getOutput fs fp = fmap go (parseFlags O.output fs) where
  go (O.Output ap)         = ap
  go (O.OutputAutoName ap) = makeAbsoluteOrStd ap fp

-- |'getSubject' takes a list of flags and returns the subject of the mail
-- to be sent by @darcs send@. Looks for a subject specified by
-- @Subject \"subject\"@ in that list of flags, if any.
-- This flag is present if darcs was invoked with @--subject=SUBJECT@
getSubject :: Config -> Maybe String
getSubject = O._subject . parseFlags O.headerFields

-- |'getCc' takes a list of flags and returns the addresses to send a copy of
-- the patch bundle to when using @darcs send@.
-- looks for a cc address specified by @Cc \"address\"@ in that list of flags.
-- Returns the addresses as a comma separated string.
getCc :: Config -> String
getCc = intercalate " , " . O._cc . parseFlags O.headerFields

getInReplyTo :: Config -> Maybe String
getInReplyTo = O._inReplyTo . parseFlags O.headerFields

hasXmlOutput :: Config -> Bool
hasXmlOutput = (== O.YesXml) . parseFlags O.xmlOutput

hasLogfile :: Config -> Maybe AbsolutePath
hasLogfile = O._logfile . parseFlags O.logfile

workRepo :: Config -> WorkRepo
workRepo = oparse (O.repoDir ^ O.possiblyRemoteRepo) go
  where
    go (Just s) _ = WorkRepoDir s
    go Nothing (Just s) = WorkRepoPossibleURL s
    go Nothing Nothing = WorkRepoCurrentDir

withNewRepo :: String -> Config -> Config
withNewRepo dir = unparseOpt O.newRepo (Just dir)
