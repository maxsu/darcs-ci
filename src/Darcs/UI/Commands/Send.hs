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

module Darcs.UI.Commands.Send ( send ) where

import Darcs.Prelude

import System.Directory ( renameFile )
import System.Exit ( exitSuccess )
import System.IO ( hClose )
import Control.Exception ( catch, IOException, onException )
import Control.Monad ( when, unless, forM_ )
import Darcs.Util.Tree ( Tree )
import Data.List ( intercalate, isPrefixOf )
import Data.List ( stripPrefix )
import Data.Maybe ( isNothing, fromMaybe )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putInfo
    , putVerbose
    , setEnvDarcsPatches
    , defaultRepo
    , amInHashedRepository
    )
import Darcs.UI.Commands.Clone ( otherHelpInheritDefault )
import Darcs.UI.Commands.Util ( printDryRunMessageAndExit, checkUnrelatedRepos )
import Darcs.UI.Flags
    ( DarcsFlag
    , willRemoveLogFile, changesReverse, dryRun, useCache, remoteRepos, setDefault
    , fixUrl
    , getCc
    , getAuthor
    , getSubject
    , getInReplyTo
    , getSendmailCmd
    , getOutput
    , charset
    , verbosity
    , isInteractive
    , author
    , hasLogfile
    , selectDeps
    , minimize
    , editDescription
    )
import Darcs.UI.Options
    ( (^), odesc, ocheck
    , defaultFlags, parseFlags, (?)
    )
import qualified Darcs.UI.Options.All as O

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, patchDesc )
import Darcs.Repository
    ( Repository
    , repoLocation
    , PatchSet
    , identifyRepositoryFor
    , ReadingOrWriting(..)
    , withRepository
    , RepoJob(..)
    , readRepo
    , readRecorded
    , prefsUrl )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch ( IsRepoType, RepoPatch, description, applyToTree, effect, invert )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:>)(..), (:\/:)(..),
    mapFL, mapFL_FL, lengthFL, nullFL )
import Darcs.Patch.Bundle
    ( makeBundle
    , minContext
    , readContextFile
    )
import Darcs.Repository.Prefs ( addRepoSource, getPreflist )
import Darcs.Repository.Flags ( DryRun(..) )
import Darcs.Util.External ( fetchFilePS, Cachable(..) )
import Darcs.UI.External
    ( signString
    , sendEmailDoc
    , generateEmail
    , editFile
    , getSystemEncoding
    , isUTF8Locale
    , checkDefaultSendmail
    )
import Darcs.Util.ByteString ( mmapFilePS, isAscii )
import qualified Data.ByteString.Char8 as BC (unpack)
import Darcs.Util.Lock
    ( withOpenTemp
    , writeDocBinFile
    , readDocBinFile
    , removeFileMayNotExist
    )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , selectionConfig
    , runSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Util.Prompt ( askUser, promptYorn )
import Data.Text.Encoding       ( decodeUtf8' )
import Darcs.Util.Progress ( debugMessage )
import Darcs.UI.Email ( makeEmail )
import Darcs.UI.Completion ( prefArgs )
import Darcs.UI.Commands.Util ( getUniqueDPatchName )
import Darcs.Util.Printer
    ( Doc, formatWords, vsep, text, ($$), (<+>), putDoc, putDocLn
    , quoted, renderPS, sentence, vcat
    )
import Darcs.Util.English ( englishNum, Noun(..) )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Path ( FilePathLike, toFilePath, AbsolutePath, AbsolutePathOrStd,
                        getCurrentDirectory, useAbsoluteOrStd, makeAbsoluteOrStd )
import Darcs.Util.HTTP ( postUrl )
import Darcs.Util.Global ( darcsSendMessage, darcsSendMessageFinal )
import Darcs.Util.SignalHandler ( catchInterrupt )

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.interactive = isInteractive True flags
    , S.selectDeps = selectDeps ? flags
    , S.withSummary = O.withSummary ? flags
    , S.withContext = O.NoContext
    }

send :: DarcsCommand
send = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "send"
    , commandHelp = cmdHelp
    , commandDescription = cmdDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["[REPOSITORY]"]
    , commandCommand = sendCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = prefArgs "repos"
    , commandArgdefaults = defaultRepo
    , commandAdvancedOptions = odesc sendAdvancedOpts
    , commandBasicOptions = odesc sendBasicOpts
    , commandDefaults = defaultFlags sendOpts
    , commandCheckOptions = ocheck sendOpts
    }
  where
    sendBasicOpts
      = O.matchSeveral
      ^ O.selectDeps
      ^ O.interactive -- True
      ^ O.headerFields
      ^ O.author
      ^ O.charset
      ^ O.mail
      ^ O.sendmailCmd
      ^ O.output
      ^ O.sign
      ^ O.dryRunXml
      ^ O.withSummary
      ^ O.editDescription
      ^ O.setDefault
      ^ O.inheritDefault
      ^ O.repoDir
      ^ O.minimize
      ^ O.allowUnrelatedRepos
    sendAdvancedOpts
      = O.logfile
      ^ O.remoteRepos
      ^ O.sendToContext 
      ^ O.changesReverse
      ^ O.network
    sendOpts = sendBasicOpts `withStdOpts` sendAdvancedOpts

sendCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
sendCmd fps opts [""] = sendCmd fps opts []
sendCmd (_,o) opts [unfixedrepodir] =
 withRepository (useCache ? opts) $ RepoJob $
  \(repository :: Repository rt p wR wU wR) -> do
  when (O.mail ? opts && dryRun ? opts == O.NoDryRun) $ do
    -- If --mail is used and the user has not provided a --sendmail-command
    -- and we can detect that the system has no default way to send emails, 
    -- then we want to fail early i.e. before asking the user any questions.
    sm_cmd <- getSendmailCmd opts
    when (isNothing sm_cmd) checkDefaultSendmail
  case O.sendToContext ? opts of
    Just contextfile -> do
        wtds <- decideOnBehavior opts (Nothing :: Maybe (Repository rt p wR wU wR))
        ref <- readRepo repository
        Sealed them <- readContextFile ref (toFilePath contextfile)
        sendToThem repository opts wtds "CONTEXT" them
    Nothing -> do
        repodir <- fixUrl o unfixedrepodir
        -- Test to make sure we aren't trying to push to the current repo
        here <- getCurrentDirectory
        when (repodir == toFilePath here) $
           fail cannotSendToSelf
        old_default <- getPreflist "defaultrepo"
        when (old_default == [repodir]) $
            putInfo opts (creatingPatch repodir)
        repo <- identifyRepositoryFor Reading repository (useCache ? opts) repodir
        them <- readRepo repo
        addRepoSource repodir (dryRun ? opts) (remoteRepos ? opts)
            (setDefault False opts) (O.inheritDefault ? opts) (isInteractive True opts)
        wtds <- decideOnBehavior opts (Just repo)
        sendToThem repository opts wtds repodir them
sendCmd _ _ _ = error "impossible case"

sendToThem :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
           => Repository rt p wR wU wT -> [DarcsFlag] -> [WhatToDo] -> String
           -> PatchSet rt p Origin wX -> IO ()
sendToThem repo opts wtds their_name them = do
  us <- readRepo repo
  common :> us' <- return $ findCommonWithThem us them
  checkUnrelatedRepos (O.allowUnrelatedRepos ? opts) us them
  case us' of
      NilFL -> do putInfo opts nothingSendable
                  exitSuccess
      _     -> putVerbose opts $ selectionIs (mapFL description us')
  pristine <- readRecorded repo
  let direction = if changesReverse ? opts then FirstReversed else First
      selection_config = selectionConfig direction "send" (patchSelOpts opts) Nothing Nothing
  (to_be_sent :> _) <- runSelection us' selection_config
  printDryRunMessageAndExit "send"
      (verbosity ? opts)
      (O.withSummary ? opts)
      (dryRun ? opts)
      O.NoXml
      (isInteractive True opts)
      to_be_sent
  when (nullFL to_be_sent) $ do
      putInfo opts selectionIsNull
      exitSuccess
  setEnvDarcsPatches to_be_sent

  let genFullBundle = prepareBundle opts common  (Right (pristine, us':\/:to_be_sent))
  bundle <- if not (minimize ? opts)
             then genFullBundle
             else do putInfo opts "Minimizing context, to send with full context hit ctrl-C..."
                     ( case minContext common to_be_sent of
                         Sealed (common' :> to_be_sent') -> prepareBundle opts common' (Left to_be_sent') )
                     `catchInterrupt` genFullBundle
  here   <- getCurrentDirectory
  let make_fname (tb:>:_) = getUniqueDPatchName $ patchDesc tb
      make_fname _ = error "impossible case"
  fname <- make_fname to_be_sent
  let outname = case getOutput opts fname of
                    Just f  -> Just f
                    Nothing | O.mail ? opts -> Nothing
                            | not $ null [ p | Post p <- wtds] -> Nothing
                            | otherwise        -> Just (makeAbsoluteOrStd here fname)
  case outname of
    Just fname' -> writeBundleToFile opts to_be_sent bundle fname' wtds their_name
    Nothing     -> sendBundle opts to_be_sent bundle fname wtds their_name


prepareBundle :: forall rt p wX wY wZ. (RepoPatch p, ApplyState p ~ Tree)
              => [DarcsFlag] -> PatchSet rt p Origin wZ
              -> Either (FL (PatchInfoAnd rt p) wX wY)
                        (Tree IO, (FL (PatchInfoAnd rt p) :\/: FL (PatchInfoAnd rt p)) wX wY)
              -> IO Doc
prepareBundle opts common e = do
  unsig_bundle <-
     case e of
       (Right (pristine, us' :\/: to_be_sent)) -> do
         pristine' <- applyToTree (invert $ effect us') pristine
         makeBundle (Just pristine')
                     (unsafeCoerceP common)
                     (mapFL_FL hopefully to_be_sent)
       Left to_be_sent -> makeBundle Nothing
                                      (unsafeCoerceP common)
                                      (mapFL_FL hopefully to_be_sent)
  signString (parseFlags O.sign opts) unsig_bundle

sendBundle :: forall rt p wX wY . (RepoPatch p, ApplyState p ~ Tree)
           => [DarcsFlag] -> FL (PatchInfoAnd rt p) wX wY
             -> Doc -> String -> [WhatToDo] -> String -> IO ()
sendBundle opts to_be_sent bundle fname wtds their_name=
         let
           auto_subject :: forall pp wA wB . FL (PatchInfoAnd rt pp) wA wB -> String
           auto_subject (p:>:NilFL)  = "darcs patch: " ++ trim (patchDesc p) 57
           auto_subject (p:>:ps) = "darcs patch: " ++ trim (patchDesc p) 43 ++
                            " (and " ++ show (lengthFL ps) ++ " more)"
           auto_subject _ = error "Tried to get a name from empty patch list."
           trim st n = if length st <= n then st
                       else take (n-3) st ++ "..."
           in do
           thetargets <- getTargets wtds
           from <- getAuthor (author ? opts) False
           let thesubject = fromMaybe (auto_subject to_be_sent) $ getSubject opts
           (mailcontents, mailfile, mailcharset) <- getDescription opts their_name to_be_sent

           let warnMailBody = case mailfile of
                                  Just mf -> putDocLn $ emailBackedUp mf
                                  Nothing -> return ()

               warnCharset msg = do
                 confirmed <- promptYorn $ promptCharSetWarning msg
                 unless confirmed $ do
                    putDocLn charsetAborted
                    warnMailBody
                    exitSuccess

           thecharset <- case charset ? opts of
                              -- Always trust provided charset
                              providedCset@(Just _) -> return providedCset
                              Nothing ->
                                case mailcharset of
                                Nothing -> do
                                  warnCharset charsetCouldNotGuess
                                  return mailcharset
                                Just "utf-8" -> do
                                  -- Check the locale encoding for consistency
                                  encoding <- getSystemEncoding
                                  debugMessage $ currentEncodingIs encoding
                                  unless (isUTF8Locale encoding) $
                                    warnCharset charsetUtf8MailDiffLocale
                                  return mailcharset
                                -- Trust other cases (us-ascii)
                                Just _ -> return mailcharset

           let body = makeEmail their_name
                        (maybe [] (\x -> [("In-Reply-To", x), ("References", x)]) . getInReplyTo $ opts)
                        (Just mailcontents)
                        thecharset
                        bundle
                        (Just fname)
               contentAndBundle = Just (mailcontents, bundle)

               sendmail =
                (do
                 let to = generateEmailToString thetargets
                 sm_cmd <- getSendmailCmd opts
                 sendEmailDoc from to thesubject (getCc opts)
                               sm_cmd contentAndBundle body
                 putInfo opts (success to (getCc opts)))
                 `onException` warnMailBody

           when (null [ p | Post p <- thetargets]) sendmail
           nbody <- withOpenTemp $ \ (fh,fn) -> do
               let to = generateEmailToString thetargets
               generateEmail fh from to thesubject (getCc opts) body
               hClose fh
               mmapFilePS fn
           forM_ [ p | Post p <- thetargets]
             (\url -> do
                putInfo opts $ postingPatch url
                postUrl url nbody "message/rfc822")
             `catch` (\(_ :: IOException) -> sendmail)
           cleanup opts mailfile

generateEmailToString :: [WhatToDo] -> String
generateEmailToString = intercalate " , " . filter (/= "") . map extractEmail
  where
    extractEmail (SendMail t) = t
    extractEmail _ = ""

cleanup :: (FilePathLike t) => [DarcsFlag] -> Maybe t -> IO ()
cleanup opts (Just mailfile) = when (isNothing (hasLogfile opts) || willRemoveLogFile opts) $
                                      removeFileMayNotExist mailfile
cleanup _ Nothing = return ()

writeBundleToFile :: forall rt p wX wY . (RepoPatch p, ApplyState p ~ Tree)
                  => [DarcsFlag] -> FL (PatchInfoAnd rt p) wX wY -> Doc ->
                    AbsolutePathOrStd -> [WhatToDo] -> String -> IO ()
writeBundleToFile opts to_be_sent bundle fname wtds their_name =
    do (d,f,_) <- getDescription opts their_name to_be_sent
       let putabs a = do writeDocBinFile a (d $$ bundle)
                         putDocLn (wroteBundle a)
           putstd = putDoc (d $$ bundle)
       useAbsoluteOrStd putabs putstd fname
       let to = generateEmailToString wtds
       unless (null to) $ putInfo opts $ savedButNotSent to
       cleanup opts f

data WhatToDo
    = Post String        -- ^ POST the patch via HTTP
    | SendMail String    -- ^ send patch via email

decideOnBehavior :: [DarcsFlag] -> Maybe (Repository rt p wR wU wT) -> IO [WhatToDo]
decideOnBehavior opts remote_repo =
    case the_targets of
    [] -> do wtds <- case remote_repo of
                     Nothing -> return []
                     Just r -> check_post r
             unless (null wtds) $ announce_recipients wtds
             return wtds
    ts -> do announce_recipients ts
             return ts
    where the_targets = collectTargets opts
          check_post the_remote_repo =
                       do p <- ((readPost . BC.unpack) `fmap`
                                fetchFilePS (prefsUrl (repoLocation the_remote_repo) ++ "/post")
                                (MaxAge 600)) `catchall` return []
                          emails <- who_to_email the_remote_repo
                          return (p++emails)
          readPost = map parseLine . lines where
              parseLine t = maybe (Post t) SendMail $ stripPrefix "mailto:" t
          who_to_email repo =
              do email <- (BC.unpack `fmap`
                           fetchFilePS (prefsUrl (repoLocation repo) ++ "/email")
                                       (MaxAge 600))
                          `catchall` return ""
                 if '@' `elem` email then return . map SendMail $ lines email
                                     else return []
          announce_recipients emails =
            let pn (SendMail s) = s
                pn (Post p) = p
                msg = willSendTo (dryRun ? opts) (map pn emails)
            in case dryRun ? opts of
                O.YesDryRun -> putInfo opts msg
                O.NoDryRun  -> when (null the_targets && isNothing (getOutput opts "")) $
                                putInfo opts msg

getTargets :: [WhatToDo] -> IO [WhatToDo]
getTargets [] = fmap ((:[]) . SendMail) $ askUser promptTarget
getTargets wtds = return wtds

collectTargets :: [DarcsFlag] -> [WhatToDo]
collectTargets flags = [ f t | t <- O._to (O.headerFields ? flags) ] where
    f url | "http:" `isPrefixOf` url = Post url
    f em = SendMail em

getDescription :: RepoPatch p
               => [DarcsFlag] -> String -> FL (PatchInfoAnd rt p) wX wY -> IO (Doc, Maybe String, Maybe String)
getDescription opts their_name patches =
    case get_filename of
        Just file -> do
                     when (editDescription ? opts) $ do
                       when (isNothing $ hasLogfile opts) $
                            writeDocBinFile file patchdesc
                       debugMessage $ aboutToEdit file
                       (_, changed) <- editFile file
                       unless changed $ do
                         confirmed <- promptYorn promptNoDescriptionChange
                         unless confirmed $ do putDocLn aborted
                                               exitSuccess
                       return ()
                     
                     updatedFile <- updateFilename file
                     doc <- readDocBinFile updatedFile
                     
                     return (doc, Just updatedFile, tryGetCharset doc)
        Nothing -> return (patchdesc, Nothing, tryGetCharset patchdesc)
    where patchdesc = text (show len)
                      <+> text (englishNum len (Noun "patch") "")
                      <+> text "for repository" <+> text their_name <> text ":"
                      $$ text ""
                      $$ vsep (mapFL description patches)
            where
              len = lengthFL patches
          updateFilename file = 
                maybe (renameFile file darcsSendMessageFinal >>
                       return darcsSendMessageFinal) (return . toFilePath) $ hasLogfile opts
          get_filename = case hasLogfile opts of
                                Just f -> Just $ toFilePath f
                                Nothing -> if editDescription ? opts
                                              then Just darcsSendMessage
                                              else Nothing
          tryGetCharset content = let body = renderPS content in
                                  if isAscii body
                                  then Just "us-ascii"
                                  else either (const Nothing)
                                              (const $ Just "utf-8")
                                              (decodeUtf8' body)

cmdDescription :: String
cmdDescription =
    "Prepare a bundle of patches to be applied to some target repository."

cmdHelp :: Doc
cmdHelp = vsep $
  map formatWords
  [ [ "Send is used to prepare a bundle of patches that can be applied to a target"
    , "repository.  Send accepts the URL of the repository as an argument.  When"
    , "called without an argument, send will use the most recent repository that"
    , "was either pushed to, pulled from or sent to.  By default, the patch bundle"
    , "is saved to a file, although you may directly send it by mail."
    ]
  , [ "The `--output`, `--output-auto-name`, and `--to` flags determine"
    , "what darcs does with the patch bundle after creating it.  If you provide an"
    , "`--output` argument, the patch bundle is saved to that file.  If you"
    , "specify `--output-auto-name`, the patch bundle is saved to a file with an"
    , "automatically generated name.  If you give one or more `--to` arguments,"
    , "the bundle of patches is sent to those locations. The locations may either"
    , "be email addresses or urls that the patch should be submitted to via HTTP."
    ]
  , [ "If you provide the `--mail` flag, darcs will look at the contents"
    , "of the `_darcs/prefs/email` file in the target repository (if it exists),"
    , "and send the patch by email to that address.  In this case, you may use"
    , "the `--cc` option to specify additional recipients without overriding the"
    , "default repository email address."
    ]
  , [ "If `_darcs/prefs/post` exists in the target repository, darcs will"
    , "upload to the URL contained in that file, which may either be a"
    , "`mailto:` URL, or an `http://` URL.  In the latter case, the"
    , "patch is posted to that URL."
    ]
  , [ "If there is no email address associated with the repository, darcs will"
    , "prompt you for an email address."
    ]
  , [ "Use the `--subject` flag to set the subject of the e-mail to be sent."
    , "If you don't provide a subject on the command line, darcs will make one up"
    , "based on names of the patches in the patch bundle."
    ]
  , [ "Use the `--in-reply-to` flag to set the In-Reply-To and References headers"
    , "of the e-mail to be sent. By default no additional headers are included so"
    , "e-mail will not be treated as reply by mail readers."
    ]
  , [ "If you want to include a description or explanation along with the bundle"
    , "of patches, you need to specify the `--edit-description` flag, which"
    , "will cause darcs to open up an editor with which you can compose a message"
    , "to go along with your patches."
    ]
  , [ "If you want to use a command different from the default one for sending"
    , "email, you need to specify a command line with the `--sendmail-command`"
    , "option. The command line can contain some format specifiers which are"
    , "replaced by the actual values. Accepted format specifiers are `%s` for"
    , "subject, `%t` for to, `%c` for cc, `%b` for the body of the mail, `%f` for"
    , "from, `%a` for the patch bundle and the same specifiers in uppercase for the"
    , "URL-encoded values."
    , "Additionally you can add `%<` to the end of the command line if the command"
    , "expects the complete email message on standard input. E.g. the command lines"
    , "for evolution and msmtp look like this:"
    ]
  ]
  ++
  -- TODO autoformatting for indented paragraphs
  [ vcat
    [ "    evolution \"mailto:%T?subject=%S&attach=%A&cc=%C&body=%B\""
    , "    msmtp -t %<"
    ]
  ]
  ++ map formatWords
  [ [ "Do not confuse the `--author` options with the return address"
    , "that `darcs send` will set for your patch bundle."
    ]
  , [ "For example, if you have two email addresses A and B:"
    ]
  ]
  ++
  -- TODO autoformatting for bullet lists
  [ vcat
    [ "  * If you use `--author A` but your machine is configured to send"
    , "    mail from address B by default, then the return address on your"
    , "    message will be B."
    , "  * If you use `--from A` and your mail client supports setting the"
    , "    From: address arbitrarily (some non-Unix-like mail clients,"
    , "    especially, may not support this), then the return address will"
    , "    be A; if it does not support this, then the return address will"
    , "    be B."
    , "  * If you supply neither `--from` nor `--author` then the return"
    , "    address will be B."
    ]
  ]
  ++
  [ formatWords
    [ "In addition, unless you specify the sendmail command with"
    , "`--sendmail-command`, darcs sends email using the default email"
    , "command on your computer. This default command is determined by the"
    , "`configure` script. Thus, on some non-Unix-like OSes,"
    , "`--from` is likely to not work at all."
    ]
  , otherHelpInheritDefault
  ]

cannotSendToSelf :: String
cannotSendToSelf = "Can't send to current repository! Did you mean send --context?"

creatingPatch :: String -> Doc
creatingPatch repodir = "Creating patch to" <+> quoted repodir <> "..."

nothingSendable :: Doc
nothingSendable = "No recorded local changes to send!"

selectionIs :: [Doc] -> Doc
selectionIs descs = text "We have the following patches to send:" $$ vcat descs

selectionIsNull :: Doc
selectionIsNull = text "You don't want to send any patches, and that's fine with me!"

emailBackedUp :: String -> Doc
emailBackedUp mf = sentence $ "Email body left in" <+> text mf <> "."

promptCharSetWarning :: String -> String
promptCharSetWarning msg = "Warning: " ++ msg ++ "  Send anyway?"

charsetAborted :: Doc
charsetAborted = "Aborted.  You can specify charset with the --charset option."

charsetCouldNotGuess :: String
charsetCouldNotGuess = "darcs could not guess the charset of your mail."

currentEncodingIs :: String -> String
currentEncodingIs e = "Current locale encoding: " ++ e

charsetUtf8MailDiffLocale :: String
charsetUtf8MailDiffLocale = "your mail is valid UTF-8 but your locale differs."

aborted :: Doc
aborted = "Aborted."

success :: String -> String -> Doc
success to cc = sentence $
    "Successfully sent patch bundle to:" <+> text to <+> copies cc
  where
    copies "" = ""
    copies x  = "and cc'ed" <+> text x

postingPatch :: String -> Doc
postingPatch url = "Posting patch to" <+> text url

wroteBundle :: FilePathLike a => a -> Doc
wroteBundle a = sentence $ "Wrote patch to" <+> text (toFilePath a)

savedButNotSent :: String -> Doc
savedButNotSent to =
        text ("The usual recipent for this bundle is: " ++ to)
    $$  text "To send it automatically, make sure sendmail is working,"
    <+> text "and add 'send mail' to _darcs/prefs/defaults or"
    <+> text " ~/.darcs/defaults"

willSendTo :: DryRun -> [String] -> Doc
willSendTo dr addresses =
    "Patch bundle" <+> will <+> " be sent to:" <+> text (unwords addresses)
  where
    will = case dr of { YesDryRun -> "would"; NoDryRun  -> "will" }

promptTarget :: String
promptTarget = "What is the target email address? "

aboutToEdit :: FilePath -> String
aboutToEdit file = "About to edit file " ++ file

promptNoDescriptionChange :: String
promptNoDescriptionChange = "File content did not change. Continue anyway?"
