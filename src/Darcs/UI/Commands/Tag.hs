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

module Darcs.UI.Commands.Tag ( tag ) where

import Darcs.Prelude

import Control.Monad ( when )
import System.IO ( hPutStr, stderr )

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( patchinfo )
import Darcs.Patch.Depends ( getUncovered )
import Darcs.Patch
    ( PrimPatch, PrimOf
    , RepoPatch
    )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Patch.Named ( infopatch, adddeps )
import Darcs.Patch.Set
    ( emptyPatchSet, appendPSFL, patchSet2FL, patchSetTags )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )

import Darcs.Repository
    ( withRepoLock, Repository, RepoJob(..), readRepo
    , tentativelyAddPatch, finalizeRepositoryChanges,
    )
import Darcs.Repository.Flags ( UpdatePending(..), DryRun(NoDryRun) )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository, putFinished )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag, getDate, compress, verbosity, useCache, umask, getAuthor, author )
import Darcs.UI.Options
    ( (^), odesc, ocheck, defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PatchHeader ( getLog )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , selectionConfig
    , runSelection
    , SelectionConfig(allowSkipAll)
    )
import qualified Darcs.UI.SelectChanges as S

import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree( Tree )


tagDescription :: String
tagDescription = "Name the current repository state for future reference."

tagHelp :: Doc
tagHelp = text $
 "The `darcs tag` command names the current repository state, so that it\n" ++
 "can easily be referred to later.  Every *important* state should be\n" ++
 "tagged; in particular it is good practice to tag each stable release\n" ++
 "with a number or codename.  Advice on release numbering can be found\n" ++
 "at <http://producingoss.com/en/development-cycle.html>.\n" ++
 "\n" ++
 "To reproduce the state of a repository `R` as at tag `t`, use the\n" ++
 "command `darcs clone --tag t R`.  The command `darcs show tags` lists\n" ++
 "all tags in the current repository.\n" ++
 "\n" ++
 "Tagging also provides significant performance benefits: when Darcs\n" ++
 "reaches a shared tag that depends on all antecedent patches, it can\n" ++
 "simply stop processing.\n" ++
 "\n" ++
 "Like normal patches, a tag has a name, an author, a timestamp and an\n" ++
 "optional long description, but it does not change the working tree.\n" ++
 "A tag can have any name, but it is generally best to pick a naming\n" ++
 "scheme and stick to it.\n" ++
 "\n" ++
 "By default a tag names the entire repository state at the time the tag\n" ++
 "is created. If the --ask-deps option is used, the patches to include\n" ++
 "as part of the tag can be explicitly selected.\n" ++
 "\n" ++
 "The `darcs tag` command accepts the `--pipe` option, which behaves as\n" ++
 "described in `darcs record`.\n"

tag :: DarcsCommand
tag = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "tag"
    , commandHelp = tagHelp
    , commandDescription = tagDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[TAGNAME]"]
    , commandCommand = tagCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc tagAdvancedOpts
    , commandBasicOptions = odesc tagBasicOpts
    , commandDefaults = defaultFlags tagOpts
    , commandCheckOptions = ocheck tagOpts
    }
  where
    tagBasicOpts
      = O.patchname
      ^ O.author
      ^ O.pipe
      ^ O.askLongComment
      ^ O.askDeps
      ^ O.repoDir
    tagAdvancedOpts = O.compress ^ O.umask
    tagOpts = tagBasicOpts `withStdOpts` tagAdvancedOpts

tagCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
tagCmd _ opts args =
  withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $ RepoJob $ \(repository :: Repository rt p wR wU wR) -> do
    date <- getDate (hasPipe opts)
    the_author <- getAuthor (author ? opts) (hasPipe opts)
    patches <- readRepo repository
    tags <- return $ patchSetTags patches
    Sealed chosenPatches <-
        if O.askDeps ? opts
            then mapSeal (appendPSFL emptyPatchSet) <$> askAboutTagDepends opts (patchSet2FL patches)
            else return $ Sealed patches
    let deps = getUncovered chosenPatches
    (name, long_comment)  <- get_name_log (NilFL :: FL (PrimOf p) wA wA) args tags
    myinfo <- patchinfo date name the_author long_comment
    let mypatch = infopatch myinfo NilFL
    _ <- tentativelyAddPatch repository (compress ? opts) (verbosity ? opts) YesUpdatePending
             $ n2pia $ adddeps mypatch deps
    _ <- finalizeRepositoryChanges repository YesUpdatePending (compress ? opts)
    putFinished opts $ "tagging '"++name++"'"
  where  get_name_log ::(PrimPatch prim) => FL prim wA wA -> [String] -> [String] -> IO (String, [String])
         get_name_log nilFL a tags
                          = do (name, comment, _) <- getLog
                                  (case parseFlags O.patchname opts of
                                    Nothing -> Just (unwords a)
                                    Just s -> Just s)
                                  (hasPipe opts)
                                  (parseFlags O.logfile opts)
                                  (parseFlags O.askLongComment opts)
                                  Nothing nilFL
                               when (length name < 2) $ hPutStr stderr $
                                 "Do you really want to tag '"
                                 ++name++"'? If not type: darcs obliterate --last=1\n"
                               when (name `elem` tags) $
                                  putStrLn $ "WARNING: The tag "  ++ 
                                             "\"" ++ name ++ "\"" ++
                                             " already exists."
                               return ("TAG " ++ name, comment)

-- This may be useful for developers, but users don't care about
-- internals:
--
-- A tagged version automatically depends on all patches in the
-- repository.  This allows you to later reproduce precisely that
-- version.  The tag does this by depending on all patches in the
-- repository, except for those which are depended upon by other tags
-- already in the repository.  In the common case of a sequential
-- series of tags, this means that the tag depends on all patches
-- since the last tag, plus that tag itself.

askAboutTagDepends
     :: forall rt p wX wY . (RepoPatch p, ApplyState p ~ Tree)
     => [DarcsFlag]
     -> FL (PatchInfoAnd rt p) wX wY
     -> IO (Sealed (FL (PatchInfoAnd rt p) wX))
askAboutTagDepends flags ps = do
  let opts = S.PatchSelectionOptions
             { S.verbosity = verbosity ? flags
             , S.matchFlags = []
             , S.interactive = True
             , S.selectDeps = O.PromptDeps
             , S.withSummary = O.NoSummary
             , S.withContext = O.NoContext
             }
  (deps:>_) <- runSelection ps $
                     ((selectionConfig FirstReversed "depend on" opts Nothing Nothing)
                          { allowSkipAll = False })
  return $ Sealed deps

hasPipe :: [DarcsFlag] -> Bool
hasPipe = parseFlags O.pipe
