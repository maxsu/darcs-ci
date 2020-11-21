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

module Darcs.UI.Commands.Clone
    ( get
    , put
    , clone
    , makeRepoName
    , cloneToSSH
    , otherHelpInheritDefault
    ) where

import Darcs.Prelude

import System.Directory ( doesDirectoryExist, doesFileExist
                        , setCurrentDirectory )
import System.Exit ( ExitCode(..) )
import Control.Monad ( when, unless )

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts
                      , nodefaults
                      , commandStub
                      , commandAlias
                      , putInfo
                      , putFinished
                      )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , cloneKind
    , patchIndexNo
    , quiet
    , remoteDarcs
    , remoteRepos
    , setDefault
    , setScriptsExecutable
    , umask
    , useCache
    , usePacks
    , verbosity
    , withNewRepo
    , withWorkingDir
    )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands.Util ( getUniqueRepositoryName )
import Darcs.Patch.Match ( MatchFlag(..) )
import Darcs.Repository ( cloneRepository )
import Darcs.Repository.Format ( identifyRepoFormat
                               , RepoProperty ( HashedInventory
                                              , RebaseInProgress
                                              )
                               , formatHas
                               )
import Darcs.Util.Lock ( withTempDir )
import Darcs.Util.Ssh ( getSSH, SSHCmd(SCP,SSH) )
import Darcs.Repository.Flags
    ( CloneKind(CompleteClone), SetDefault(NoSetDefault), ForgetParent(..) )
import Darcs.Repository.Prefs ( showMotd )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Printer ( Doc, formatWords, formatText, text, vsep, ($$), ($+$) )
import Darcs.Util.Path ( toPath, ioAbsoluteOrRemote, AbsolutePath )
import Darcs.Util.Workaround ( getCurrentDirectory )
import Darcs.Util.URL ( SshFilePath(..), isSshUrl, splitSshUrl )
import Darcs.Util.Exec ( exec, Redirect(..), )

cloneDescription :: String
cloneDescription = "Make a copy of an existing repository."

cloneHelp :: Doc
cloneHelp = vsep $
  map formatWords
  [ [ "Clone creates a copy of a repository.  The optional second"
    , "argument specifies a destination directory for the new copy;"
    , "if omitted, it is inferred from the source location."
    ]
  , [ "By default Darcs will copy every patch from the original repository."
    , "If you expect the original repository to remain accessible, you can"
    , "use `--lazy` to avoid copying patches until they are needed ('copy on"
    , "demand').  This is particularly useful when copying a remote"
    , "repository with a long history that you don't care about."
    ]
  , [ "When cloning locally, Darcs automatically uses hard linking where"
    , "possible.  As well as saving time and space, this enables to move or"
    , "delete the original repository without affecting the copy."
    , "Hard linking requires that the copy be on the same filesystem as the"
    , "original repository, and that the filesystem support hard linking."
    , "This includes NTFS, HFS+ and all general-purpose Unix filesystems"
    , "(such as ext, UFS and ZFS). FAT does not support hard links."
    ]
  , [ "When cloning from a remote location, Darcs will look for and attempt"
    , "to use packs created by `darcs optimize http` in the remote repository."
    , "Packs are single big files that can be downloaded faster than many"
    , "little files."
    ]
  , [ "Darcs clone will not copy unrecorded changes to the source repository's"
    , "working tree."
    ]
  , [ "You can copy a repository to a ssh url, in which case the new repository"
    , "will always be complete."
    ]
  ]
  ++
  [ cloneHelpTag
  , cloneHelpSSE
  , cloneHelpInheritDefault
  ]

clone :: DarcsCommand
clone = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "clone"
    , commandHelp = cloneHelp
    , commandDescription = cloneDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<REPOSITORY>", "[<DIRECTORY>]"]
    , commandCommand = cloneCmd
    , commandPrereq = \_ -> return $ Right ()
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc cloneAdvancedOpts
    , commandBasicOptions = odesc cloneBasicOpts
    , commandDefaults = defaultFlags cloneOpts
    , commandCheckOptions = ocheck cloneOpts
    }
  where
    cloneBasicOpts
      = O.newRepo
      ^ O.cloneKind
      ^ O.matchOneContext
      ^ O.setDefault
      ^ O.inheritDefault
      ^ O.setScriptsExecutable
      ^ O.withWorkingDir
    cloneAdvancedOpts = O.usePacks ^ O.patchIndexNo ^ O.umask ^ O.network
    cloneOpts = cloneBasicOpts `withStdOpts` cloneAdvancedOpts

get :: DarcsCommand
get = commandAlias "get" Nothing clone

putDescription :: String
putDescription = "Deprecated command, replaced by clone."

putHelp :: Doc
putHelp = formatText 80
 [ "This command is deprecated."
 , "To clone the current repository to a ssh destination, " ++
   "use the syntax `darcs clone . user@server:path` ."
 ]

put :: DarcsCommand
put = commandStub "put" putHelp putDescription clone

cloneCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
cloneCmd fps opts [inrepodir, outname] = cloneCmd fps (withNewRepo outname opts) [inrepodir]
cloneCmd _ opts [inrepodir] = do
  debugMessage "Starting work on clone..."
  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir
  unless (quiet opts) $ showMotd repodir
  rfsource <- identifyRepoFormat repodir
  debugMessage $ "Found the format of "++repodir++"..."

  -- This merely forbids clone from an old-style rebase in progress, which is
  -- exactly what we want. Transferring patches from repos with new-style
  -- rebase in progress is unproblematic and fully supported.
  when (formatHas RebaseInProgress rfsource) $
      fail "Cannot clone a repository with an old-style rebase in progress"

  unless (formatHas HashedInventory rfsource) $ putInfo opts $
       text "***********************************************************************"
    $$ text "  _______   Sorry for the wait! The repository you are cloning is"
    $$ text " |       |  using the DEPRECATED 'old-fashioned' format. I'm doing a"
    $$ text " | O   O |  hashed copy instead, but this may take a while."
    $$ text " |  ___  |"
    $$ text " | /   \\ |  We recommend that the maintainer upgrade the remote copy"
    $$ text " |_______|  as well. See http://wiki.darcs.net/OF for more information."
    $$ text ""
    $$ text "***********************************************************************"

  case cloneToSSH opts of
    Just repo -> do
      withTempDir "clone" $ \_ -> do
         guardRemoteDirDoesNotExist repo
         putInfo opts $ text "Creating local clone..."
         currentDir <- getCurrentDirectory
         mysimplename <- makeRepoName True [] repodir -- give correct name to local clone
         cloneRepository repodir mysimplename (verbosity ? opts) (useCache ? opts)
                         CompleteClone (umask ? opts) (remoteDarcs opts)
                         (setScriptsExecutable ? opts)
                         (remoteRepos ? opts) (NoSetDefault True)
                         O.NoInheritDefault -- never inherit defaultrepo when cloning to ssh
                         (map convertUpToToOne (O.matchOneContext ? opts))
                         rfsource
                         (withWorkingDir ? opts)
                         (patchIndexNo ? opts)
                         (usePacks ? opts)
                         YesForgetParent
         setCurrentDirectory currentDir
         (scp, args) <- getSSH SCP
         putInfo opts $ text $ "Transferring clone using " ++ scp ++ "..."
         r <- exec scp (args ++ ["-r", mysimplename ++ "/", repo]) (AsIs,AsIs,AsIs)
         when (r /= ExitSuccess) $ fail $ "Problem during " ++ scp ++ " transfer."
         putInfo opts $ text "Cloning and transferring successful."
    Nothing -> do
      mysimplename <- makeRepoName True opts repodir
      cloneRepository repodir mysimplename (verbosity ? opts) (useCache ? opts)
                  (cloneKind ? opts) (umask ? opts) (remoteDarcs opts)
                  (setScriptsExecutable ? opts)
                  (remoteRepos ? opts) (setDefault True opts)
                  (O.inheritDefault ? opts)
                  (map convertUpToToOne (O.matchOneContext ? opts))
                  rfsource
                  (withWorkingDir ? opts)
                  (patchIndexNo ? opts)
                  (usePacks ? opts)
                  NoForgetParent
      putFinished opts "cloning"

cloneCmd _ _ _ = fail "You must provide 'clone' with either one or two arguments."

cloneToSSH :: [DarcsFlag] -> Maybe String
cloneToSSH fs = case O.newRepo ? fs of
  Nothing -> Nothing
  Just r -> if isSshUrl r then Just r else Nothing

-- | Make sure we do not overwrite an existing remote directory.
guardRemoteDirDoesNotExist :: String -> IO ()
guardRemoteDirDoesNotExist rpath = do
  (ssh, ssh_args) <- getSSH SSH
  let sshfp = splitSshUrl rpath
  let ssh_cmd = "mkdir '" ++ sshRepo sshfp ++ "'"
  r <- exec ssh (ssh_args ++ [sshUhost sshfp, ssh_cmd]) (AsIs,AsIs,AsIs)
  when (r /= ExitSuccess) $
    fail $ "Cannot create remote directory '"  ++ sshRepo sshfp ++ "'."

makeRepoName :: Bool -> [DarcsFlag] -> FilePath -> IO String
makeRepoName talkative fs d =
  case O.newRepo ? fs of
    Just n -> do
      exists <- doesDirectoryExist n
      file_exists <- doesFileExist n
      if exists || file_exists
        then fail $ "Directory or file named '" ++ n ++ "' already exists."
        else return n
    Nothing ->
      case mkName d of
        "" -> getUniqueRepositoryName talkative "anonymous_repo"
        base@('/':_) -> getUniqueRepositoryName talkative base -- Absolute
        base -- Relative
         -> do
          cwd <- getCurrentDirectory
          getUniqueRepositoryName talkative (cwd ++ "/" ++ base)
      where mkName = dropWhile (== '.') . reverse .
              takeWhile (not . (`elem` "/:")) . dropWhile (== '/') . reverse

cloneHelpTag :: Doc
cloneHelpTag = formatWords
  [ ""
  , "It is often desirable to make a copy of a repository that excludes"
  , "some patches.  For example, if releases are tagged then `darcs clone"
  , "--tag .` would make a copy of the repository as at the latest release."
  , ""
  , "An untagged repository state can still be identified unambiguously by"
  , "a context file, as generated by `darcs log --context`.  Given the"
  , "name of such a file, the `--context` option will create a repository"
  , "that includes only the patches from that context.  When a user reports"
  , "a bug in an unreleased version of your project, the recommended way to"
  , "find out exactly what version they were running is to have them" 
  , "include a context file in the bug report."
  , ""
  , "You can also make a copy of an untagged state using the `--to-patch` or"
  , "`--to-match` options, which exclude patches *after* the first matching"
  , "patch.  Because these options treat the set of patches as an ordered"
  , "sequence, you may get different results after reordering with `darcs"
  , "optimize reorder`."
  ]

cloneHelpSSE :: Doc
cloneHelpSSE = formatWords
  [ "The `--set-scripts-executable` option causes scripts to be made"
  , "executable in the working tree. A script is any file that starts"
  , "with a shebang (\"#!\")."
  ]

cloneHelpInheritDefault :: Doc
cloneHelpInheritDefault = commonHelpInheritDefault $+$ formatWords
  [ "For the clone command it means the following:"
  , "If the source repository already has a defaultrepo set (either because"
  , "you cloned it or because you explicitly used the --set-default option),"
  , "and both source and target are locally valid paths on the same host,"
  , "then the target repo will get the same defaultrepo as the source repo."
  , "Otherwise the target repo gets the source repo itself as defaultrepo,"
  , "i.e. we fall back to the defalt behavior (--no-inherit-default)."
  ]

otherHelpInheritDefault :: Doc
otherHelpInheritDefault = commonHelpInheritDefault $+$ formatWords
  [ "For the commands push, pull, and send it means the following:"
  , "Changes the meaning of the --set-default option so that it sets the"
  , "(local) defaultrepo to the defaultrepo of the remote repo, instead of"
  , "the remote repo itself. This happens only if the remote repo does have"
  , "a defaultrepo set and both local and remote repositories are locally"
  , "valid paths on the same host, otherwise fall back to the default behavior"
  , "(--no-inherit-default)."
  ]

commonHelpInheritDefault :: Doc
commonHelpInheritDefault = formatWords
  [ "The --inherit-default option is meant to support a work flow where"
  , "you have different branches of the same upstream repository and want"
  , "all your branches to have the same upstream repo as the defaultrepo."
  , "It is most useful when enabled globally by adding 'ALL --inherit-default'"
  , "to your ~/darcs/defaults file."
  ]

-- | The 'clone' command takes --to-patch and --to-match as arguments,
-- but internally wants to handle them as if they were --patch and --match.
-- This function does the conversion.
convertUpToToOne :: MatchFlag -> MatchFlag
convertUpToToOne (UpToPattern p) = OnePattern p
convertUpToToOne (UpToPatch p) = OnePatch p
convertUpToToOne (UpToHash p) = OneHash p
convertUpToToOne f = f
