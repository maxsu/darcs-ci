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

module Darcs.UI.Commands.Convert.Darcs2 ( convertDarcs2 ) where

import Control.Monad ( when, unless )
import qualified Data.ByteString as B
import Data.Char ( toLower )
import Data.Maybe ( catMaybes )
import Data.List ( lookup )
import System.FilePath.Posix ( (</>) )
import System.Directory ( doesDirectoryExist, doesFileExist )

import Darcs.Prelude

import Darcs.Patch ( RepoPatch, effect, displayPatch )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( isTag, piRename, piTag )
import Darcs.Patch.Named ( Named(..), getdeps, patch2patchinfo, patchcontents )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info, n2pia )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.RepoType ( IsRepoType(..), RebaseType(..), RepoType(..) )
import Darcs.Patch.Set ( inOrderTags, patchSet2FL, patchSet2RL )
import qualified Darcs.Patch.V1 as V1 ( RepoPatchV1 )
import Darcs.Patch.V1.Commute ( publicUnravel )
import qualified Darcs.Patch.V1.Core as V1 ( RepoPatchV1(PP), isMerger )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim(..) )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim(..) )
import qualified Darcs.Patch.V2.RepoPatch as V2 ( RepoPatchV2(Normal) )
import Darcs.Patch.V2.RepoPatch ( mergeUnravelled )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..), (=/\=) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , bunchFL
    , concatFL
    , foldFL_M
    , mapFL_FL
    , mapRL
    )
import Darcs.Patch.Witnesses.Sealed ( FlippedSeal(..), mapSeal )

import Darcs.Repository
    ( RepoJob(..)
    , Repository
    , applyToWorking
    , createRepositoryV2
    , finalizeRepositoryChanges
    , invalidateIndex
    , readRepo
    , revertRepositoryChanges
    , withRepositoryLocation
    , withUMaskFlag
    )
import qualified Darcs.Repository as R ( setScriptsExecutable )
import Darcs.Repository.Flags ( Compression(..), UpdatePending(..) )
import Darcs.Repository.Format
    ( RepoProperty(Darcs2)
    , formatHas
    , identifyRepoFormat
    )
import Darcs.Repository.Hashed ( UpdatePristine(..), tentativelyAddPatch_ )
import Darcs.Repository.Prefs ( showMotd, prefsFilePath )

import Darcs.UI.Commands ( DarcsCommand(..), nodefaults, putFinished, withStdOpts )
import Darcs.UI.Commands.Convert.Util ( updatePending )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( verbosity, useCache, umask, withWorkingDir, patchIndexNo
    , DarcsFlag, withNewRepo
    , quiet
    )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Util.External ( fetchFilePS, Cachable(Uncachable) )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Lock ( withNewDirectory )
import Darcs.Util.Path( ioAbsoluteOrRemote, toPath, AbsolutePath )
import Darcs.Util.Printer ( Doc, text, ($$) )
import Darcs.Util.Printer.Color ( traceDoc )
import Darcs.Util.Prompt ( askUser )
import Darcs.Util.Tree( Tree )
import Darcs.Util.Workaround ( getCurrentDirectory )

type RepoPatchV1 = V1.RepoPatchV1 V1.Prim
type RepoPatchV2 = V2.RepoPatchV2 V2.Prim

convertDarcs2Help :: Doc
convertDarcs2Help = text $ unlines
 [ "This command converts a repository that uses the old patch semantics"
 , "`darcs-1` to a new repository with current `darcs-2` semantics."
 , ""
 , convertDarcs2Help'
 ]

-- | This part of the help is split out because it is used twice: in
-- the help string, and in the prompt for confirmation.
convertDarcs2Help' :: String
convertDarcs2Help' = unlines
 [ "WARNING: the repository produced by this command is not understood by"
 , "Darcs 1.x, and patches cannot be exchanged between repositories in"
 , "darcs-1 and darcs-2 formats. Also, you should not exchange patches"
 , "between repositories created by different invocations of this command."
 , "This means:"
 , "- Before doing this conversion, you should merge into this repo any patches"
 , "  existing elsewhere that you might want to merge in future, so that they"
 , "  will remain mergeable. (You can always remove them again after converting)."
 , "- After converting, you should tell everyone with a fork of this repo"
 , "  to discard it and make a new fork of the converted repo."
 ]

convertDarcs2 :: DarcsCommand
convertDarcs2 = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "darcs-2"
    , commandHelp = convertDarcs2Help
    , commandDescription = "Convert darcs-1 repository to the darcs-2 patch format"
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["<SOURCE>", "[<DESTINATION>]"]
    , commandCommand = toDarcs2
    , commandPrereq = \_ -> return $ Right ()
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc convertDarcs2AdvancedOpts
    , commandBasicOptions = odesc convertDarcs2BasicOpts
    , commandDefaults = defaultFlags (convertDarcs2Opts ^ convertDarcs2SilentOpts)
    , commandCheckOptions = ocheck convertDarcs2Opts
    }
  where
    convertDarcs2BasicOpts = O.newRepo ^ O.setScriptsExecutable ^ O.withWorkingDir
    convertDarcs2AdvancedOpts = O.network ^ O.patchIndexNo ^ O.umask
    convertDarcs2Opts = convertDarcs2BasicOpts `withStdOpts` convertDarcs2AdvancedOpts
    convertDarcs2SilentOpts = O.patchFormat

toDarcs2 :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
toDarcs2 _ opts' args = do
  (inrepodir, opts) <-
    case args of
      [arg1, arg2] -> return (arg1, withNewRepo arg2 opts')
      [arg1] -> return (arg1, opts')
      _ -> fail "You must provide either one or two arguments."
  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir

  format <- identifyRepoFormat repodir
  when (formatHas Darcs2 format) $ fail "Repository is already in darcs 2 format."

  putStrLn convertDarcs2Help'
  answer <- askUser ("Do you still want to proceed ? If so, please type \"yes\": ")
  when (map toLower answer /= "yes") $ fail "Ok, doing nothing."

  unless (quiet opts) $ showMotd repodir

  mysimplename <- makeRepoName opts repodir
  withUMaskFlag (umask ? opts) $ withNewDirectory mysimplename $ do
    _repo <- createRepositoryV2
      (withWorkingDir ? opts) (patchIndexNo ? opts) (O.useCache ? opts)
    _repo <- revertRepositoryChanges _repo NoUpdatePending

    withRepositoryLocation (useCache ? opts) repodir $ V1Job $ \other -> do
      theirstuff <- readRepo other
      let patches = mapFL_FL (convertNamed . hopefully) $ patchSet2FL theirstuff
          outOfOrderTags = catMaybes $ mapRL oot $ patchSet2RL theirstuff
              where oot t = if isTag (info t) && info t `notElem` inOrderTags theirstuff
                            then Just (info t, getdeps $ hopefully t)
                            else Nothing
          fixDep p = case lookup p outOfOrderTags of
                     Just d -> p : concatMap fixDep d
                     Nothing -> [p]
          primV1toV2 = V2.Prim . V1.unPrim
          convertOne :: RepoPatchV1 wX wY -> FL RepoPatchV2 wX wY
          convertOne x | V1.isMerger x =
            let ex = mapFL_FL primV1toV2 (effect x) in
            case mergeUnravelled $ map (mapSeal (mapFL_FL primV1toV2)) $ publicUnravel x of
             Just (FlippedSeal y) ->
                 case effect y =/\= ex of
                 IsEq -> y :>: NilFL
                 NotEq ->
                     traceDoc (text "lossy conversion:" $$
                               displayPatch x) $
                     mapFL_FL V2.Normal ex
             Nothing -> traceDoc (text
                                  "lossy conversion of complicated conflict:" $$
                                  displayPatch x) $
                        mapFL_FL V2.Normal ex
          convertOne (V1.PP x) = V2.Normal (primV1toV2 x) :>: NilFL
          convertOne _ = error "impossible case"
          convertFL :: FL RepoPatchV1 wX wY -> FL RepoPatchV2 wX wY
          convertFL = concatFL . mapFL_FL convertOne
          convertNamed :: Named RepoPatchV1 wX wY
                       -> PatchInfoAnd ('RepoType 'NoRebase) RepoPatchV2 wX wY
          convertNamed n = n2pia $
                           NamedP
                            (convertInfo $ patch2patchinfo n)
                            (map convertInfo $ concatMap fixDep $ getdeps n)
                            (convertFL $ patchcontents n)
          convertInfo n | n `elem` inOrderTags theirstuff = n
                        | otherwise = maybe n (\t -> piRename n ("old tag: "++t)) $ piTag n

      -- Note: we use bunchFL so we can commit every 100 patches
      _ <- applyAll opts _repo $ bunchFL 100 $ progressFL "Converting patch" patches
      when (parseFlags O.setScriptsExecutable opts == O.YesSetScriptsExecutable)
        R.setScriptsExecutable

      -- Copy over the prefs file
      (fetchFilePS (repodir </> prefsFilePath) Uncachable >>= B.writeFile prefsFilePath)
       `catchall` return ()

      putFinished opts "converting"
  where
    applyOne :: (RepoPatch p, ApplyState p ~ Tree)
             => [DarcsFlag]
             -> W2 (Repository rt p wR) wX
             -> PatchInfoAnd rt p wX wY
             -> IO (W2 (Repository rt p wR) wY)
    applyOne opts (W2 _repo) x = do
      _repo <- tentativelyAddPatch_ (updatePristine opts) _repo
        GzipCompression (verbosity ? opts) (updatePending opts) x
      _repo <- applyToWorking _repo (verbosity ? opts) (effect x)
      invalidateIndex _repo
      return (W2 _repo)

    applySome opts (W3 _repo) xs = do
      _repo <- unW2 <$> foldFL_M (applyOne opts) (W2 _repo) xs
      -- commit after applying a bunch of patches
      _repo <- finalizeRepositoryChanges _repo (updatePending opts) GzipCompression
      _repo <- revertRepositoryChanges _repo (updatePending opts)
      return (W3 _repo)

    applyAll :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
             => [DarcsFlag]
             -> Repository rt p wX wX wX
             -> FL (FL (PatchInfoAnd rt p)) wX wY
             -> IO (Repository rt p wY wY wY)
    applyAll opts r xss = unW3 <$> foldFL_M (applySome opts) (W3 r) xss

    updatePristine :: [DarcsFlag] -> UpdatePristine
    updatePristine opts =
      case withWorkingDir ? opts of
        O.WithWorkingDir -> UpdatePristine
        -- this should not be necessary but currently is, because
        -- some commands (e.g. send) cannot cope with a missing pristine
        -- even if the repo is marked as having no working tree
        O.NoWorkingDir -> {- DontUpdatePristineNorRevert -}UpdatePristine

-- | Need this to make 'foldFL_M' work with a function that changes
-- the last two (identical) witnesses at the same time.
newtype W2 r wX = W2 {unW2 :: r wX wX}

-- | Similarly for when the function changes all three witnesses.
newtype W3 r wX = W3 {unW3 :: r wX wX wX}

makeRepoName :: [DarcsFlag] -> FilePath -> IO String
makeRepoName opts d =
  case O.newRepo ? opts of
    Just n -> do
      exists <- doesDirectoryExist n
      file_exists <- doesFileExist n
      if exists || file_exists
        then fail $ "Directory or file named '" ++ n ++ "' already exists."
        else return n
    Nothing ->
      case dropWhile (== '.') $
           reverse $
           takeWhile (\c -> c /= '/' && c /= ':') $
           dropWhile (== '/') $ reverse d of
        "" -> modifyRepoName "anonymous_repo"
        base -> modifyRepoName base

modifyRepoName :: String -> IO String
modifyRepoName name =
    if head name == '/'
    then mrn name (-1)
    else do cwd <- getCurrentDirectory
            mrn (cwd ++ "/" ++ name) (-1)
 where
  mrn :: String -> Int -> IO String
  mrn n i = do
    exists <- doesDirectoryExist thename
    file_exists <- doesFileExist thename
    if not exists && not file_exists
       then do when (i /= -1) $
                    putStrLn $ "Directory '"++ n ++
                               "' already exists, creating repository as '"++
                               thename ++"'"
               return thename
       else mrn n $ i+1
    where thename = if i == -1 then n else n++"_"++show i
