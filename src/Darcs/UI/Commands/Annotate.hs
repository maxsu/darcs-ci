--  Copyright (C) 2003 David Roundy, 2010-2011 Petr Rockai
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

module Darcs.UI.Commands.Annotate ( annotate ) where

import Darcs.Prelude

import Control.Monad ( when )

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.External ( viewDocWith )
import Darcs.UI.Flags ( DarcsFlag, useCache, patchIndexYes, pathsFromArgs )
import Darcs.UI.Options ( (^), odesc, ocheck
                        , defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O

import Darcs.Repository.State ( readRecorded )
import Darcs.Repository
    ( withRepository
    , withRepoLockCanFail
    , RepoJob(..)
    , readRepo
    )
import Darcs.Repository.PatchIndex ( attemptCreatePatchIndex )
import Darcs.Patch.Set ( patchSet2RL )
import qualified Data.ByteString.Char8 as BC ( pack, concat, intercalate )
import Data.ByteString.Lazy ( toChunks )
import Darcs.Patch.ApplyMonad( withFileNames )
import Darcs.Patch.Match ( patchSetMatch, rollbackToPatchSetMatch  )
import Darcs.Repository.Match ( getOnePatchset )
import Darcs.Repository.PatchIndex ( getRelevantSubsequence, canUsePatchIndex )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal )
import qualified Darcs.Patch.Annotate as A

import Darcs.Util.Tree( TreeItem(..) )
import qualified Darcs.Util.Tree as T ( readBlob, list, expand )
import Darcs.Util.Tree.Monad( findM, virtualTreeIO )
import Darcs.Util.Path( AbsolutePath, AnchoredPath, displayPath, catPaths )
import Darcs.Util.Printer ( Doc, simplePrinters, renderString, text )
import Darcs.Util.Exception ( die )

annotateDescription :: String
annotateDescription = "Annotate lines of a file with the last patch that modified it."

annotateHelp :: Doc
annotateHelp = text $ unlines
 [ "When `darcs annotate` is called on a file, it will find the patch that"
 , "last modified each line in that file. This also works on directories."
 , ""
 , "The `--machine-readable` option can be used to generate output for"
 , "machine postprocessing."
 ]

annotate :: DarcsCommand
annotate = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "annotate"
    , commandHelp = annotateHelp
    , commandDescription = annotateDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]"]
    , commandCommand = annotateCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = knownFileArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc annotateAdvancedOpts
    , commandBasicOptions = odesc annotateBasicOpts
    , commandDefaults = defaultFlags annotateOpts
    , commandCheckOptions = ocheck annotateOpts
    }
  where
    annotateBasicOpts = O.machineReadable ^ O.matchUpToOne ^ O.repoDir
    annotateAdvancedOpts = O.patchIndexYes
    annotateOpts = annotateBasicOpts `withStdOpts` annotateAdvancedOpts

annotateCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
annotateCmd fps opts args = do
  paths <- pathsFromArgs fps args
  case paths of
    [path] -> do
      when (patchIndexYes ? opts == O.YesPatchIndex)
        $ withRepoLockCanFail (useCache ? opts)
        $ RepoJob (\repo -> readRepo repo >>= attemptCreatePatchIndex repo)
      annotateCmd' opts path
    _ -> die "Error: annotate requires a single filepath argument"

annotateCmd' :: [DarcsFlag] -> AnchoredPath -> IO ()
annotateCmd' opts fixed_path = withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  let matchFlags = parseFlags O.matchUpToOne opts
  r <- readRepo repository
  recorded <- readRecorded repository
  (patches, initial, path) <-
    case patchSetMatch matchFlags of
      Just psm -> do
        Sealed x <- getOnePatchset repository psm
        let (_, [path'], _) =
              withFileNames Nothing [fixed_path] (rollbackToPatchSetMatch psm r)
        initial <- snd `fmap` virtualTreeIO (rollbackToPatchSetMatch psm r) recorded
        return (seal $ patchSet2RL x, initial, path')
      Nothing ->
        return (seal $ patchSet2RL r, recorded, fixed_path)

  found <- findM initial path
  -- TODO need to decide about the --machine flag
  let (fmt, view) = if parseFlags O.machineReadable opts
                      then (A.machineFormat, putStrLn . renderString)
                      else (A.format, viewDocWith simplePrinters)
  usePatchIndex <- (O.yes (O.patchIndexYes ? opts) &&) <$> canUsePatchIndex repository
  case found of
    Nothing -> die $ "Error: path not found in repository: " ++ displayPath fixed_path
    Just (SubTree s) -> do
      -- TODO the semantics and implementation of annotating of directories need to be revised
      s' <- T.expand s
      let subs = map (catPaths path . fst) $ T.list s'
          showPath (n, File _) = BC.pack $ displayPath $ path `catPaths` n
          showPath (n, _) = BC.concat [BC.pack $ displayPath $ path `catPaths` n, "/"]
      (Sealed ans_patches) <- do
         if not usePatchIndex
            then return patches
            else getRelevantSubsequence patches repository r subs
      view . text $
        fmt (BC.intercalate "\n" $ map showPath $ T.list s') $
        A.annotateDirectory ans_patches path subs
    Just (File b) -> do (Sealed ans_patches) <- do
                           if not usePatchIndex
                              then return patches
                              else getRelevantSubsequence patches repository r [path]
                        con <- BC.concat `fmap` toChunks `fmap` T.readBlob b
                        view $ text . fmt con $
                          A.annotateFile ans_patches path con
    Just (Stub _ _) -> error "impossible case"
