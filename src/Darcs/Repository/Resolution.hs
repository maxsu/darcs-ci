--  Copyright (C) 2003,2005 David Roundy
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
module Darcs.Repository.Resolution
    ( standardResolution
    , externalResolution
    , patchsetConflictResolutions
    , StandardResolution(..)
    , announceConflicts
    , warnUnmangled
    , showUnmangled
    , showUnravelled
    ) where

import Darcs.Prelude

import System.FilePath.Posix ( (</>) )
import System.Exit ( ExitCode( ExitSuccess ) )
import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Data.List ( intersperse, zip4 )
import Data.List.Ordered ( nubSort )
import Data.Maybe ( catMaybes, isNothing )
import Control.Monad ( when )

import Darcs.Repository.Diff( treeDiff )
import Darcs.Patch
    ( PrimOf
    , PrimPatchBase
    , RepoPatch
    , applyToTree
    , effect
    , effectOnPaths
    , invert
    , listConflictedFiles
    , resolveConflicts
    )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Conflict ( Conflict, ConflictDetails(..), Mangled, Unravelled )
import Darcs.Patch.Inspect ( listTouchedFiles )
import Darcs.Patch.Merge ( mergeList )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Util.Path
    ( AnchoredPath
    , anchorPath
    , displayPath
    , filterPaths
    , toFilePath
    )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unseal, unFreeLeft )

import Darcs.Util.CommandLine ( parseCmd )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd )
import Darcs.Util.Prompt ( askEnter )
import Darcs.Patch.Set ( PatchSet(..), Origin, patchSet2RL )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Util.Exec ( exec, Redirect(..) )
import Darcs.Util.Lock ( withTempDir )
import Darcs.Util.External ( cloneTree )
import Darcs.Repository.Flags
    ( AllowConflicts (..)
    , ExternalMerge (..)
    , WantGuiPause (..)
    , DiffAlgorithm (..)
    )

import qualified Darcs.Util.Tree as Tree
import Darcs.Util.Tree.Plain ( writePlainTree, readPlainTree )

import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Printer ( Doc, renderString, ($$), text, redText, vcat )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Patch ( displayPatch )

data StandardResolution prim wX =
  StandardResolution {
    mangled :: Mangled prim wX,
    unmangled :: [Unravelled prim wX],
    conflictedPaths :: [AnchoredPath]
  }

standardResolution :: (Commute p, PrimPatchBase p, Conflict p)
                   => RL (PatchInfoAnd rt p) wO wX
                   -> RL (PatchInfoAnd rt p) wX wY
                   -> StandardResolution (PrimOf p) wY
standardResolution context interesting =
  case mergeList $ catMaybes $ map conflictMangled conflicts of
    Right mangled -> StandardResolution {..}
    Left (Sealed ps, Sealed qs) ->
      error $ renderString
        $ redText "resolutions conflict:"
        $$ displayPatch ps
        $$ redText "conflicts with"
        $$ displayPatch qs
  where
    conflicts = resolveConflicts context interesting
    unmangled = map conflictParts $ filter (isNothing . conflictMangled) conflicts
    conflictedPaths =
      nubSort $
      concatMap (unseal listTouchedFiles) (concatMap conflictParts conflicts)

warnUnmangled :: PrimPatch prim => StandardResolution prim wX -> IO ()
warnUnmangled StandardResolution {..}
  | null unmangled = return ()
  | otherwise = ePutDocLn $ showUnmangled unmangled

showUnmangled :: PrimPatch prim => [Unravelled prim wX] -> Doc
showUnmangled = vcat . map showUnmangledConflict
  where
    showUnmangledConflict unravelled =
      redText "Cannot mark these conflicting patches:" $$
      showUnravelled (redText "versus") unravelled

showUnravelled :: PrimPatch prim => Doc -> Unravelled prim wX -> Doc
showUnravelled sep =
  vcat . intersperse sep . map (unseal displayPatch)

announceConflicts :: PrimPatch prim
                  => String
                  -> AllowConflicts
                  -> ExternalMerge
                  -> StandardResolution prim wX
                  -> IO Bool
announceConflicts cmd allowConflicts externalMerge conflicts =
  case nubSort (conflictedPaths conflicts) of
    [] -> return False
    cfs -> do
      ePutDocLn $ vcat $ redText
        "We have conflicts in the following files:" : map (text . displayPath) cfs
      when (allowConflicts == YesAllowConflictsAndMark) $ warnUnmangled conflicts
      if allowConflicts `elem` [YesAllowConflicts,YesAllowConflictsAndMark]
              || externalMerge /= NoExternalMerge
        then return True
        else fail $
          "Refusing to "++cmd++" patches leading to conflicts.\n"++
          "If you would rather apply the patch and mark the conflicts,\n"++
          "use the --mark-conflicts or --allow-conflicts options to "++cmd++"\n"++
          "These can set as defaults by adding\n"++
          " "++cmd++" mark-conflicts\n"++
          "to "++darcsdir++"/prefs/defaults in the target repo. "

externalResolution :: forall p wX wY wZ wA. (RepoPatch p, ApplyState p ~ Tree.Tree)
                   => DiffAlgorithm
                   -> Tree.Tree IO        -- ^ working tree
                   -> String              -- ^ external merge tool command
                   -> WantGuiPause        -- ^ tell whether we want GUI pause
                   -> FL (PrimOf p) wX wY -- ^ our effect
                   -> FL (PrimOf p) wX wZ -- ^ their effect
                   -> FL p wY wA          -- ^ them merged (standard_resolution)
                   -> IO (Sealed (FL (PrimOf p) wA))
externalResolution diffa s1 c wantGuiPause p1 p2 pmerged = do
 sa <- applyToTree (invert p1) s1
 sm <- applyToTree pmerged s1
 s2 <- applyToTree p2 sa
 let nms = listConflictedFiles pmerged
     nas = effectOnPaths (invert (effect pmerged)) nms
     n1s = effectOnPaths p1 nas
     n2s = effectOnPaths p2 nas
     ns = zip4 (tofp nas) (tofp n1s) (tofp n2s) (tofp nms)
     tofp = map (anchorPath "")
     write_files tree fs = writePlainTree (Tree.filter (filterPaths fs) tree) "."
  in do
   former_dir <- getCurrentDirectory
   withTempDir "version1" $ \absd1 -> do
     let d1 = toFilePath absd1
     write_files s1 n1s
     setCurrentDirectory former_dir
     withTempDir "ancestor" $ \absda -> do
       let da = toFilePath absda
       write_files sa nas
       setCurrentDirectory former_dir
       withTempDir "merged" $ \absdm -> do
         let dm = toFilePath absdm
         write_files sm nms
         setCurrentDirectory former_dir
         withTempDir "cleanmerged" $ \absdc -> do
           let dc = toFilePath absdc
           cloneTree dm "."
           setCurrentDirectory former_dir
           withTempDir "version2" $ \absd2 -> do
             let d2 = toFilePath absd2
             write_files s2 n2s
             mapM_ (externallyResolveFile c wantGuiPause da d1 d2 dm) ns
             sc <- readPlainTree dc
             sfixed <- readPlainTree dm
             ftf <- filetypeFunction
             unFreeLeft `fmap` treeDiff diffa ftf sc sfixed

externallyResolveFile :: String -- ^ external merge tool command
                      -> WantGuiPause -- ^ tell whether we want GUI pause
                      -> String -- ^ path to merge base
                      -> String -- ^ path to side 1 of the merge
                      -> String -- ^ path to side 2 of the merge
                      -> String -- ^ path where resolved content should go
                      -> (FilePath, FilePath, FilePath, FilePath)
                      -> IO ()
externallyResolveFile c wantGuiPause da d1 d2 dm (fa, f1, f2, fm) = do
    putStrLn $ "Merging file "++fm++" by hand."
    ec <- run c [('1', d1</>f1), ('2', d2</>f2), ('a', da</>fa), ('o', dm</>fm), ('%', "%")]
    when (ec /= ExitSuccess) $
         putStrLn $ "External merge command exited with " ++ show ec
    when (wantGuiPause == YesWantGuiPause) $
        askEnter "Hit return to move on, ^C to abort the whole operation..."

run :: String -> [(Char,String)] -> IO ExitCode
run c replacements =
    case parseCmd replacements c of
    Left err     -> fail $ show err
    Right (c2,_) -> rr c2
    where rr (command:args) = do putStrLn $ "Running command '" ++
                                            unwords (command:args) ++ "'"
                                 exec command args (Null,Null,Null)
          rr [] = return ExitSuccess

patchsetConflictResolutions :: RepoPatch p
                            => PatchSet rt p Origin wX
                            -> StandardResolution (PrimOf p) wX
patchsetConflictResolutions (PatchSet ts xs) =
  -- optimization: all patches before the latest known clean tag
  -- are known to be resolved
  standardResolution (patchSet2RL (PatchSet ts NilRL)) xs
