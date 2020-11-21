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

module Darcs.UI.Commands.Log
    ( changes
    , log
    , changelog
    , logInfoFL
    , simpleLogInfo -- for darcsden
    ) where

import Darcs.Prelude

import Data.List ( intersect, find )
import Data.List.Ordered ( nubSort )
import Data.Maybe ( fromMaybe, isJust )
import Control.Arrow ( second )
import Control.Exception ( catch, IOException )
import Control.Monad.State.Strict

import Darcs.UI.PrintPatch ( showFriendly )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAndG, fmapFLPIAP, hopefullyM, info )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, commandAlias, findRepository )
import Darcs.UI.Commands.Util ( matchRange )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.UI.External ( viewDocWith )
import Darcs.UI.Flags
    ( DarcsFlag
    , changesReverse, onlyToFiles
    , useCache, maxCount, hasXmlOutput
    , verbosity, withContext, isInteractive, verbose
    , getRepourl, pathSetFromArgs )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path
    ( SubPath
    , AbsolutePath
    , simpleSubPath
    , AnchoredPath
    , floatSubPath
    , displayPath
    )
import Darcs.Repository ( PatchInfoAnd,
                          withRepositoryLocation, RepoJob(..),
                          readRepo, unrecordedChanges,
                          withRepoLockCanFail )
import Darcs.Repository.Flags ( UseIndex(..), ScanKnown(..), DiffAlgorithm(MyersDiff) )
import Darcs.Util.Lock ( withTempDir )
import Darcs.Patch.Set ( PatchSet, patchSet2RL, Origin )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( toXml, toXmlShort, showPatchInfo, displayPatchInfo, escapeXML, PatchInfo )
import Darcs.Patch.Ident ( PatchId )
import Darcs.Patch.Invertible ( mkInvertible )
import Darcs.Patch.Depends ( contextPatches )
import Darcs.Patch.Show ( ShowPatch, ShowPatchFor(..) )
import Darcs.Patch.TouchesFiles ( lookTouch )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch ( PrimPatchBase(..), invert, xmlSummary, description,
                     effectOnPaths, listTouchedFiles, showPatch )
import Darcs.Patch.Named ( HasDeps, getdeps )
import Darcs.Patch.Prim.Class ( PrimDetails )
import Darcs.Patch.Summary ( Summary )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(NilFL), RL(..), filterOutFLFL, filterRL,
    reverseFL, (:>)(..), mapFL, mapRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..), unseal2, Sealed(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Match
    ( MatchFlag
    , Matchable
    , MatchableRP
    , matchAPatch
    , haveNonrangeMatch
    )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , (<+>)
    , formatWords
    , hsep
    , insertBeforeLastline
    , prefix
    , simplePrinters
    , text
    , vcat
    , vsep
    )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Progress ( setProgressMode, debugMessage )
import Darcs.UI.SelectChanges ( viewChanges )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import Darcs.Repository.PatchIndex ( PatchFilter, maybeFilterPatches, attemptCreatePatchIndex )
import Darcs.Util.Tree( Tree )

logHelp :: Doc
logHelp = vsep $ map formatWords
  [ [ "The `darcs log` command lists patches of the current repository or,"
    , "with `--repo`, a remote repository.  Without options or arguments,"
    , "ALL patches will be listed."
    ]
  , [ "When given files or directories paths as arguments, only patches which"
    , "affect those paths are listed.  This includes patches that happened to"
    , "files before they were moved or renamed."
    ]
  , [ "When given `--from-tag` or `--from-patch`, only patches since that tag"
    , "or patch are listed.  Similarly, the `--to-tag` and `--to-patch`"
    , "options restrict the list to older patches."
    ]
  , [ "The `--last` and `--max-count` options both limit the number of patches"
    , "listed.  The former applies BEFORE other filters, whereas the latter"
    , "applies AFTER other filters.  For example `darcs log foo.c"
    , "--max-count 3` will print the last three patches that affect foo.c,"
    , "whereas `darcs log --last 3 foo.c` will, of the last three"
    , "patches, print only those that affect foo.c."
    ]
  , [ "Four output formats exist.  The default is `--human-readable`. The slightly"
    , "different `--machine-readable` format enables to see patch dependencies in"
    , "non-interactive mode. You can also select `--context`, which is an internal"
    , "format that can be re-read by Darcs (e.g. `darcs clone --context`)."
    ]
  , [ "Finally, there is `--xml-output`, which emits valid XML... unless a the"
    , "patch metadata (author, name or description) contains a non-ASCII"
    , "character and was recorded in a non-UTF8 locale."
    ]
  ]

log :: DarcsCommand
log = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "log"
    , commandHelp = logHelp
    , commandDescription = "List patches in the repository."
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCompleteArgs = knownFileArgs
    , commandCommand = logCmd
    , commandPrereq = findRepository
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc logAdvancedOpts
    , commandBasicOptions = odesc logBasicOpts
    , commandDefaults = defaultFlags logOpts
    , commandCheckOptions = ocheck logOpts
    }
  where
    logBasicOpts
      = O.matchSeveralOrRange
      ^ O.maxCount
      ^ O.onlyToFiles
      ^ O.changesFormat
      ^ O.withSummary
      ^ O.changesReverse
      ^ O.possiblyRemoteRepo
      ^ O.repoDir
      ^ O.interactive
    logAdvancedOpts = O.network ^ O.patchIndexYes
    logOpts = logBasicOpts `withStdOpts` logAdvancedOpts

logCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
logCmd fps opts args
  | O.changesFormat ? opts == Just O.GenContext = if not . null $ args
      then fail "log --context cannot accept other arguments"
      else logContext opts
  | hasRemoteRepo opts = do
      (fs, es) <- remoteSubPaths args []
      if null es then
        withTempDir "darcs.log"
          (\_ -> showLog opts $ maybeNotNull $ nubSort $ map floatSubPath fs)
      else
        fail $ "For a remote repo I can only handle relative paths.\n"
            ++ "Invalid arguments: "++unwords es
  | null args = showLog opts Nothing
  | otherwise = do
      unless (isInteractive False opts)
        $ when (O.patchIndexNo ? opts == O.YesPatchIndex)
          $ withRepoLockCanFail (useCache ? opts)
            $ RepoJob (\repo -> readRepo repo >>= attemptCreatePatchIndex repo)
      paths <- pathSetFromArgs fps args
      showLog opts paths

maybeNotNull :: [a] -> Maybe [a]
maybeNotNull [] = Nothing
maybeNotNull xs = Just xs

hasRemoteRepo :: [DarcsFlag] -> Bool
hasRemoteRepo = isJust . getRepourl

remoteSubPaths :: [String] -> [String] -> IO ([SubPath],[String])
remoteSubPaths [] es = return ([], es)
remoteSubPaths (arg:args) es = case simpleSubPath arg of
  Nothing -> remoteSubPaths args (arg:es)
  Just sp -> do
    (sps, es') <- remoteSubPaths args es
    return (sp:sps, es')

showLog :: [DarcsFlag] -> Maybe [AnchoredPath] -> IO ()
showLog opts files =
  let repodir = fromMaybe "." (getRepourl opts) in
  withRepositoryLocation (useCache ? opts) repodir $ RepoJob $ \repository -> do
  unless (O.debug ? opts) $ setProgressMode False
  Sealed unrec <- case files of
    Nothing -> return $ Sealed NilFL
    Just _ -> Sealed `fmap` unrecordedChanges (UseIndex, ScanKnown, MyersDiff)
                  O.NoLookForMoves O.NoLookForReplaces
                  repository files
                  `catch` \(_ :: IOException) -> return (Sealed NilFL) -- this is triggered when repository is remote
  debugMessage "About to read the repository..."
  patches <- readRepo repository
  debugMessage "Done reading the repository."
  let recFiles = effectOnPaths (invert unrec) <$> files
      filtered_changes p =
          maybe_reverse <$>
          getLogInfo
              (maxCount ? opts)
              (parseFlags O.matchSeveralOrRange opts)
              (onlyToFiles ? opts)
              recFiles
              (maybeFilterPatches repository patches)
              p
  if isInteractive False opts
    then do li <- filtered_changes patches
            viewChanges (logPatchSelOpts opts) (map fst (liPatches li))
    else do let header =
                  case recFiles of
                    Just fs | not (hasXmlOutput opts) ->
                      let pathlist = map (text . displayPath) fs
                      in hsep (text "Changes to" : pathlist) <> text ":" $$ text ""
                    _ -> mempty
            debugMessage "About to print the patches..."
            let printers = if hasXmlOutput opts then simplePrinters else fancyPrinters
            ps <- readRepo repository -- read repo again to prevent holding onto
                                       -- values forced by filtered_changes
            logOutput <- changelog opts (patchSet2RL ps) `fmap` filtered_changes patches
            viewDocWith printers (header $$ logOutput)
  where
    maybe_reverse li@(LogInfo xs b c) =
      if changesReverse ? opts then LogInfo (reverse xs) b c else li

data LogInfo p = LogInfo
  { liPatches :: [(Sealed2 p, [AnchoredPath])]
  , liRenames :: [(AnchoredPath, AnchoredPath)]
  , liErrorMsg :: Maybe Doc
  }

mkLogInfo :: [Sealed2 p] -> LogInfo p
mkLogInfo ps = LogInfo (map (,[]) ps) [] Nothing

logInfoFL :: FL p wX wY -> LogInfo p
logInfoFL = mkLogInfo . mapFL Sealed2

matchNonrange :: (Matchable p, PatchId p ~ PatchInfo)
              => [MatchFlag] -> RL p wA wB -> [Sealed2 p]
matchNonrange matchFlags
  | haveNonrangeMatch matchFlags = filterRL (matchAPatch matchFlags)
  | otherwise = mapRL Sealed2

simpleLogInfo :: ( MatchableRP p
                 , ApplyState p ~ Tree
                 )
              => AnchoredPath
              -> PatchFilter rt p
              -> PatchSet rt p Origin wY
              -> IO [Sealed2 (PatchInfoAnd rt p)]
simpleLogInfo path pf ps =
  map fst . liPatches <$> getLogInfo Nothing [] False (Just [path]) pf ps

getLogInfo :: forall rt p wY.
              ( MatchableRP p
              , ApplyState p ~ Tree
              )
           => Maybe Int -> [MatchFlag] -> Bool
           -> Maybe [AnchoredPath]
           -> PatchFilter rt p
           -> PatchSet rt p Origin wY
           -> IO (LogInfo (PatchInfoAnd rt p))
getLogInfo maxCountFlag matchFlags onlyToFilesFlag paths patchFilter ps =
  case matchRange matchFlags ps of
    Sealed2 range ->
      let ps' = matchNonrange matchFlags (reverseFL range) in
      case paths of
        Nothing -> return $ mkLogInfo $ maybe id take maxCountFlag ps'
        Just fs -> do
          filterOutUnrelatedChanges <$> do
            ps'' <- patchFilter fs ps'
            return $ filterPatchesByNames maxCountFlag fs ps''
  where
        -- What we do here is somewhat unclean: we modify the contents of
        -- our patches and throw out everything not related to our files.
        -- This is okay because we only use the result for display.
        filterOutUnrelatedChanges li
          | onlyToFilesFlag = li { liPatches = map onlyRelated (liPatches li) }
          | otherwise       = li

        onlyRelated (Sealed2 p, fs) =
          (Sealed2 $ fmapFLPIAP (filterOutFLFL (unrelated fs)) p, fs)

        unrelated fs p
          -- If the change does not affect the patches we are looking at,
          -- we ignore the difference between the two states.
          | null $ fs `intersect` listTouchedFiles p = unsafeCoerceP IsEq
          | otherwise                                = NotEq

-- | Take a list of filenames and patches and produce a list of patches that
-- actually touch the given files with a list of touched file names, a list of
-- original-to-current filepath mappings, indicating the original names of the
-- affected files and possibly an error. Additionaly, the function takes a
-- "depth limit" -- maxcount, that could be Nothing (return everything) or
-- "Just n" -- returns at most n patches touching the file (starting from the
-- beginning of the patch list).
filterPatchesByNames
    :: forall rt p.
       ( MatchableRP p
       , ApplyState p ~ Tree
       )
    => Maybe Int                      -- ^ maxcount
    -> [AnchoredPath]                 -- ^ paths
    -> [Sealed2 (PatchInfoAnd rt p)]  -- ^ patches
    -> LogInfo (PatchInfoAnd rt p)
filterPatchesByNames maxcount paths patches = removeNonRenames $
    evalState (filterPatchesByNamesM paths patches) (maxcount, initRenames) where
        removeNonRenames li = li { liRenames = removeIds (liRenames li) }
        removeIds = filter $ uncurry (/=)
        initRenames = map (\x -> (x, x)) paths
        returnFinal = (\renames -> LogInfo [] renames Nothing) <$> gets snd
        filterPatchesByNamesM [] _ = returnFinal
        filterPatchesByNamesM _ [] = returnFinal
        filterPatchesByNamesM fs (s2hp@(Sealed2 hp) : ps) = do
            (count, renames) <- get
            case count of
                Just c | c <= 0 -> returnFinal
                _ ->
                  case hopefullyM hp of
                    Nothing -> do
                        let err = text "Can't find patches prior to:"
                                  $$ displayPatchInfo (info hp)
                        return (LogInfo [] renames (Just err))
                    Just p ->
                        case lookTouch (Just renames) fs (invert (mkInvertible p)) of
                            (True, affected, [], renames') ->
                                return (LogInfo [(s2hp, affected)] renames' Nothing)
                            (True, affected, fs', renames') -> do
                                let sub1Mb c = subtract 1 <$> c
                                modify $ \(c, _) -> (sub1Mb c, renames')
                                rest <- filterPatchesByNamesM fs' ps
                                return $ rest {
                                    liPatches = (s2hp, affected) : liPatches rest
                                  }
                            (False, _, fs', renames') -> do
                                modify $ second (const renames')
                                filterPatchesByNamesM fs' ps

changelog :: forall rt p wStart wX
           . ( ShowPatch p, PatchListFormat p
             , Summary p, HasDeps p, PrimDetails (PrimOf p)
             )
          => [DarcsFlag] -> RL (PatchInfoAndG rt p) wStart wX
          -> LogInfo (PatchInfoAndG rt p)
          -> Doc
changelog opts patches li
    | O.changesFormat ? opts == Just O.CountPatches =
        text $ show $ length $ liPatches li
    | hasXmlOutput opts = xml_changelog
    | O.yes (O.withSummary ? opts) || verbose opts =
        vsep (map (number_patch change_with_summary) ps) $$ mbErr
    | otherwise = vsep (map (number_patch description') ps) $$ mbErr
    where ps_and_fs = liPatches li
          mbErr = fromMaybe mempty (liErrorMsg li)
          change_with_summary :: Sealed2 (PatchInfoAndG rt p) -> Doc
          change_with_summary (Sealed2 hp)
            | Just p <- hopefullyM hp =
              if O.changesFormat ? opts == Just O.MachineReadable
                then showPatch ForStorage p
                else showFriendly (verbosity ? opts) (O.withSummary ? opts) p
            | otherwise = description hp $$ indent (text "[this patch is unavailable]")

          xml_changelog = vcat
            [ text "<changelog>"
            , vcat xml_created_as
            , vcat xml_changes
            , text "</changelog>"
            ]

          xml_with_summary :: Sealed2 (PatchInfoAndG rt p) -> Doc
          xml_with_summary (Sealed2 hp) | Just p <- hopefullyM hp =
                    let
                      deps = getdeps p
                      xmlDependencies =
                        text "<explicit_dependencies>"
                        $$ vcat (map (indent . toXmlShort) deps)
                        $$ text "</explicit_dependencies>"
                      summary | deps == [] = indent $ xmlSummary p
                              | otherwise = indent $ xmlDependencies $$ xmlSummary p
                    in
                      insertBeforeLastline (toXml $ info hp) summary
          xml_with_summary (Sealed2 hp) = toXml (info hp)
          indent = prefix "    "
          xml_changes =
            case O.withSummary ? opts of
              O.YesSummary -> map xml_with_summary ps
              O.NoSummary -> map (toXml . unseal2 info) ps
          xml_created_as = map create (liRenames li) where
            create :: (AnchoredPath, AnchoredPath) -> Doc
            create rename@(_, as) = createdAsXml (first_change_of as) rename
            -- We need to reorder the patches when they haven't been reversed
            -- already, so that we find the *first* patch that modifies a given
            -- file, not the last (by default, the list is oldest->newest).
            reorderer = if not (changesReverse ? opts) then reverse else id
            oldest_first_ps_and_fs = reorderer ps_and_fs
            couldnt_find fn = error $ "Couldn't find first patch affecting " ++
                                      (displayPath fn) ++ " in ps_and_fs"
            mb_first_change_of fn = find ((fn `elem`) . snd) oldest_first_ps_and_fs
            find_first_change_of fn = fromMaybe (couldnt_find fn)
              (mb_first_change_of fn)
            first_change_of :: AnchoredPath -> PatchInfo
            first_change_of = unseal2 info . fst . find_first_change_of

          number_patch f x = if O.changesFormat ? opts == Just O.NumberPatches
                             then case get_number x of
                                  Just n -> text (show n++":") <+> f x
                                  Nothing -> f x
                             else f x

          get_number :: Sealed2 (PatchInfoAndG re p) -> Maybe Int
          get_number (Sealed2 y) = gn 1 patches
              where iy = info y
                    gn :: Int -> RL (PatchInfoAndG rt p) wStart wY -> Maybe Int
                    gn n (bs:<:b) | seq n (info b) == iy = Just n
                                  | otherwise = gn (n+1) bs
                    gn _ NilRL = Nothing
          ps = map fst ps_and_fs
          description' = unseal2 description

logContext :: [DarcsFlag] -> IO ()
logContext opts = do
  let repodir = fromMaybe "." $ getRepourl opts
  withRepositoryLocation (useCache ? opts) repodir $ RepoJob $ \repository -> do
      (_ :> ps) <- contextPatches `fmap` readRepo repository
      let header = text "\nContext:\n"
      viewDocWith simplePrinters $ vsep
          (header : mapRL (showPatchInfo ForStorage . info) ps)

-- | changes is an alias for log
changes :: DarcsCommand
changes = commandAlias "changes" Nothing log

createdAsXml :: PatchInfo -> (AnchoredPath, AnchoredPath) -> Doc
createdAsXml pinfo (current, createdAs) =
    text "<created_as current_name='"
       <> escapeXML (displayPath current)
       <> text "' original_name='"
       <> escapeXML (displayPath createdAs)
       <> text "'>"
    $$    toXml pinfo
    $$    text "</created_as>"

logPatchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
logPatchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveralOrRange flags
    , S.interactive = isInteractive False flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.withSummary ? flags
    , S.withContext = withContext ? flags
    }
