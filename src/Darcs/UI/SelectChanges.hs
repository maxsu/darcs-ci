-- Copyright (C) 2002-2003 David Roundy
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

{-# LANGUAGE RecordWildCards #-}
module Darcs.UI.SelectChanges
    ( -- * Working with changes
      WhichChanges(..)
    , viewChanges
    , withSelectedPatchFromList
    , runSelection
    , runInvertibleSelection
    , selectionConfigPrim
    , selectionConfigGeneric
    , selectionConfig
    , SelectionConfig(allowSkipAll)
    -- * Interactive selection utils
    , PatchSelectionOptions(..)
    , InteractiveSelectionM
    , InteractiveSelectionState(..)
    , initialSelectionState
    -- ** Navigating the patchset
    , currentPatch
    , skipMundane
    , skipOne
    , backOne
    , backAll
    -- ** Decisions
    , decide
    , decideWholeFile
    -- ** Prompts and queries
    , isSingleFile
    , currentFile
    , promptUser
    , prompt
    , KeyPress(..)
    , keysFor
    , helpFor
    , askAboutDepends
    ) where

import Darcs.Prelude

import Control.Monad ( liftM, unless, when, (>=>) )
import Control.Monad.Identity ( Identity (..) )
import Control.Monad.Reader
    ( ReaderT
    , asks
    , runReaderT
    )
import Control.Monad.State
    ( StateT, execStateT, gets
    , modify, runStateT, state
    )
import Control.Monad.Trans ( liftIO )
import Data.List ( intercalate, union )
import Data.Maybe ( isJust )
import System.Exit ( exitSuccess )

import Darcs.Patch
    ( IsRepoType, RepoPatch, PrimOf
    , commuteFL, invert
    , listTouchedFiles
    )
import qualified Darcs.Patch ( thing, things )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Choices
    ( PatchChoices, Slot (..), LabelledPatch
    , forceFirst, forceLast, forceMatchingFirst
    , forceMatchingLast, getChoices
    , makeEverythingLater, makeEverythingSooner
    , forceMiddle, patchChoices
    , patchSlot
    , refineChoices, selectAllMiddles
    , separateFirstFromMiddleLast
    , substitute, label, unLabel
    , labelPatches
    )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Depends ( contextPatches )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Invertible
import Darcs.Patch.Match
    ( Matchable
    , MatchableRP
    , haveNonrangeMatch
    , matchAPatch
    )
import Darcs.Patch.Named ( adddeps, anonymous )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch.Permutations ( commuteWhatWeCanRL )
import Darcs.Patch.Show ( ShowPatch, ShowContextPatch )
import Darcs.Patch.Split ( Splitter(..) )
import Darcs.Patch.TouchesFiles ( selectNotTouching, deselectNotTouching )
import Darcs.Patch.Witnesses.Ordered
    ( (:>) (..), (:||:) (..), FL (..)
    , RL (..), filterFL, lengthFL, mapFL
    , mapFL_FL, spanFL, spanFL_M
    , (+>+), (+<<+)
    , reverseFL, reverseRL
    )
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal (..), Sealed2 (..)
    , flipSeal, seal2, unseal2
    )
import Darcs.Patch.Witnesses.WZipper
    ( FZipper (..), focus, jokers, left, right
    , rightmost, toEnd, toStart
    )
import Darcs.Repository ( Repository, repoLocation, readTentativeRepo )
import Darcs.UI.External ( editText )
import Darcs.UI.Options.All
    ( Verbosity(..), WithSummary(..)
    , WithContext(..), SelectDeps(..), MatchFlag )
import Darcs.UI.PrintPatch
    ( printContent
    , printContentWithPager
    , printFriendly
    , printSummary
    , showFriendly
    )
import Darcs.Util.English ( Noun (..), englishNum, capitalize )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer ( putDocLnWith, greenText, vcat )
import Darcs.Util.Printer.Color ( fancyPrinters )
import Darcs.Util.Prompt ( PromptConfig (..), askUser, promptChar )
import Darcs.Util.Tree ( Tree )


-- | When asking about patches, we either ask about them in
-- oldest-first or newest first (with respect to the current ordering
-- of the repository), and we either want an initial segment or a
-- final segment of the poset of patches.
--
-- 'First': ask for an initial
-- segment, first patches first (default for all pull-like commands)
--
-- 'FirstReversed': ask for an initial segment, last patches first
-- (used to ask about dependencies in record, and for pull-like
-- commands with the @--reverse@ flag).
--
-- 'LastReversed': ask for a final segment, last patches first. (default
-- for unpull-like commands, except for selecting *primitive* patches in
-- rollback)
--
-- 'Last': ask for a final segment, first patches first. (used for selecting
-- primitive patches in rollback, and for unpull-like commands with the
-- @--reverse@ flag
--
-- IOW: First = initial segment
--      Last = final segment
--      Reversed = start with the newest patch instead of oldest
-- As usual, terminology is not, ahem, very intuitive.
data WhichChanges = Last | LastReversed | First | FirstReversed deriving (Eq, Show)

-- | A 'WhichChanges' is 'backward' if the segment of patches we ask for
-- is at the opposite end of where we start to present them.
backward :: WhichChanges -> Bool
backward w = w == Last || w == FirstReversed

-- | A 'WhichChanges' is reversed if the order in which patches are presented
-- is latest (or newest) patch first.
reversed :: WhichChanges -> Bool
reversed w = w == LastReversed || w == FirstReversed

-- | The type of the function we use to filter patches when @--match@ is
-- given.
data MatchCriterion p = MatchCriterion
   { mcHasNonrange :: Bool
   , mcFunction :: forall wA wB. p wA wB -> Bool
   }

data PatchSelectionOptions = PatchSelectionOptions
  { verbosity :: Verbosity
  , matchFlags :: [MatchFlag]
  , interactive :: Bool
  , selectDeps :: SelectDeps
  , withSummary :: WithSummary
  , withContext :: WithContext
  }

-- | All the static settings for selecting patches.
data SelectionConfig p =
  PSC { opts :: PatchSelectionOptions
      , splitter :: Maybe (Splitter p)
      , files :: Maybe [AnchoredPath]
      , matchCriterion :: MatchCriterion p
      , jobname :: String
      , allowSkipAll :: Bool
      , pristine :: Maybe (Tree IO)
      , whichChanges :: WhichChanges
      }

-- | A 'SelectionConfig' for selecting 'Prim' patches.
selectionConfigPrim :: WhichChanges
                    -> String
                    -> PatchSelectionOptions
                    -> Maybe (Splitter prim)
                    -> Maybe [AnchoredPath]
                    -> Maybe (Tree IO)
                    -> SelectionConfig prim
selectionConfigPrim whch jn o spl fs p =
 PSC { opts = o
     , splitter = spl
     , files = fs
     , matchCriterion = triv
     , jobname = jn
     , allowSkipAll = True
     , pristine = p
     , whichChanges = whch
     }

-- | A 'SelectionConfig' for selecting full ('Matchable') patches
selectionConfig :: Matchable p
                 => WhichChanges
                 -> String
                 -> PatchSelectionOptions
                 -> Maybe (Splitter p)
                 -> Maybe [AnchoredPath]
                 -> SelectionConfig p
selectionConfig whch jn o spl fs =
 PSC { opts = o
     , splitter = spl
     , files = fs
     , matchCriterion = iswanted seal2 (matchFlags o)
     , jobname = jn
     , allowSkipAll = True
     , pristine = Nothing
     , whichChanges = whch
     }

-- | A generic 'SelectionConfig'.
selectionConfigGeneric :: Matchable p
                       => (forall wX wY . q wX wY -> Sealed2 p)
                       -> WhichChanges
                       -> String
                       -> PatchSelectionOptions
                       -> Maybe [AnchoredPath]
                       -> SelectionConfig q
selectionConfigGeneric extract whch jn o fs =
 PSC { opts = o
     , splitter = Nothing
     , files = fs
     , matchCriterion = iswanted extract (matchFlags o)
     , jobname = jn
     , allowSkipAll = True
     , pristine = Nothing
     , whichChanges = whch
     }

-- | The dynamic parameters for interactive selection of patches.
data InteractiveSelectionState p wX wY =
 ISC { total :: Int                           -- ^ total number of patches
     , current :: Int                         -- ^ number of already-seen patches
     , lps :: FZipper (LabelledPatch p) wX wY -- ^ the patches we offer
     , choices :: PatchChoices p wX wY        -- ^ the user's choices
     }

type PatchSelectionM p a = ReaderT (SelectionConfig p) a

type InteractiveSelectionM p wX wY a =
    StateT (InteractiveSelectionState p wX wY)
           (PatchSelectionM p IO) a

-- Common match criteria

-- | For commands without @--match@, 'triv' matches all patches
triv :: MatchCriterion p
triv = MatchCriterion { mcHasNonrange = False, mcFunction = \ _ -> True }

-- | 'iswanted' selects patches according to the given match flags
iswanted :: Matchable p
         => (forall wX wY . q wX wY -> Sealed2 p)
         -> [MatchFlag]
         -> MatchCriterion q
iswanted extract mflags = MatchCriterion
    { mcHasNonrange = haveNonrangeMatch mflags
    , mcFunction = unseal2 (matchAPatch mflags) . extract
    }

-- | Run a 'PatchSelection' action in the given 'SelectionConfig',
-- without assuming that patches are invertible.
runSelection :: ( MatchableRP p, ShowPatch p, ShowContextPatch p
                , ApplyState p ~ Tree, ApplyState p ~ ApplyState (PrimOf p)
                )
             => FL p wX wY
             -> SelectionConfig p
             -> IO ((FL p :> FL p) wX wY)
runSelection _ PSC { splitter = Just _ } =
  -- a Splitter makes sense for prim patches only and these are invertible anyway
  error "cannot use runSelection with Splitter"
runSelection ps PSC { matchCriterion = mc, .. } = do
    unwrapOutput <$> runInvertibleSelection (wrapInput ps) ictx
  where
    convertMC :: MatchCriterion p -> MatchCriterion (Invertible p)
    convertMC MatchCriterion { mcFunction = mcf, .. } =
      MatchCriterion { mcFunction = withInvertible mcf, .. }
    ictx = PSC { matchCriterion = convertMC mc, splitter = Nothing, .. }
    wrapInput = mapFL_FL mkInvertible
    unwrapOutput (xs :> ys) =
      mapFL_FL fromPositiveInvertible xs :> mapFL_FL fromPositiveInvertible ys

-- | Run a 'PatchSelection' action in the given 'SelectionConfig',
-- assuming patches are invertible.
runInvertibleSelection :: forall p wX wY .
                          ( Invert p, MatchableRP p, ShowPatch p
                          , ShowContextPatch p, ApplyState p ~ Tree
                          )
                       => FL p wX wY
                       -> SelectionConfig p
                       -> IO ((FL p :> FL p) wX wY)
runInvertibleSelection ps psc = runReaderT (selection ps) psc where
  selection
    | reversed whch = fmap invert . doit . invert
    | otherwise = doit
  -- efficiency note: we should first filterUnwanted to apply matchers,
  -- as this often requires to read only metadata; then filterNotTouching
  -- applies path restrictions which needs to read patch contents
  doit =
    fmap (canonizeAfterSplitter . selectedPatches) .
    selectChanges . filterNotTouching . filterUnwanted . patchChoices

  -- configuration
  whch = whichChanges psc
  fs = files psc
  os = opts psc
  crit = matchCriterion psc
  mspl = splitter psc

  -- after selecting with a splitter, the results may not be canonical
  canonizeAfterSplitter :: (FL p :> FL p) wA wB -> (FL p :> FL p) wA wB
  canonizeAfterSplitter (x :> y) =
    let canonizeIfNeeded = maybe id canonizeSplit mspl
    in canonizeIfNeeded x :> canonizeIfNeeded y

  -- retrieve the results of patch selection
  selectedPatches :: PatchChoices p wA wB -> (FL p :> FL p) wA wB
  selectedPatches pc
    | backward whch =
        case getChoices pc of
          fc :> mc :> lc -> mapFL_FL unLabel (fc +>+ mc) :> mapFL_FL unLabel lc
    | otherwise =
        case separateFirstFromMiddleLast pc of
          xs :> ys -> mapFL_FL unLabel xs :> mapFL_FL unLabel ys

  selectChanges :: PatchChoices p wA wB
                -> PatchSelectionM p IO (PatchChoices p wA wB)
  selectChanges
    | interactive os = refineChoices textSelect
    | otherwise      = return . promote

  promote
    | backward whch = makeEverythingLater
    | otherwise     = makeEverythingSooner
  demote
    | backward whch = makeEverythingSooner
    | otherwise     = makeEverythingLater

  filterNotTouching
    | backward whch = selectNotTouching fs
    | otherwise     = deselectNotTouching fs

  -- when using @--match@, remove unmatched patches
  -- not depended upon by matched patches
  filterUnwanted :: PatchChoices p wA wB -> PatchChoices p wA wB
  filterUnwanted
    | mcHasNonrange crit =
        case selectDeps os of
          NoDeps -> deselectUnwanted
          _      -> demote . selectWanted
    | otherwise = id

  selectWanted
    | backward whch = forceMatchingLast iswanted_
    | otherwise     = forceMatchingFirst iswanted_
  deselectUnwanted
    | backward whch = forceMatchingFirst (not . iswanted_)
    | otherwise     = forceMatchingLast (not . iswanted_)
  iswanted_ = mcFunction crit . unLabel

  {- end of runInvertibleSelection -}

-- | The equivalent of 'runSelection' for the @darcs log@ command
viewChanges :: (ShowPatch p, ShowContextPatch p, ApplyState p ~ Tree)
            => PatchSelectionOptions -> [Sealed2 p] -> IO ()
viewChanges ps_opts = textView ps_opts Nothing 0 []

-- | The type of the answers to a "shall I [wiggle] that [foo]?" question
-- They are found in a [[KeyPress]] bunch, each list representing a set of
-- answers which belong together
data KeyPress = KeyPress { kp     :: Char
                           , kpHelp :: String }

-- | Generates the help for a set of basic and advanced 'KeyPress' groups.
helpFor :: String -> [[KeyPress]] -> [[KeyPress]] -> String
helpFor jn basicKeypresses advancedKeyPresses =
  unlines $ [ "How to use "++jn++":" ]
            ++ intercalate [""] (map (map help) keypresses)
            ++ [ ""
               , "?: show this help"
               , ""
               , "<Space>: accept the current default (which is capitalized)"
               ]
  where help i = kp i:(": "++kpHelp i)
        keypresses = basicKeypresses ++ advancedKeyPresses

-- | The keys used by a list of 'keyPress' groups.
keysFor :: [[KeyPress]] -> [Char]
keysFor = concatMap (map kp)

-- | The function for selecting a patch to amend record. Read at your own risks.
withSelectedPatchFromList
    :: (Commute p, Matchable p, ShowPatch p, ShowContextPatch p, ApplyState p ~ Tree)
    => String   -- name of calling command (always "amend" as of now)
    -> RL p wO wR
    -> PatchSelectionOptions
    -> (forall wA . (FL p :> p) wA wR -> IO ())
    -> IO ()
withSelectedPatchFromList jn patches o job = do
    sp <- wspfr jn (matchAPatch $ matchFlags o) patches NilFL
    case sp of
        Just (FlippedSeal (skipped :> selected')) -> job (skipped :> selected')
        Nothing ->
            putStrLn $ "Cancelling " ++ jn ++ " since no patch was selected."

data SkippedReason = SkippedAutomatically | SkippedManually

data WithSkipped p wX wY = WithSkipped
    { _skippedReason :: SkippedReason
    , skippedPatch :: p wX wY
    }

-- | This ensures that the selected patch commutes freely with the skipped
-- patches, including pending and also that the skipped sequences has an
-- ending context that matches the recorded state, z, of the repository.
wspfr :: forall p wX wY wU.
         (Commute p, Matchable p, ShowPatch p, ShowContextPatch p, ApplyState p ~ Tree)
      => String
      -> (forall wA wB . p wA wB -> Bool)
      -> RL p wX wY
      -> FL (WithSkipped p) wY wU
      -> IO (Maybe (FlippedSeal (FL p :> p) wU))
wspfr _ _ NilRL _ = return Nothing
wspfr jn matches remaining@(pps:<:p) skipped
    | not $ matches p = wspfr jn matches pps
                            (WithSkipped SkippedAutomatically p :>: skipped)
    | otherwise =
    case commuteFL (p :> mapFL_FL skippedPatch skipped) of
    Nothing -> do putStrLn "\nSkipping depended-upon patch:"
                  defaultPrintFriendly p
                  wspfr jn matches pps (WithSkipped SkippedAutomatically p :>: skipped)

    Just (skipped' :> p') -> do
        defaultPrintFriendly p
        let repeatThis = do
              yorn <- promptChar
                    PromptConfig { pPrompt = prompt'
                                 , pBasicCharacters = keysFor basicOptions
                                 , pAdvancedCharacters = keysFor advancedOptions
                                 , pDefault = Just 'n'
                                 , pHelp = "?h" }
              case yorn of
                'y' -> return $ Just $ flipSeal $ skipped' :> p'
                'n' -> nextPatch
                'j' -> nextPatch
                'k' -> previousPatch remaining skipped
                'v' -> printContent p >> repeatThis
                'p' -> printContentWithPager p >> repeatThis
                'x' -> do printSummary p
                          repeatThis
                'r' -> defaultPrintFriendly p >> repeatThis
                'q' -> do putStrLn $ (capitalize jn) ++ " cancelled."
                          exitSuccess
                _   -> do putStrLn $ helpFor jn basicOptions advancedOptions
                          repeatThis
        repeatThis
  where prompt' = "Shall I " ++ jn ++ " this patch?"
        nextPatch = wspfr jn matches pps (WithSkipped SkippedManually p:>:skipped)
        previousPatch :: RL p wX wQ
                      -> FL (WithSkipped p) wQ wU
                      -> IO (Maybe (FlippedSeal
                              (FL p :> p) wU))
        previousPatch remaining' NilFL = wspfr jn matches remaining' NilFL
        previousPatch remaining' (WithSkipped sk prev :>: skipped'') =
            case sk of
                SkippedManually -> wspfr jn matches (remaining' :<: prev) skipped''
                SkippedAutomatically -> previousPatch (remaining' :<: prev) skipped''
        basicOptions =
                    [[ KeyPress 'y' (jn ++ " this patch")
                     , KeyPress 'n' ("don't " ++ jn ++ " it")
                     , KeyPress 'j' "skip to next patch"
                     , KeyPress 'k' "back up to previous patch"
                    ]]
        advancedOptions =
                    [[ KeyPress 'v' "view this patch in full"
                     , KeyPress 'p' "view this patch in full with pager"
                     , KeyPress 'x' "view a summary of this patch"
                     , KeyPress 'r' "view this patch"
                     , KeyPress 'q' ("cancel " ++ jn)
                    ]]
        defaultPrintFriendly =
          printFriendly Nothing NormalVerbosity NoSummary NoContext

-- | Runs a function on the underlying @PatchChoices@ object
liftChoices :: StateT (PatchChoices p wX wY) Identity a
            -> InteractiveSelectionM p wX wY a
liftChoices act = do
  ch <- gets choices
  let (result, _) = runIdentity $ runStateT act ch
  modify $ \isc -> isc {choices = ch} -- Should this be ch or the result of runState?
  return result

-- | @justDone n@ notes that @n@ patches have just been processed
justDone :: Int -> InteractiveSelectionM p wX wY ()
justDone n = modify $ \isc -> isc{ current = current isc + n}

initialSelectionState :: FL (LabelledPatch p) wX wY
                      -> PatchChoices p wX wY
                      -> InteractiveSelectionState p wX wY
initialSelectionState lps pcs =
  ISC { total = lengthFL lps
      , current = 0
      , lps = FZipper NilRL lps
      , choices = pcs
      }

-- | The actual interactive selection process.
textSelect :: ( Commute p, Invert p, ShowPatch p, ShowContextPatch p
              , PatchInspect p, ApplyState p ~ Tree )
           => FL (LabelledPatch p) wX wY
           -> PatchChoices p wX wY
           -> PatchSelectionM p IO (PatchChoices p wX wY)
textSelect lps' pcs =
  choices <$>
    execStateT (skipMundane >> printCurrent >> textSelectIfAny)
      (initialSelectionState lps' pcs)
  where
    textSelectIfAny = do
      z <- gets lps
      unless (rightmost z) $ textSelect'

textSelect' :: ( Commute p, Invert p, ShowPatch p, ShowContextPatch p
               , PatchInspect p, ApplyState p ~ Tree )
            => InteractiveSelectionM p wX wY ()
textSelect' = do
  z <- gets lps
  done <- if not $ rightmost z
           then textSelectOne
           else lastQuestion
  unless done $ textSelect'

optionsBasic :: String -> String -> [KeyPress]
optionsBasic jn aThing =
    [ KeyPress 'y' (jn++" this "++aThing)
    , KeyPress 'n' ("don't "++jn++" it")
    , KeyPress 'w' "wait and decide later, defaulting to no" ]

optionsFile :: String -> [KeyPress]
optionsFile jn =
    [ KeyPress 's' ("don't "++jn++" the rest of the changes to this file")
    , KeyPress 'f' (jn++" the rest of the changes to this file") ]

optionsView :: String -> String -> [KeyPress]
optionsView aThing someThings =
    [ KeyPress 'v' ("view this "++aThing++" in full")
    , KeyPress 'p' ("view this "++aThing++" in full with pager")
    , KeyPress 'r' ("view this "++aThing)
    , KeyPress 'l' ("list all selected "++someThings) ]

optionsSummary :: String -> [KeyPress]
optionsSummary aThing =
    [ KeyPress 'x' ("view a summary of this "++aThing) ]

optionsQuit :: String -> Bool -> String -> [KeyPress]
optionsQuit jn allowsa someThings =
    [ KeyPress 'd' (jn++" selected "++someThings++
                    ", skipping all the remaining "++someThings)
            | allowsa ]
    ++
    [ KeyPress 'a' (jn++" all the remaining "++someThings)
    , KeyPress 'q' ("cancel "++jn) ]

optionsNav :: String -> Bool -> [KeyPress]
optionsNav aThing isLast=
    [ KeyPress 'j' ("skip to next "++ aThing) | not isLast ]
    ++
    [ KeyPress 'k' ("back up to previous "++ aThing)
    , KeyPress 'g' ("start over from the first "++aThing)]

optionsSplit :: Maybe (Splitter a) -> String -> [KeyPress]
optionsSplit split aThing
    | Just _ <- split
             = [ KeyPress 'e' ("interactively edit this "++ aThing) ]
    | otherwise = []

optionsLast :: String -> String -> ([[KeyPress]], [[KeyPress]])
optionsLast jn aThing =
  (optionsNav aThing True:
   [[ KeyPress 'y' "confirm this operation"
    , KeyPress 'q' ("cancel " ++ jn) ]
    , [ KeyPress 'l' "list all selected" ]
   ]
  ,[[KeyPress 'a' "confirm this operation"
    , KeyPress 'd' "confirm this operation"
    , KeyPress 'n' ("cancel " ++ jn) ]])

options :: (ShowPatch p)
        => Bool
        -> InteractiveSelectionM p wX wY ([[KeyPress]],[[KeyPress]])
options single = do
  split <- asks splitter
  jn <- asks jobname
  allowsa <- asks allowSkipAll
  aThing <- thing
  someThings <- things
  o <- asks opts
  return ([optionsBasic jn aThing]
         ,[optionsSplit split aThing]
         ++ [optionsFile jn | single]
         ++ [optionsView aThing someThings ++
                if withSummary o == YesSummary
                    then []
                    else optionsSummary aThing]
         ++ [optionsQuit jn allowsa someThings]
         ++ [optionsNav aThing False]
         )

-- | Returns a 'Sealed2' version of the patch we are asking the user
-- about.
currentPatch :: InteractiveSelectionM p wX wY (Maybe (Sealed2 (LabelledPatch p)))
currentPatch = focus <$> gets lps

-- | Returns the patches we have yet to ask the user about.
todo :: InteractiveSelectionM p wX wY (FlippedSeal (FL (LabelledPatch p)) wY)
todo = jokers <$> gets lps

-- | Modify the underlying @PatchChoices@ by some function
modifyChoices :: (PatchChoices p wX wY -> PatchChoices p wX wY)
              -> InteractiveSelectionM p wX wY ()
modifyChoices f = modify $ \isc -> isc{choices = f $ choices isc}

-- | returns @Just f@ if the 'currentPatch' only modifies @f@,
-- @Nothing@ otherwise.
currentFile :: (PatchInspect p)
            => InteractiveSelectionM p wX wY (Maybe AnchoredPath)
currentFile = do
  c <- currentPatch
  return $ case c of
             Nothing -> Nothing
             Just (Sealed2 lp) ->
                 case listTouchedFiles lp of
                   [f] -> Just f
                   _ -> Nothing

-- | @decide True@ selects the current patch, and @decide False@ deselects
-- it.
decide :: Commute p
       => Bool
       -> LabelledPatch p wT wU
       -> InteractiveSelectionM p wX wY ()
decide takeOrDrop lp = do
    whch <- asks whichChanges
    if backward whch == takeOrDrop -- we go backward xor we are dropping
    then modifyChoices $ forceLast (label lp)
    else modifyChoices $ forceFirst (label lp)

-- | like 'decide', but for all patches touching @file@
decideWholeFile :: (Commute p, PatchInspect p)
                => AnchoredPath -> Bool -> InteractiveSelectionM p wX wY ()
decideWholeFile path takeOrDrop =
    do
      FlippedSeal lps_todo <- todo
      let patches_to_skip =
              filterFL (\lp' -> listTouchedFiles lp' == [path]) lps_todo
      mapM_ (unseal2 $ decide takeOrDrop) patches_to_skip

-- | Undecide the current patch.
postponeNext :: Commute p => InteractiveSelectionM p wX wY ()
postponeNext =
    do
      Just (Sealed2 lp) <- currentPatch
      modifyChoices $ forceMiddle (label lp)

-- | Focus the next patch.
skipOne :: InteractiveSelectionM p wX wY ()
skipOne = modify so
    where so x = x{lps = right (lps x), current = current x +1}

-- | Focus the previous patch.
backOne :: InteractiveSelectionM p wX wY ()
backOne = modify so
    where so isc = isc{lps = left (lps isc), current = max (current isc-1) 0}

-- | Split the current patch (presumably a hunk), and add the replace it
-- with its parts.
splitCurrent :: Splitter p
             -> InteractiveSelectionM p wX wY ()
splitCurrent s = do
    FZipper lps_done (lp:>:lps_todo) <- gets lps
    case applySplitter s (unLabel lp) of
      Nothing -> return ()
      Just (text, parse) ->
          do
            newText <- liftIO $ editText "darcs-patch-edit" text
            case parse newText of
               Nothing -> return ()
               Just ps -> do
                 lps_new <- liftIO $ return $ labelPatches (Just (label lp)) ps
                 modify $ \isc -> isc { total = total isc + lengthFL lps_new - 1
                                      , lps = FZipper lps_done
                                               (lps_new +>+ lps_todo)
                                      , choices = substitute
                                                   (seal2 (lp :||: lps_new))
                                                   (choices isc)
                                      }

-- | Print the list of the selected patches. We currently choose to display
-- them in "commuted" form, that is, in the order in which they have been
-- selected and with deselected patches moved out of the way.
printSelected :: (Commute p, ShowPatch p) => InteractiveSelectionM p wX wY ()
printSelected = do
  someThings <- things
  o <- asks opts
  w <- asks whichChanges
  let showFL = vcat . mapFL (showFriendly (verbosity o) (withSummary o) . unLabel)
  (first_chs :> _ :> last_chs) <- getChoices <$> gets choices
  liftIO $ putDocLnWith fancyPrinters $ vcat
    [ greenText $ "---- selected "++someThings++" ----"
    , if backward w then showFL last_chs else showFL first_chs
    , greenText $ "---- end of selected "++someThings++" ----"
    ]

-- | Skips all remaining patches.
skipAll ::  InteractiveSelectionM p wX wY ()
skipAll = modify $ \isc -> isc {lps = toEnd $ lps isc}

backAll ::  InteractiveSelectionM p wX wY ()
backAll = modify $ \isc -> isc {lps = toStart $ lps isc
                               ,current = 0}

isSingleFile :: PatchInspect p => p wX wY -> Bool
isSingleFile p = length (listTouchedFiles p) == 1

askConfirmation ::  InteractiveSelectionM p wX wY ()
askConfirmation = do
    jn <- asks jobname
    liftIO $ when (jn `elem` ["unpull", "unrecord", "obliterate"]) $ do
               yorn <- askUser $ "Really " ++ jn ++ " all undecided patches? "
               case yorn of
                 ('y':_) -> return ()
                 _ -> exitSuccess

-- | The singular form of the noun for items of type @p@.
thing :: (ShowPatch p) => InteractiveSelectionM p wX wY String
thing = (Darcs.Patch.thing . helper) `liftM` gets choices
        where
          helper :: PatchChoices p wA wB -> p wA wB
          helper = undefined

-- | The plural form of the noun for items of type @p@.
things :: (ShowPatch p) => InteractiveSelectionM p wX wY String
things = (Darcs.Patch.things . helper) `liftM` gets choices
        where
          helper :: PatchChoices p wA wB -> p wA wB
          helper = undefined

-- | The question to ask about one patch.
prompt :: (ShowPatch p) => InteractiveSelectionM p wX wY String
prompt = do
  jn <- asks jobname
  aThing <- thing
  n <- gets current
  n_max <- gets total
  return $ "Shall I "++jn++" this "++aThing++"? "
             ++ "(" ++ show (n+1) ++ "/" ++ show n_max ++ ") "

-- | Asks the user about one patch, returns their answer.
promptUser :: (ShowPatch p)
           => Bool -> Char -> InteractiveSelectionM p wX wY Char
promptUser single def = do
  thePrompt <- prompt
  (basicOptions,advancedOptions) <- options single
  liftIO $ promptChar PromptConfig { pPrompt = thePrompt
                                   , pBasicCharacters = keysFor basicOptions
                                   , pAdvancedCharacters = keysFor advancedOptions
                                   , pDefault = Just def
                                   , pHelp = "?h"
                                   }

-- | Ask the user what to do with the next patch.
textSelectOne :: ( Commute p, ShowPatch p, ShowContextPatch p, PatchInspect p
                 , ApplyState p ~ Tree )
              => InteractiveSelectionM p wX wY Bool
textSelectOne = do
 c <- currentPatch
 case c of
   Nothing -> return False
   Just (Sealed2 lp) ->
       do
         jn <- asks jobname
         spl <- asks splitter
         whichch <- asks whichChanges
         let singleFile = isSingleFile (unLabel lp)
             p = unLabel lp
         (basicOptions,advancedOptions) <- options singleFile
         theSlot <- liftChoices $ state $ patchSlot lp
         let the_default = getDefault (backward whichch) theSlot
         yorn <- promptUser singleFile the_default
         let nextPatch = skipMundane >> printCurrent
         case yorn of
               'y' -> decide True lp >> skipOne >> nextPatch
                      >> return False
               'n' -> decide False lp >> skipOne >> nextPatch
                      >> return False
               'w' -> postponeNext >> skipOne >> nextPatch
                      >> return False
               'e' | (Just s) <- spl -> splitCurrent s >> printCurrent
                                        >> return False
               's' -> currentFile >>= maybe
                       (return ())
                       (\f -> decideWholeFile f False) >> nextPatch
                       >> return False
               'f' -> currentFile >>= maybe
                       (return ())
                       (\f -> decideWholeFile f True) >> nextPatch
                       >> return False
               'v' -> liftIO $ printContent p >> return False
               'p' -> liftIO $ printContentWithPager p >> return False
               'r' -> printCurrent >> return False
               'l' -> printSelected >> printCurrent >> return False
               'x' -> liftIO $ printSummary p >> return False
               'd' -> skipAll >> return True
               'g' -> backAll >> printCurrent >> return False
               'a' ->
                   do
                     askConfirmation
                     modifyChoices $ selectAllMiddles (backward whichch)
                     skipAll
                     return True
               'q' -> liftIO $
                      do putStrLn $ capitalize jn ++ " cancelled."
                         exitSuccess
               'j' -> skipOne >> printCurrent >> return False
               'k' -> backOne >> printCurrent >> return False
               _   -> do
                 liftIO . putStrLn $ helpFor jn basicOptions advancedOptions
                 return False

lastQuestion :: (Commute p, ShowPatch p, ShowContextPatch p, ApplyState p ~ Tree)
             => InteractiveSelectionM p wX wY Bool
lastQuestion = do
  jn <- asks jobname
  theThings <-things
  aThing <- thing
  let (basicOptions, advancedOptions) = optionsLast jn aThing
  yorn <- liftIO . promptChar $
            PromptConfig { pPrompt = "Do you want to "++capitalize jn++
                                      " these "++theThings++"?"
                         , pBasicCharacters = "yglqk"
                         , pAdvancedCharacters = "dan"
                         , pDefault = Just 'y'
                         , pHelp = "?h"}
  case yorn of c | c `elem` "yda" -> return True
                 | c `elem` "qn" -> liftIO $
                                    do putStrLn $ jn ++" cancelled."
                                       exitSuccess
               'g' -> backAll >> printCurrent >> return False
               'l' -> printSelected >> return False
               'k' -> backOne >> printCurrent >> return False
               _ -> do
                 liftIO . putStrLn $ helpFor "this confirmation prompt"
                    basicOptions advancedOptions
                 return False

-- | Shows the current patch as it should be seen by the user.
printCurrent :: (ShowPatch p, ShowContextPatch p, ApplyState p ~ Tree)
             => InteractiveSelectionM p wX wY ()
printCurrent = do
  o <- asks opts
  pr <- asks pristine
  c <- currentPatch
  case c of
    Nothing -> return ()
    Just (Sealed2 lp) ->
      liftIO $ printFriendly pr (verbosity o) (withSummary o) (withContext o) $ unLabel lp

-- | The interactive part of @darcs changes@
textView :: (ShowPatch p, ShowContextPatch p, ApplyState p ~ Tree)
         => PatchSelectionOptions -> Maybe Int -> Int
         -> [Sealed2 p] -> [Sealed2 p]
         -> IO ()
textView _ _ _ _ [] = return ()
textView o n_max n
            ps_done ps_todo@(p:ps_todo') = do
      defaultPrintFriendly p
      repeatThis -- prompt the user
    where
        defaultPrintFriendly =
          unseal2 (printFriendly Nothing (verbosity o) (withSummary o) (withContext o))
        prev_patch :: IO ()
        prev_patch = case ps_done of
                       [] -> repeatThis
                       (p':ps_done') ->
                         textView o
                            n_max (n-1) ps_done' (p':ps_todo)
        next_patch :: IO ()
        next_patch = case ps_todo' of
                         [] -> -- May as well work out the length now we have all
                                  -- the patches in memory
                               textView o n_max
                                   n ps_done []
                         _ -> textView o n_max
                                  (n+1) (p:ps_done) ps_todo'
        first_patch = textView o n_max 0 [] (ps_done++ps_todo)
        options_yn =
          [ KeyPress 'y' "view this patch and go to the next"
          , KeyPress 'n' "skip to the next patch" ]
        optionsView' =
          [ KeyPress 'v' "view this patch in full"
          , KeyPress 'p' "view this patch in full with pager"
          , KeyPress 'r' "view this patch" ]
        optionsSummary' =
          [ KeyPress 'x' "view a summary of this patch" ]
        optionsNav' =
          [ KeyPress 'q' "quit view changes"
          , KeyPress 'k' "back up to previous patch"
          , KeyPress 'j' "skip to next patch"
          , KeyPress 'g' "start over from the first patch"
          , KeyPress 'c' "count total patch number" ]
        basicOptions = [ options_yn ]
        advancedOptions =
                     (optionsView' ++
                        if withSummary o == YesSummary then [] else optionsSummary')
                  : [ optionsNav' ]
        prompt' = "Shall I view this patch? "
               ++ "(" ++ show (n+1) ++ "/" ++ maybe "?" show n_max ++ ")"
        repeatThis :: IO ()
        repeatThis = do
          yorn <- promptChar (PromptConfig prompt' (keysFor basicOptions) (keysFor advancedOptions) (Just 'n') "?h")
          case yorn of
            'y' -> unseal2 printContent p >> next_patch
            'n' -> next_patch
            'v' -> unseal2 printContent p >> repeatThis
            'p' -> unseal2 printContentWithPager p >> repeatThis
            'r' -> do defaultPrintFriendly p
                      repeatThis
            'x' -> do unseal2 printSummary p
                      repeatThis
            'q' -> exitSuccess
            'k' -> prev_patch
            'j' -> next_patch
            'g' -> first_patch
            'c' -> textView o
                       count_n_max n ps_done ps_todo
            _   -> do putStrLn $ helpFor "view changes" basicOptions advancedOptions
                      repeatThis
        count_n_max | isJust n_max = n_max
                    | otherwise    = Just $ length ps_done + length ps_todo

-- | Skips patches we should not ask the user about
skipMundane :: (Commute p, ShowPatch p)
            => InteractiveSelectionM p wX wY ()
skipMundane = do
  (FZipper lps_done lps_todo) <- gets lps
  o <- asks opts
  crit <- asks matchCriterion
  jn <- asks jobname
  (skipped :> unskipped) <- liftChoices $ spanFL_M
                                 (state . patchSlot >=> return . decided)
                                 lps_todo
  let numSkipped = lengthFL skipped
  when (numSkipped > 0) . liftIO $ show_skipped o jn numSkipped skipped
  let boringThenInteresting =
          if selectDeps o == AutoDeps
          then spanFL (not . mcFunction crit . unLabel) unskipped
          else NilFL :> unskipped
  case boringThenInteresting of
    boring :> interesting ->
        do
          justDone $ lengthFL boring + numSkipped
          modify $ \isc -> isc {lps = FZipper (lps_done +<<+ skipped +<<+ boring)
                                      interesting}
    where
      show_skipped o jn n ps = do putStrLn $ _nevermind_ jn ++ _these_ n ++ "."
                                  when (verbosity o == Verbose) $
                                       showskippedpatch ps
      _nevermind_ jn = "Will not ask whether to " ++ jn ++ " "
      _these_ n  = show n ++ " already decided " ++ _elem_ n ""
      _elem_ n = englishNum n (Noun "patch")
      showskippedpatch :: ShowPatch p => FL (LabelledPatch p) wY wT -> IO ()
      showskippedpatch =
        putDocLnWith fancyPrinters . vcat . mapFL (showFriendly NormalVerbosity NoSummary . unLabel)

decided :: Slot -> Bool
decided InMiddle = False
decided _ = True

-- | The action bound to space, depending on the current status of the
-- patch.
getDefault :: Bool -> Slot -> Char
getDefault _ InMiddle = 'w'
getDefault True InFirst  = 'n'
getDefault True InLast   = 'y'
getDefault False InFirst = 'y'
getDefault False InLast  = 'n'

askAboutDepends :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                => Repository rt p wR wU wT -> FL (PrimOf p) wT wY
                -> PatchSelectionOptions
                -> [PatchInfo] -> IO [PatchInfo]
askAboutDepends repository pa' ps_opts olddeps = do
  -- Ideally we'd just default the olddeps to yes but still ask about them.
  -- SelectChanges doesn't currently (17/12/09) offer a way to do this so would
  -- have to have this support added first.
  pset <- readTentativeRepo repository (repoLocation repository)
  -- Let the user select only from patches after the last clean tag.
  -- We do this for efficiency, otherwise independentPatchIds can
  -- take a /very/ long time to finish. The limitation this imposes
  -- is a bit arbitrary from a user perspective. Note however that
  -- contextPatches at least gives us this latest clean tag to select.
  _ :> untagged <- return $ contextPatches pset
  -- Note: using anonymous here seems to be safe since we don't store any patches
  -- and only return a list of PatchInfo
  pa <- n2pia . flip adddeps olddeps <$> anonymous pa'
  -- get rid of all (implicit and explicit) dependencies of pa
  _ :> _ :> non_deps <- return $ commuteWhatWeCanRL (untagged :> pa)
  candidates :> _ <-
    runSelection (reverseRL non_deps) $
      selectionConfig FirstReversed "depend on" ps_opts
        { matchFlags = [], interactive = True } Nothing Nothing
  return $ olddeps `union` independentPatchIds (reverseFL candidates)

-- | From an 'RL' of patches select the identities of those that are
-- not depended upon by later patches.
independentPatchIds :: (Commute p, Ident p) => RL p wX wY -> [PatchId p]
independentPatchIds NilRL = []
independentPatchIds (ps :<: p) =
  case commuteWhatWeCanRL (ps :> p) of
    _ :> _ :> non_deps ->
      ident p : independentPatchIds non_deps
