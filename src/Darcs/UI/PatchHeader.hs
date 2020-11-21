module Darcs.UI.PatchHeader
    ( getLog
    , getAuthor
    , updatePatchHeader, AskAboutDeps(..)
    , HijackT, HijackOptions(..)
    , runHijackT
    ) where

import Darcs.Prelude

import Darcs.Patch
    ( IsRepoType, RepoPatch, PrimPatch, PrimOf
    , summaryFL
    )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Info ( PatchInfo,
                          piAuthor, piName, piLog, piDateString,
                          patchinfo
                        )
import Darcs.Patch.Named
   ( Named, patchcontents, patch2patchinfo, infopatch, getdeps, adddeps
   )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Patch.Prim ( canonizeFL )

import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+) )

import Darcs.Repository ( Repository )
import Darcs.Util.Lock
    ( readTextFile
    , writeTextFile
    )

import Darcs.UI.External ( editFile )
import Darcs.UI.Flags ( getEasyAuthor, promptAuthor, getDate )
import qualified Darcs.UI.Options.All as O
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.UI.SelectChanges ( askAboutDepends )

import qualified Darcs.Util.Diff as D ( DiffAlgorithm )
import Darcs.Util.English ( capitalize )
import Darcs.Util.Global ( darcsLastMessage )
import Darcs.Util.Path ( FilePathLike, toFilePath )
import Darcs.Util.Prompt ( PromptConfig(..), askUser, promptChar, promptYorn )
import Darcs.Util.Printer ( text, ($$), vcat, prefixLines, renderString )
import qualified Darcs.Util.Ratified as Ratified ( hGetContents )

import Darcs.Util.Tree ( Tree )

import Control.Exception ( catch, IOException )
import Control.Monad ( when, void )
import Control.Monad.Trans              ( liftIO )
import Control.Monad.Trans.State.Strict ( StateT(..), evalStateT, get, put  )
import Data.List ( isPrefixOf, stripPrefix )
import Data.Maybe ( fromMaybe, isJust )
import System.Exit ( exitSuccess )
import System.IO ( stdin )

data PName = FlagPatchName String | PriorPatchName String | NoPatchName

-- | Options for how to deal with the situation where we are somehow
--   modifying a patch that is not our own
data HijackOptions = IgnoreHijack                  -- ^ accept all hijack requests
                   | RequestHijackPermission       -- ^ prompt once, accepting subsequent hijacks if yes
                   | AlwaysRequestHijackPermission -- ^ always prompt

-- | Transformer for interactions with a hijack warning state that we
--   need to thread through
type HijackT = StateT HijackOptions

-- | Get the patch name and long description from one of
--
--  * the configuration (flags, defaults, hard-coded)
--
--  * an existing log file
--
--  * stdin (e.g. a pipe)
--
--  * a text editor
--
-- It ensures the patch name is not empty nor starts with the prefix TAG.
--
-- The last result component is a possible path to a temporary file that should be removed later.
getLog :: forall prim wX wY . PrimPatch prim
       => Maybe String                          -- ^ patchname option
       -> Bool                                  -- ^ pipe option
       -> O.Logfile                             -- ^ logfile option
       -> Maybe O.AskLongComment                -- ^ askLongComment option
       -> Maybe (String, [String])              -- ^ possibly an existing patch name and long description
       -> FL prim wX wY                         -- ^ changes to record
       -> IO (String, [String], Maybe String)   -- ^ patch name, long description and possibly the path
                                                --   to the temporary file that should be removed later
getLog m_name has_pipe log_file ask_long m_old chs =
  restoreTagPrefix <$> go has_pipe log_file ask_long
 where
  go True _ _ = do
      p <- case patchname_specified of
             FlagPatchName p  -> check_badname p >> return p
             PriorPatchName p -> return p
             NoPatchName      -> prompt_patchname False
      putStrLn "What is the log?"
      thelog <- lines `fmap` Ratified.hGetContents stdin
      return (p, thelog, Nothing)
  go _ (O.Logfile { O._logfile = Just f }) _ = do
      mlp <- readTextFile f `catch` (\(_ :: IOException) -> return [])
      firstname <- case (patchname_specified, mlp) of
                     (FlagPatchName  p, []) -> check_badname p >> return p
                     (_, p:_)               -> if is_badname p
                                                 then prompt_patchname True
                                                 else return p -- logfile trumps prior!
                     (PriorPatchName p, []) -> return p
                     (NoPatchName, [])      -> prompt_patchname True
      append_info f firstname
      when (ask_long == Just O.YesEditLongComment) (void $ editFile f)
      (name, thelog) <- read_long_comment f firstname
      return (name, thelog, if O._rmlogfile log_file then Just $ toFilePath f else Nothing)
  go _ _ (Just O.YesEditLongComment) =
      case patchname_specified of
          FlagPatchName  p  -> get_log_using_editor p
          PriorPatchName p  -> get_log_using_editor p
          NoPatchName       -> get_log_using_editor ""
  go _ _ (Just O.NoEditLongComment) =
      case patchname_specified of
          FlagPatchName  p  -> check_badname p >> return (p, default_log, Nothing) -- record (or amend) -m
          PriorPatchName p  -> return (p, default_log, Nothing) -- amend
          NoPatchName       -> do p <- prompt_patchname True -- record
                                  return (p, [], Nothing)
  go _ _ (Just O.PromptLongComment) =
      case patchname_specified of
          FlagPatchName p   -> check_badname p >> prompt_long_comment p -- record (or amend) -m
          PriorPatchName p  -> prompt_long_comment p
          NoPatchName       -> prompt_patchname True >>= prompt_long_comment
  go _ _ Nothing =
      case patchname_specified of
          FlagPatchName  p  -> check_badname p >> return (p, default_log, Nothing)  -- record (or amend) -m
          PriorPatchName "" -> get_log_using_editor ""
          PriorPatchName p  -> return (p, default_log, Nothing)
          NoPatchName       -> get_log_using_editor ""

  tagPrefix = "TAG "

  hasTagPrefix name = tagPrefix `isPrefixOf` name

  restoreTagPrefix (name, log, file)
    | Just (old_name, _) <- m_old
    , hasTagPrefix old_name = (tagPrefix ++ name, log, file)
  restoreTagPrefix args = args

  stripTagPrefix name = fromMaybe name $ stripPrefix tagPrefix name

  patchname_specified =
    case (m_name, m_old) of
      (Just name, _)              -> FlagPatchName name
      (Nothing,   Just (name, _)) -> PriorPatchName (stripTagPrefix name)
      (Nothing,   Nothing)        -> NoPatchName

  default_log = case m_old of
                  Nothing    -> []
                  Just (_,l) -> l

  check_badname = maybe (return ()) fail . just_a_badname

  prompt_patchname retry = do
      n <- askUser "What is the patch name? "
      maybe (return n) prompt_again $ just_a_badname n
    where
      prompt_again msg = do
        putStrLn msg
        if retry then prompt_patchname retry else fail "Bad patch name!"

  just_a_badname n =
    if null n then
      Just "Error: The patch name must not be empty!"
    else if hasTagPrefix n then
      Just "Error: The patch name must not start with \"TAG \"!"
    else
      Nothing

  is_badname = isJust . just_a_badname

  prompt_long_comment oldname =
    do let verb = case m_old of Nothing -> "add a"; Just _ -> "edit the"
       y <- promptYorn $ "Do you want to "++verb++" long comment?"
       if y then get_log_using_editor oldname
            else return (oldname, default_log, Nothing)

  get_log_using_editor p =
                       do let logf = darcsLastMessage
                          writeTextFile logf $ unlines $ p : default_log
                          append_info logf p
                          _ <- editFile logf
                          (name,long) <- read_long_comment logf p
                          check_badname name
                          return (name,long,Just logf)

  read_long_comment :: FilePathLike p => p -> String -> IO (String, [String])
  read_long_comment f oldname =
      do t <- readTextFile f
         let filter_out_info = filter (not.("#" `isPrefixOf`))
         case reverse $ dropWhile null $ reverse $ filter_out_info t of
            []     -> return (oldname, [])
            (n:ls) -> do
                check_badname n
                return (n, ls)

  append_info f oldname = do
    fc <- readTextFile f
    writeTextFile f $ renderString
       $ vcat (map text $ if null fc then [oldname] else fc)
      $$ text "# Please enter the patch name in the first line, and"
      $$ text "# optionally, a long description in the following lines."
      $$ text "#"
      $$ text "# Lines starting with '#' will be ignored."
      $$ text "#"
      $$ text "#"
      $$ text "# Summary of selected changes:"
      $$ text "#"
      $$ prefixLines (text "#") (summaryFL chs)

-- |specify whether to ask about dependencies with respect to a particular repository, or not
data AskAboutDeps rt p wR wU wT = AskAboutDeps (Repository rt p wR wU wT) | NoAskAboutDeps

-- | Run a job that involves a hijack confirmation prompt.
--
--   See 'RequestHijackPermission' for initial values
runHijackT :: Monad m => HijackOptions -> HijackT m a -> m a
runHijackT = flip evalStateT

-- | Update the metadata for a patch.
--   This potentially involves a bit of interactivity, so we may return @Nothing@
--   if there is cause to abort what we're doing along the way
updatePatchHeader :: forall rt p wX wY wR wU wT . (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                  => String -- ^ verb: command name
                  -> AskAboutDeps rt p wR wU wT
                  -> S.PatchSelectionOptions
                  -> D.DiffAlgorithm
                  -> Bool -- keepDate
                  -> Bool -- selectAuthor
                  -> Maybe String -- author
                  -> Maybe String -- patchname
                  -> Maybe O.AskLongComment
                  -> Named (PrimOf p) wT wX
                  -- ^ patch to edit, must be conflict-free as conflicts can't be preserved when changing
                  -- the identity of a patch. If necessary this can be achieved by calling @fmapFL_Named effect@
                  -- on an @Named p@ first, but some callers might already have @Named (PrimOf p)@ available.
                  -> FL (PrimOf p) wX wY -- ^new primitives to add
                  -> HijackT IO (Maybe String, PatchInfoAnd rt p wT wY)
updatePatchHeader verb ask_deps pSelOpts da nKeepDate nSelectAuthor nAuthor nPatchname nAskLongComment oldp chs = do

    let newchs = canonizeFL da (patchcontents oldp +>+ chs)

    let old_pdeps = getdeps oldp
    newdeps <-
        case ask_deps of
           AskAboutDeps repository -> liftIO $ askAboutDepends repository newchs pSelOpts old_pdeps
           NoAskAboutDeps -> return old_pdeps

    let old_pinf = patch2patchinfo oldp
        prior    = (piName old_pinf, piLog old_pinf)
    date <- if nKeepDate then return (piDateString old_pinf) else liftIO $ getDate False
    new_author <- getAuthor verb nSelectAuthor nAuthor old_pinf
    liftIO $ do
        (new_name, new_log, mlogf) <- getLog
            nPatchname False (O.Logfile Nothing False) nAskLongComment (Just prior) chs
        new_pinf <- patchinfo date new_name new_author new_log
        let newp = n2pia (adddeps (infopatch new_pinf newchs) newdeps)
        return (mlogf, newp)


-- | @getAuthor@ tries to return the updated author for the patch.
--   There are two different scenarios:
--
--   * [explicit] Either we want to override the patch author, be it by
--     prompting the user (@select@) or having them pass it in from
--     the UI (@new_author@), or
--
--   * [implicit] We want to keep the original author, in which case we
--     also double-check that we are not inadvertently \"hijacking\"
--     somebody else's patch (if the patch author is not the same as the
--     repository author, we give them a chance to abort the whole
--     operation)
getAuthor :: String          -- ^ verb:   command name
          -> Bool            -- ^ select: prompt for new auhor
          -> Maybe String    -- ^ new author: explict new author
          -> PatchInfo       -- ^ patch to update
          -> HijackT IO String
getAuthor _ True  _ _  = do
    auth <- liftIO $ promptAuthor False True
    return auth
getAuthor _    False (Just new) _   =
    return new
getAuthor verb False Nothing pinfo = do
    whitelist <- liftIO $ getEasyAuthor
    hj <- get
    if orig `elem` whitelist || canIgnore hj
        then allowHijack
        else do
            hijackResp <- liftIO $ askAboutHijack hj
            case hijackResp of
                'y' -> allowHijack
                'a' -> put IgnoreHijack >> allowHijack
                _   -> liftIO exitSuccess
  where
    askAboutHijack hj = promptChar (PromptConfig msg opts [] Nothing [])
       where
         msg  = "You're not " ++ orig ++"! " ++ capitalize verb ++ " anyway? "
         opts = case hj of
             AlwaysRequestHijackPermission -> "yn"
             _ -> "yna"
    canIgnore IgnoreHijack                  = True
    canIgnore RequestHijackPermission       = False
    canIgnore AlwaysRequestHijackPermission = False
    allowHijack = return orig
    orig = piAuthor pinfo
