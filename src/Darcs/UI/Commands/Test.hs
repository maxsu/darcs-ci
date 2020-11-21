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

module Darcs.UI.Commands.Test
    (
      test
    ) where

import Darcs.Prelude hiding ( init )

import Control.Exception ( catch, IOException )
import Control.Monad( when )

import System.Process ( system )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( hFlush, stdout )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , nodefaults
    , putInfo
    , amInHashedRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, useCache, verbosity )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Repository (
                          readRepo
                        , withRepository
                        , RepoJob(..)
                        , withRecorded
                        , setScriptsExecutablePatches
                        , setScriptsExecutable
                        )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..)
    , (:>)(..)
    , (+<+)
    , reverseRL
    , splitAtRL
    , lengthRL
    , mapRL
    , mapFL
    , mapRL_RL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..) )
import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch ( RepoPatch
                   , description
                   )
import Darcs.Patch.Named ( Named )
import Darcs.Patch.Set ( patchSet2RL )
import Darcs.Util.Printer ( Doc, putDocLn, text )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Repository.ApplyPatches ( DefaultIO, runDefault )
import Darcs.Repository.Test ( getTest )
import Darcs.Util.Lock
    ( withTempDir
    , withPermDir
    )


testDescription :: String
testDescription = "Run tests and search for the patch that introduced a bug."

testHelp :: Doc
testHelp = text $
 unlines
 [ "Run test on the current recorded state of the repository.  Given no"
  ,"arguments, it uses the default repository test (see `darcs setpref`)."
  ,"Given one argument, it treats it as a test command."
  ,"Given two arguments, the first is an initialization command and the"
  ,"second is the test (meaning the exit code of the first command is not"
  ,"taken into account to determine success of the test)."
  ,"If given the `--linear` or `--bisect` flags, it tries to find the most"
  ,"recent version in the repository which passes a test."
  ,""
  ,"`--linear` does linear search starting from head, and moving away"
  ,"from head. This strategy is best when the test runs very quickly"
  ,"or the patch you're seeking is near the head."
  ,""
  ,"`--bisect` does binary search.  This strategy is best when the test"
  ,"runs very slowly or the patch you're seeking is likely to be in"
  ,"the repository's distant past."
  ,""
  ,"`--backoff` starts searching from head, skipping further and further"
  ,"into the past until the test succeeds.  It then does a binary search"
  ,"on a subset of those skipped patches.  This strategy works well unless"
  ,"the patch you're seeking is in the repository's distant past."
  ,""
  ,"Under the assumption that failure is monotonous, `--linear` and"
  ,"`--bisect` produce the same result.  (Monotonous means that when moving"
  ,"away from head, the test result changes only once from \"fail\" to"
  ,"\"ok\".)  If failure is not monotonous, any one of the patches that"
  ,"break the test is found at random."
 ]

test :: DarcsCommand
test = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "test"
    , commandHelp = testHelp
    , commandDescription = testDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[[INITIALIZATION]", "COMMAND]"]
    , commandCommand = testCommand
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc testAdvancedOpts
    , commandBasicOptions = odesc testBasicOpts
    , commandDefaults = defaultFlags testOpts
    , commandCheckOptions = ocheck testOpts
    }
  where
    testBasicOpts = O.testStrategy ^ O.leaveTestDir ^ O.repoDir
    testAdvancedOpts = O.setScriptsExecutable
    testOpts = testBasicOpts `withStdOpts` testAdvancedOpts

data TestResult = Success | Failure Int

data SearchTypeResult = AssumedMonotony | WasLinear

data StrategyResult p =
    StrategySuccess -- the initial run of the test passed
  | NoPasses
  | PassesOnHead
  | Blame SearchTypeResult (Sealed2 (Named p))
  -- these two are just for oneTest
  | RunSuccess
  | RunFailed Int

-- | Functions defining a strategy for executing a test
type Strategy = forall p wX wY
               . (RepoPatch p, ApplyMonad (ApplyState p) DefaultIO)
              => [DarcsFlag]
              -> IO TestResult  -- ^ test command
              -> TestResult
              -> RL (Named p) wX wY
              -> IO (StrategyResult p)

testCommand :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
testCommand _ opts args =
 withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  patches <- readRepo repository
  (init,testCmd) <- case args of
    [] ->
      do t <- getTest (verbosity ? opts)
         return (return ExitSuccess, exitCodeToTestResult <$> t)
    [cmd] ->
      do putStrLn $ "Using test command:\n"++cmd
         return (return ExitSuccess, exitCodeToTestResult <$> system cmd)
    [init,cmd] ->
      do putStrLn $ "Using initialization command:\n"++init
         putStrLn $ "Using test command:\n"++cmd
         return (system init, exitCodeToTestResult <$> system cmd)
    _ -> fail "Test expects zero to two arguments."
  let wd = case O.leaveTestDir ? opts of
            O.YesLeaveTestDir -> withPermDir
            O.NoLeaveTestDir -> withTempDir
  withRecorded repository (wd "testing") $ \_ -> do
    when (O.yes (O.setScriptsExecutable ? opts)) setScriptsExecutable
    _ <- init
    putInfo opts $ text "Running test...\n"
    testResult <- testCmd
    let track = chooseStrategy (O.testStrategy ? opts)
    result <- track opts testCmd testResult (mapRL_RL hopefully . patchSet2RL $ patches)
    case result of
      StrategySuccess -> putStrLn "Success!"
      NoPasses -> putStrLn "Noone passed the test!"
      PassesOnHead -> putStrLn "Test does not fail on head."
      Blame searchTypeResult (Sealed2 p) -> do
        let extraText =
              case searchTypeResult of
                AssumedMonotony -> " (assuming monotony in the given range)"
                WasLinear -> ""
        putStrLn ("Last recent patch that fails the test" ++ extraText ++ ":")
        putDocLn (description p)
      RunSuccess -> putInfo opts $ text "Test ran successfully.\n"
      RunFailed n -> do
        putInfo opts $ text "Test failed!\n"
        exitWith (ExitFailure n)

exitCodeToTestResult :: ExitCode -> TestResult
exitCodeToTestResult ExitSuccess = Success
exitCodeToTestResult (ExitFailure n) = Failure n

chooseStrategy :: O.TestStrategy -> Strategy
chooseStrategy O.Bisect = trackBisect
chooseStrategy O.Linear = trackLinear
chooseStrategy O.Backoff = trackBackoff
chooseStrategy O.Once = oneTest

-- | test only the last recorded state
oneTest :: Strategy
oneTest _ _ Success _ = return RunSuccess
oneTest _ _ (Failure n)  _ = return $ RunFailed n

-- | linear search (with --linear)
trackLinear :: Strategy
trackLinear _ _ Success _ = return StrategySuccess
trackLinear _ _ (Failure _) NilRL = return NoPasses
trackLinear opts testCmd (Failure _) ps = trackNextLinear opts testCmd ps

trackNextLinear
    :: (RepoPatch p, ApplyMonad (ApplyState p) DefaultIO)
    => [DarcsFlag]
    -> IO TestResult
    -> RL (Named p) wX wY
    -> IO (StrategyResult p)
trackNextLinear _ _ NilRL = return NoPasses
trackNextLinear opts testCmd (ps:<:p) = do
    safeUnapply p
    when (O.yes (O.setScriptsExecutable ? opts)) $ setScriptsExecutablePatches p
    putStrLn "Trying without the patch:"
    putDocLn $ description p
    hFlush stdout
    testResult <- testCmd
    case testResult of
        Success -> return $ Blame WasLinear $ Sealed2 p
        Failure _ -> trackNextLinear opts testCmd ps

-- | exponential backoff search (with --backoff)
trackBackoff :: Strategy
trackBackoff _ _ Success NilRL = return StrategySuccess
trackBackoff _ _ (Failure _) NilRL = return NoPasses
trackBackoff _ _ Success _ = return PassesOnHead
trackBackoff opts testCmd (Failure _) ps =
    trackNextBackoff opts testCmd 4 ps

trackNextBackoff :: (RepoPatch p, ApplyMonad (ApplyState p) DefaultIO)
                 => [DarcsFlag]
                 -> IO TestResult
                 -> Int -- ^ number of patches to skip
                 -> RL (Named p) wY wZ -- ^ patches not yet skipped
                 -> IO (StrategyResult p)
trackNextBackoff _ _ _ NilRL = return NoPasses
trackNextBackoff opts testCmd n ahead
    | n >= lengthRL ahead = initialBisect opts testCmd ahead
trackNextBackoff opts testCmd n ahead = do
    putStrLn $ "Skipping " ++ show n ++ " patches..."
    hFlush stdout
    case splitAtRL n ahead of
        ( ahead' :> skipped' ) -> do
            unapplyRL skipped'
            when (O.yes (O.setScriptsExecutable ? opts)) $ setScriptsExecutablePatches skipped'
            testResult <- testCmd
            case testResult of
                Failure _ ->
                    trackNextBackoff opts testCmd (2*n) ahead'
                Success -> do
                    applyRL skipped'  -- offending patch is one of these
                    initialBisect opts testCmd skipped' -- bisect to find it

-- | binary search (with --bisect)
trackBisect :: Strategy
trackBisect _ _ Success NilRL = return StrategySuccess
trackBisect _ _ (Failure _) NilRL = return NoPasses
trackBisect _ _ Success _ = return PassesOnHead
trackBisect opts testCmd (Failure _) ps =
    initialBisect opts testCmd ps

initialBisect ::  (RepoPatch p, ApplyMonad (ApplyState p) DefaultIO)
              => [DarcsFlag]
              -> IO TestResult
              -> RL (Named p) wX wY
              -> IO (StrategyResult p)
initialBisect opts testCmd ps =
    trackNextBisect opts currProg testCmd BisectRight (patchTreeFromRL ps)
  where
    maxProg  = 1 + round ((logBase 2 $ fromIntegral $ lengthRL ps) :: Double)
    currProg = (1, maxProg) :: BisectProgress

-- | Bisect Patch Tree
data PatchTree p wX wY where
    Leaf :: p wX wY -> PatchTree p wX wY
    Fork :: PatchTree p wY wZ -> PatchTree p wX wY -> PatchTree p wX wZ

-- | Direction of Bisect trackdown
data BisectDir = BisectLeft | BisectRight deriving Show

-- | Progress of Bisect
type BisectProgress = (Int, Int)

-- | Create Bisect PatchTree from the RL
patchTreeFromRL :: RL p wX wY -> PatchTree p wX wY
patchTreeFromRL (NilRL :<: l) = Leaf l
patchTreeFromRL xs = case splitAtRL (lengthRL xs `div` 2) xs of
                       (r :> l) -> Fork (patchTreeFromRL l) (patchTreeFromRL r)

-- | Convert PatchTree back to RL
patchTree2RL :: PatchTree p wX wY -> RL p wX wY
patchTree2RL (Leaf p)   = NilRL :<: p
patchTree2RL (Fork l r) = patchTree2RL r +<+ patchTree2RL l

-- | Iterate the Patch Tree
trackNextBisect :: (RepoPatch p, ApplyMonad (ApplyState p) DefaultIO)
                => [DarcsFlag]
                -> BisectProgress
                -> IO TestResult -- ^ test command
                -> BisectDir
                -> PatchTree (Named p) wX wY
                -> IO (StrategyResult p)
trackNextBisect opts (dnow, dtotal) testCmd dir (Fork l r) = do
  putStr $ "Trying " ++ show dnow ++ "/" ++ show dtotal ++ " sequences...\n"
  hFlush stdout
  case dir of
    BisectRight -> jumpHalfOnRight opts l  -- move in temporary repo
    BisectLeft  -> jumpHalfOnLeft  opts r  -- within given direction
  testResult <- testCmd -- execute test on repo
  case testResult of
    Success -> trackNextBisect opts (dnow+1, dtotal) testCmd
                               BisectLeft l  -- continue left  (to the present)
    _       -> trackNextBisect opts (dnow+1, dtotal) testCmd
                               BisectRight r -- continue right (to the past)
trackNextBisect _ _ _ _ (Leaf p) = return (Blame AssumedMonotony (Sealed2 p))

jumpHalfOnRight :: (Apply p, PatchInspect p,
                    ApplyMonad (ApplyState p) DefaultIO)
                => [DarcsFlag] -> PatchTree p wX wY -> IO ()
jumpHalfOnRight opts l = do unapplyRL ps
                            when (O.yes (O.setScriptsExecutable ? opts)) $ setScriptsExecutablePatches ps
  where ps = patchTree2RL l

jumpHalfOnLeft :: (Apply p, PatchInspect p,
                   ApplyMonad (ApplyState p) DefaultIO)
               => [DarcsFlag] -> PatchTree p wX wY -> IO ()
jumpHalfOnLeft opts r = do applyRL p
                           when (O.yes (O.setScriptsExecutable ? opts)) $ setScriptsExecutablePatches p

  where p = patchTree2RL r

applyRL :: (Apply p, ApplyMonad (ApplyState p) DefaultIO)
        => RL p wX wY -> IO ()
applyRL   patches = sequence_ (mapFL safeApply (reverseRL patches))

unapplyRL :: (Apply p, ApplyMonad (ApplyState p) DefaultIO)
           => RL p wX wY -> IO ()
unapplyRL patches = sequence_ (mapRL safeUnapply patches)

safeApply :: (Apply p, ApplyMonad (ApplyState p) DefaultIO)
          => p wX wY -> IO ()
safeApply p = runDefault (apply p) `catch` \(msg :: IOException) -> fail $ "Bad patch:\n" ++ show msg

safeUnapply :: (Apply p, ApplyMonad (ApplyState p) DefaultIO)
            => p wX wY -> IO ()
safeUnapply p = runDefault (unapply p) `catch` \(msg :: IOException) -> fail $ "Bad patch:\n" ++ show msg
