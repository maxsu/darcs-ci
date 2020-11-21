--  Copyright (C) 2009 Ganesh Sittampalam
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Darcs.UI.Commands.GZCRCs
    ( gzcrcs
    , doCRCWarnings
    ) where

import Darcs.Prelude

import Control.Monad ( when, unless, forM_ )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Writer ( runWriterT, tell )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Monoid ( Any(..), Sum(..) )
import System.Directory ( doesFileExist, doesDirectoryExist )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr )
import Darcs.Util.File ( getRecursiveContentsFullPath )
import Darcs.Util.ByteString ( isGZFile, gzDecompress )
import Darcs.Util.Global ( getCRCWarnings, resetCRCWarnings )
import Darcs.Repository ( Repository, withRepository, RepoJob(..), repoCache )
-- This command needs access beyond the normal repository APIs (to
-- get at the caches and inspect them directly)
-- Could move the relevant code into Darcs.Repository modules
-- but it doesn't really seem worth it.
import Darcs.Repository.Cache
    ( allHashedDirs
    , cacheEntries
    , hashedFilePath
    , isThisRepo
    , writable
    )
import Darcs.Util.Lock ( gzWriteAtomicFilePSs )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository
    , putInfo, putVerbose
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Options ( (^), oid, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.Flags ( DarcsFlag, useCache )
import Darcs.Util.Printer ( Doc, ($$), formatText, hPutDocLn, pathlist, text )

gzcrcsHelp :: Doc
gzcrcsHelp = formatText 80
    [ "Versions of darcs >=1.0.4 and <2.2.0 had a bug that caused compressed "
      ++ "files with bad CRCs (but valid data) to be written out. CRCs were "
      ++ "not checked on reading, so this bug wasn't noticed."
    , "This command inspects your repository for this corruption and "
      ++ "optionally repairs it."
    , "By default it also does this for any caches you have configured and "
      ++ "any other local repositories listed as sources of patches for this "
      ++ "one, perhaps because of a lazy clone. You can limit the scope to just "
      ++ "the current repo with the --just-this-repo flag."
    , "Note that readonly caches, or other repositories listed as sources, "
      ++ "will be checked but not repaired. Also, this command will abort if "
      ++ "it encounters any non-CRC corruption in compressed files."
    , "You may wish to also run 'darcs check --complete' before repairing the "
      ++ "corruption. This is not done automatically because it might result "
      ++ "in needing to fetch extra patches if the repository is lazy."
    , "If there are any other problems with your repository, you can still "
      ++ "repair the CRCs, but you are advised to first make a backup copy in "
      ++ "case the CRC errors are actually caused by bad data and the old "
      ++ "CRCs might be useful in recovering that data."
    , "If you were warned about CRC errors during an operation involving "
      ++ "another repository, then it is possible that the other repository "
      ++ "contains the corrupt CRCs, so you should arrange for that "
      ++ "repository to also be checked/repaired."
    ]

-- |This is designed for use in an atexit handler, e.g. in Darcs.RunCommand
doCRCWarnings :: Bool -> IO ()
doCRCWarnings verbose = do
    files <- getCRCWarnings
    resetCRCWarnings
    unless (null files) $ do
        hPutDocLn stderr . formatText 80 $
            [ "Warning: CRC errors found. These are probably harmless but "
              ++ "should be repaired. See 'darcs gzcrcs --help' for more "
              ++ "information."
            ]
        when verbose $
            hPutDocLn stderr $
                text "The following corrupt files were found:" $$ pathlist files

gzcrcsDescription :: String
gzcrcsDescription = "Check or repair the CRCs of compressed files in the "
                    ++ "repository."

gzcrcs :: DarcsCommand
gzcrcs = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "gzcrcs"
    , commandHelp = gzcrcsHelp
    , commandDescription = gzcrcsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = gzcrcsCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc gzcrcsBasicOpts
    , commandDefaults = defaultFlags gzcrcsOpts
    , commandCheckOptions = ocheck gzcrcsOpts
    }
  where
    gzcrcsBasicOpts = O.gzcrcsActions ^ O.justThisRepo ^ O.repoDir
    gzcrcsOpts = gzcrcsBasicOpts `withStdOpts` oid

gzcrcsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
gzcrcsCmd _ opts _ =
  case O.gzcrcsActions ? opts of
    Nothing -> fail "You must specify --check or --repair for gzcrcs"
    Just action -> withRepository (useCache ? opts) (RepoJob (gzcrcs' action opts))

gzcrcs' :: O.GzcrcsAction -> [DarcsFlag] -> Repository rt p wR wU wT -> IO ()
gzcrcs' action opts repo = do
    -- Somewhat ugly IORef use here because it's convenient, would be nicer to
    -- pre-filter the list of locs to check and then decide whether to print
    -- the message up front.
    warnRelatedRepos <- newIORef $ not isJustThisRepo
    let locs = cacheEntries $ repoCache repo
    (_, Any checkFailed) <- runWriterT $ forM_ locs $ \loc ->
        unless (isJustThisRepo && not (isThisRepo loc)) $ do
            let isWritable = writable loc
            forM_ allHashedDirs $ \hdir -> do
                let dir = hashedFilePath loc hdir ""
                exists <- liftIO $ doesDirectoryExist dir
                when exists $ do
                    liftIO $ do
                        warn <- readIORef warnRelatedRepos
                        when (warn && not (isThisRepo loc)) $ do
                            writeIORef warnRelatedRepos False
                            putInfo opts $ text $
                                "Also checking related repos and caches; use "
                                ++ "--just-this-repo to disable.\n"
                                ++ "Checking " ++ dir
                                ++ (if isWritable then "" else " (readonly)")
                    files <- liftIO $ getRecursiveContentsFullPath dir
                    (_, Sum count) <- runWriterT $ forM_ files $ \file -> do
                        isfile <- liftIO $ doesFileExist file
                        when isfile $ do
                            gz <- liftIO $ isGZFile file
                            case gz of
                                Nothing -> return ()
                                Just len -> do
                                    contents <- liftIO $ B.readFile file
                                    let contentsbl = BL.fromChunks [contents]
                                        (uncompressed, isCorrupt) =
                                            gzDecompress (Just len) contentsbl
                                    when isCorrupt $ do
                                        -- Count of files in current directory
                                        tell (Sum 1)
                                        liftIO . putVerbose opts $ text $
                                            "Corrupt: " ++ file
                                        when (isWritable && shouldRepair) $
                                            doRepair file uncompressed
                    when (count > (0 :: Int)) $ do
                        liftIO . putInfo opts $ text $
                            "Found " ++ show count ++ " corrupt file"
                            ++ (if count > 1 then "s" else "")
                            ++ (if shouldRepair
                                    then if isWritable
                                            then " (repaired)"
                                            else " (not repaired)"
                                    else "")
                        -- Something corrupt somewhere
                        tell (Any True)
    when (action == O.GzcrcsCheck && checkFailed) $
        exitWith (ExitFailure 1)
  where
    shouldRepair = action == O.GzcrcsRepair
    isJustThisRepo = O.justThisRepo ? opts
    doRepair name contents = liftIO $ gzWriteAtomicFilePSs name contents
