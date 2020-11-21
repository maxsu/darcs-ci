-- Copyright (C) 2005 David Roundy
--
-- This file is licensed under the GPL, version two or later.

{- | The format file.

The purpose of the format file is to check compatibility between
repositories in different formats and to allow the addition of new features
without risking corruption by old darcs versions that do not yet know about
these features.

This allows a limited form of forward compatibility between darcs versions.
Old versions of darcs that are unaware of features added in later versions
will fail with a decent error message instead of crashing or misbehaving or
even corrupting new repos.

The format file lives at _darcs/format and must only contain printable ASCII
characters and must not contain the characters @<@ and @>@.

(We currently do not strip whitespace from the lines, but may want to do so
in the future.)

The file consists of format properties. A format property can contain any
allowed ASCII character except the vertical bar (@|@) and newlines. Empty
lines are ignored and multiple properties on the same line are separated
with a @|@.

If multiple properties appear on the same line (separated by vertical bars),
then this indicates alternative format properties. These have a generic
meaning:

 * If we know *any* of these properties, then we can read the repo.

 * If we know *all* of them, we can also write the repo.

The above rules are necessary conditions, not sufficient ones. It is allowed
to further restrict read and/or write access for specific commands, but care
should be taken to not unnecessarily break forward compatibility. It is not
recommended, but sometimes necessary, to impose ad-hoc restrictions on the
format, see 'transferProblem' and 'readProblem' for examples.

The no-working-dir property is an example for how to use alternative
properties. An old darcs version that does not know this format can perform
most read-only operations correctly even if there is no working tree;
however, whatsnew will report that the whole tree was removed, so the
solution is not perfect.

When you add a new property as an alternative to an existing one, you should
make sure that the old format remains to be updated in parallel to the new
one, so that reading the repo with old darcs versions behaves correctly. If
this cannot be guaranteed, it is better to add the new format on a separate
line.

It is not advisable for commands to modify an existing format file. However,
sometimes compatibility requirements may leave us no other choice. In this
case make sure to write the format file only after having checked that the
existing repo format allows modification of the repo, and that you have
taken the repo lock.

-}

{-# LANGUAGE OverloadedStrings #-}
module Darcs.Repository.Format
    ( RepoFormat(..)
    , RepoProperty(..)
    , identifyRepoFormat
    , tryIdentifyRepoFormat
    , createRepoFormat
    , writeRepoFormat
    , writeProblem
    , readProblem
    , transferProblem
    , formatHas
    , addToFormat
    , removeFromFormat
    ) where

import Darcs.Prelude

import Control.Monad ( mplus, (<=<) )
import qualified Data.ByteString.Char8 as BC ( split, pack, unpack, elem )
import qualified Data.ByteString  as B ( ByteString, null, empty, stripPrefix )
import Data.List ( partition, intercalate, (\\) )
import Data.Maybe ( mapMaybe )
import Data.String ( IsString )
import System.FilePath.Posix( (</>) )

import Darcs.Util.External
    ( fetchFilePS
    , Cachable( Cachable )
    )
import Darcs.Util.Lock ( writeBinFile )
import qualified Darcs.Repository.Flags as F
    ( WithWorkingDir (..), PatchFormat (..)  )
import Darcs.Repository.Paths ( formatPath, oldInventoryPath )
import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.Exception ( catchall, prettyException )

import Darcs.Util.ByteString ( linesPS )
import Darcs.Util.Progress ( beginTedious, endTedious, finishedOneIO )

data RepoProperty = Darcs1
                  | Darcs2
                  | Darcs3
                  | HashedInventory
                  | NoWorkingDir
                  | RebaseInProgress
                  | RebaseInProgress_2_16
                  | UnknownFormat B.ByteString
                  deriving ( Eq )

-- | Define string constants in one place, for reuse in show/parse functions.
darcs1Format, darcs2Format, darcs3Format,
  hashedInventoryFormat, noWorkingDirFormat,
  rebaseInProgressFormat, rebaseInProgress_2_16,
  newStyleRebaseInProgress :: IsString s => s

darcs1Format = "darcs-1.0"
darcs2Format = "darcs-2"
darcs3Format = "darcs-3"
hashedInventoryFormat = "hashed"
noWorkingDirFormat = "no-working-dir"
rebaseInProgressFormat = "rebase-in-progress"
rebaseInProgress_2_16 = "rebase-in-progress-2-16"
-- compatibility alias, may want to remove this at some point in the future
newStyleRebaseInProgress = "new-style-rebase-in-progress"

instance Show RepoProperty where
    show Darcs1 = darcs1Format
    show Darcs2 = darcs2Format
    show Darcs3 = darcs3Format
    show HashedInventory = hashedInventoryFormat
    show NoWorkingDir = noWorkingDirFormat
    show RebaseInProgress = rebaseInProgressFormat
    show RebaseInProgress_2_16 = rebaseInProgress_2_16
    show (UnknownFormat f) = BC.unpack f

readRepoProperty :: B.ByteString -> RepoProperty
readRepoProperty input
    | input == darcs1Format = Darcs1
    | input == darcs2Format = Darcs2
    | input == darcs3Format = Darcs3
    | input == hashedInventoryFormat = HashedInventory
    | input == noWorkingDirFormat = NoWorkingDir
    | input == rebaseInProgressFormat = RebaseInProgress
    | input == newStyleRebaseInProgress = RebaseInProgress_2_16
    | input == rebaseInProgress_2_16 = RebaseInProgress_2_16
    | otherwise = UnknownFormat input

-- | Representation of the format of a repository. Each
-- sublist corresponds to a line in the format file.
newtype RepoFormat = RF [[RepoProperty]]

-- | Is a given property contained within a given format?
formatHas :: RepoProperty -> RepoFormat -> Bool
formatHas f (RF rps) = f `elem` concat rps

-- | Add a single property to an existing format.
addToFormat :: RepoProperty -> RepoFormat -> RepoFormat
addToFormat f (RF rps) = RF (rps ++ [[f]])

-- | Remove a single property from an existing format.
removeFromFormat :: RepoProperty -> RepoFormat -> RepoFormat
removeFromFormat f (RF rps) = RF (rps \\ [[f]])

instance Show RepoFormat where
    show (RF rf) = unlines $ map (intercalate "|" . map show) rf

-- | Identify the format of the repository at the
-- given location (directory, URL, or SSH path).
-- Fails if we weren't able to identify the format.
identifyRepoFormat :: String -> IO RepoFormat
identifyRepoFormat = either fail return <=< tryIdentifyRepoFormat

-- | Identify the format of the repository at the
-- given location (directory, URL, or SSH path).
-- Return @'Left' reason@ if it fails, where @reason@ explains why
-- we weren't able to identify the format. Note that we do no verification of
-- the format, which is handled by 'readProblem' or 'writeProblem' on the
-- resulting 'RepoFormat'.
tryIdentifyRepoFormat :: String -> IO (Either String RepoFormat)
tryIdentifyRepoFormat repo = do
    let k = "Identifying repository " ++ repo
    beginTedious k
    finishedOneIO k "format"
    formatInfo <- (fetchFilePS (repo </> formatPath) Cachable)
                  `catchall` (return B.empty)
    -- We use a workaround for servers that don't return a 404 on nonexistent
    -- files (we trivially check for something that looks like a HTML/XML tag).
    format <-
      if B.null formatInfo || BC.elem '<' formatInfo then do
        finishedOneIO k "inventory"
        missingInvErr <- checkFile (repo </> oldInventoryPath)
        case missingInvErr of
          Nothing -> return . Right $ RF [[Darcs1]]
          Just e -> return . Left $ makeErrorMsg e
      else return . Right $ readFormat formatInfo
    endTedious k
    return format
  where
    readFormat =
      RF . map (map (readRepoProperty . fixupUnknownFormat)) . splitFormat

    -- silently fixup unknown format entries broken by previous darcs versions
    fixupUnknownFormat s =
      case B.stripPrefix "Unknown format: " s of
        Nothing -> s
        Just s' -> fixupUnknownFormat s' -- repeat until not found anymore

    -- split into lines, then split each non-empty line on '|'
    splitFormat = map (BC.split '|') . filter (not . B.null) . linesPS

    checkFile path = (fetchFilePS path Cachable >> return Nothing)
                     `catchNonSignal`
                     (return . Just . prettyException)

    makeErrorMsg e =  "Not a repository: " ++ repo ++ " (" ++ e ++ ")"

-- | Write the repo format to the given file.
writeRepoFormat :: RepoFormat -> FilePath -> IO ()
writeRepoFormat rf loc = writeBinFile loc $ BC.pack $ show rf
-- note: this assumes show returns ascii

-- | Create a repo format. The first argument specifies the patch
-- format; the second says whether the repo has a working tree.
createRepoFormat :: F.PatchFormat -> F.WithWorkingDir -> RepoFormat
createRepoFormat fmt wwd = RF $ (HashedInventory : flags2wd wwd) : flags2format fmt
  where
    flags2format F.PatchFormat1 = []
    flags2format F.PatchFormat2 = [[Darcs2]]
    flags2format F.PatchFormat3 = [[Darcs3]]
    flags2wd F.NoWorkingDir   = [NoWorkingDir]
    flags2wd F.WithWorkingDir = []

-- | @'writeProblem' source@ returns 'Just' an error message if we cannot write
-- to a repo in format @source@, or 'Nothing' if there's no such problem.
writeProblem :: RepoFormat -> Maybe String
writeProblem target = readProblem target `mplus` findProblems target wp
  where
    wp [] = error "impossible case"
    wp x = case partition isKnown x of
               (_, []) -> Nothing
               (_, unknowns) -> Just . unwords $
                    "Can't write repository: unknown formats:" : map show unknowns

-- | @'transferProblem' source target@ returns 'Just' an error message if we
-- cannot transfer patches from a repo in format @source@ to a repo in format
-- @target@, or 'Nothing' if there are no such problem.
transferProblem :: RepoFormat -> RepoFormat -> Maybe String
transferProblem source target
    | formatHas Darcs3 source /= formatHas Darcs3 target =
        Just "Cannot mix darcs-3 repositories with older formats"
    | formatHas Darcs2 source /= formatHas Darcs2 target =
        Just "Cannot mix darcs-2 repositories with older formats"
    | formatHas RebaseInProgress source =
        Just "Cannot transfer patches from a repository \
          \where an old-style rebase is in progress"
    | otherwise = readProblem source `mplus` writeProblem target

-- | @'readProblem' source@ returns 'Just' an error message if we cannot read
-- from a repo in format @source@, or 'Nothing' if there's no such problem.
readProblem :: RepoFormat -> Maybe String
readProblem source
    | formatHas Darcs1 source && formatHas Darcs2 source =
        Just "Invalid repository format: format 2 is incompatible with format 1"
    | formatHas RebaseInProgress source && formatHas RebaseInProgress_2_16 source =
        Just "Invalid repository format: \
          \cannot have both old-style and new-style rebase in progress"
readProblem source = findProblems source rp
  where
    rp x | any isKnown x = Nothing
    rp [] = error "impossible case"
    rp x = Just . unwords $ "Can't read repository: unknown formats:" : map show x

-- |'findProblems' applies a function that maps format-entries to an optional
-- error message, to each repoformat entry. Returning any errors.
findProblems :: RepoFormat -> ([RepoProperty] -> Maybe String) -> Maybe String
findProblems (RF ks) formatHasProblem = case mapMaybe formatHasProblem ks of
                                            [] -> Nothing
                                            xs -> Just $ unlines xs

-- | Does this version of darcs know how to handle this property?
isKnown :: RepoProperty -> Bool
isKnown p = p `elem` knownProperties
  where
    knownProperties :: [RepoProperty]
    knownProperties = [ Darcs1
                      , Darcs2
                      , Darcs3
                      , HashedInventory
                      , NoWorkingDir
                      , RebaseInProgress
                      , RebaseInProgress_2_16
                      ]
