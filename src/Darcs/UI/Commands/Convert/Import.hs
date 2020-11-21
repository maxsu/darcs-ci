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

{-# LANGUAGE OverloadedStrings #-}

module Darcs.UI.Commands.Convert.Import ( convertImport ) where

import Darcs.Prelude hiding ( readFile, lex )

import Control.Applicative ((<|>),many)
import Control.Arrow ((&&&), second)
import Control.Monad (unless, void, when)
import Control.Monad.State.Strict (gets, modify)
import Control.Monad.Trans (liftIO)

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef (modifyIORef, newIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (stdin)

import Darcs.Patch.Depends ( getUncovered )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Patch ( PrimOf, RepoPatch, move )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Named ( Named(..), infopatch )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , RL(..)
    , (+<+)
    , reverseFL
    , reverseRL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), unFreeLeft )

import Darcs.Patch.Info ( PatchInfo, patchinfo )
import Darcs.Patch.Prim ( sortCoalesceFL )
import Darcs.Patch.RepoType ( IsRepoType(..) )

import Darcs.Repository
    ( EmptyRepository(..)
    , Repository
    , cleanRepository
    , createPristineDirectoryTree
    , createRepository
    , finalizeRepositoryChanges
    , readTentativeRepo
    , repoCache
    , repoLocation
    , revertRepositoryChanges
    , withUMaskFlag
    )
import Darcs.Repository.Diff (treeDiff)
import Darcs.Repository.Flags (Compression(..), DiffAlgorithm(PatienceDiff))
import Darcs.Repository.Hashed (addToTentativeInventory)
import Darcs.Repository.Paths (pristineDirPath, tentativePristinePath)
import Darcs.Repository.Prefs (FileType(..))
import Darcs.Repository.State (readRecorded)

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , nodefaults
    , withStdOpts
    )
import Darcs.UI.Commands.Convert.Util
    ( Marks
    , addMark
    , emptyMarks
    , getMark
    , patchHash
    , updatePending
    )
import Darcs.UI.Commands.Util.Tree (treeHasDir, treeHasFile)
import Darcs.UI.Completion (noArgs)
import Darcs.UI.Flags
    ( DarcsFlag
    , patchFormat
    , patchIndexNo
    , umask
    , useCache
    , withWorkingDir
    )
import Darcs.UI.Options
    ( (?)
    , (^)
    , defaultFlags
    , ocheck
    , odesc
    )
import qualified Darcs.UI.Options.All as O

import Darcs.Util.ByteString (decodeLocale, unpackPSFromUTF8)
import Darcs.Util.DateTime
    ( formatDateTime
    , parseDateTime
    , startOfTime
    )
import Darcs.Util.Global (darcsdir)
import Darcs.Util.Hash (Hash(..), encodeBase16, sha256)
import Darcs.Util.Lock (withNewDirectory)
import Darcs.Util.Path
    ( AbsolutePath
    , AnchoredPath(..)
    , appendPath
    , floatPath
    , makeName
    , parent
    , darcsdirName
    )
import Darcs.Util.Printer ( Doc, text )
import qualified Darcs.Util.Tree as T
import Darcs.Util.Tree
    ( Tree
    , TreeItem(..)
    , findTree
    , listImmediate
    , readBlob
    , treeHash
    )
import Darcs.Util.Tree.Hashed (darcsAddMissingHashes, hashedTreeIO)
import qualified Darcs.Util.Tree.Monad as TM
import Darcs.Util.Tree.Monad hiding (createDirectory, exists, rename)


convertImportHelp :: Doc
convertImportHelp = text $ unlines
 [ "This command imports git repositories into new darcs repositories."
 , "Further options are accepted (see `darcs help init`)."
 , ""
 , "To convert a git repo to a new darcs one you may run:"
 , ""
 , "    $ (cd gitrepo && git fast-export --all -M) | darcs convert import darcsmirror"
 , ""
 , "WARNING: git repositories with branches will produce weird results,"
 , "         use at your own risks."
 , ""
 , "Incremental import with marksfiles is currently not supported."
 ]

convertImport :: DarcsCommand
convertImport = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "import"
    , commandHelp = convertImportHelp
    , commandDescription = "Import from a git-fast-export stream into darcs"
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[<DIRECTORY>]"]
    , commandCommand = fastImport
    , commandPrereq = \_ -> return $ Right ()
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc convertImportAdvancedOpts
    , commandBasicOptions = odesc convertImportBasicOpts
    , commandDefaults = defaultFlags convertImportOpts
    , commandCheckOptions = ocheck convertImportOpts
    }
  where
    convertImportBasicOpts
      = O.newRepo
      ^ O.setScriptsExecutable
      ^ O.patchFormat
      ^ O.withWorkingDir
    convertImportAdvancedOpts = O.patchIndexNo ^ O.umask
    convertImportOpts = convertImportBasicOpts `withStdOpts` convertImportAdvancedOpts

type Marked = Maybe Int
type Branch = B.ByteString
type AuthorInfo = B.ByteString
type Message = B.ByteString
type Content = B.ByteString
type Tag = B.ByteString

data RefId = MarkId Int | HashId B.ByteString | Inline
           deriving Show

-- Newish (> 1.7.6.1) Git either quotes filenames or has two
-- non-special-char-containing paths. Older git doesn't do any quoting, so
-- we'll have to manually try and find the correct paths, when we use the
-- paths.
data CopyRenameNames = Quoted B.ByteString B.ByteString
                     | Unquoted B.ByteString deriving Show

data Object = Blob (Maybe Int) Content
            | Reset Branch (Maybe RefId)
            | Commit Branch Marked AuthorInfo Message
            | Tag Tag Int AuthorInfo Message
            | Modify (Either Int Content) B.ByteString -- (mark or content), filename
            | Gitlink B.ByteString
            | Copy CopyRenameNames
            | Rename CopyRenameNames
            | Delete B.ByteString -- filename
            | From Int
            | Merge Int
            | Progress B.ByteString
            | End
            deriving Show

type Ancestors = (Marked, [Int])
data State p where
  Toplevel :: Marked -> Branch -> State p
  InCommit :: Marked -> Ancestors -> Branch -> Tree IO -> RL (PrimOf p) cX cY -> PatchInfo -> State p
  Done :: State p

instance Show (State p) where
  show Toplevel {} = "Toplevel"
  show InCommit {} = "InCommit"
  show Done =  "Done"

fastImport :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
fastImport _ opts [outrepo] =
  withUMaskFlag (umask ? opts) $ withNewDirectory outrepo $ do
    EmptyRepository _repo <- createRepository
      (patchFormat ? opts)
      (withWorkingDir ? opts)
      (patchIndexNo ? opts)
      (useCache ? opts)
    -- TODO implement --dry-run, which would be read-only?
    _repo <- revertRepositoryChanges _repo (updatePending opts)
    marks <- fastImport' _repo emptyMarks
    _ <- finalizeRepositoryChanges _repo (updatePending opts) GzipCompression
    cleanRepository _repo
    createPristineDirectoryTree _repo "." (withWorkingDir ? opts)
    return marks
fastImport _ _ _ = fail "I need exactly one output repository."

fastImport' :: forall rt p r u . (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree) =>
               Repository rt p r u r -> Marks -> IO ()
fastImport' repo marks = do
    pristine <- readRecorded repo
    marksref <- newIORef marks
    let initial = Toplevel Nothing $ BC.pack "refs/branches/master"

        go :: State p -> B.ByteString -> TreeIO ()
        go state rest = do (rest', item) <- parseObject rest
                           state' <- process state item
                           case state' of
                             Done -> return ()
                             _ -> go state' rest'

        -- sort marks into buckets, since there can be a *lot* of them
        markpath :: Int -> AnchoredPath
        markpath n = floatPath (darcsdir </> "marks")
                        `appendPath` (either error id $ makeName $ show (n `div` 1000))
                        `appendPath` (either error id $ makeName $ show (n `mod` 1000))

        makeinfo author message tag = do
          let (name, log) = case unpackPSFromUTF8 message of
                                      "" -> ("Unnamed patch", [])
                                      msg -> (head &&& tail) . lines $ msg
              (author'', date'') = span (/='>') $ unpackPSFromUTF8 author
              date' = dropWhile (`notElem` ("0123456789" :: String)) date''
              author' = author'' ++ ">"
              date = formatDateTime "%Y%m%d%H%M%S" $ fromMaybe startOfTime (parseDateTime "%s %z" date')
          liftIO $ patchinfo date (if tag then "TAG " ++ name else name) author' log

        addtag author msg =
          do info_ <- makeinfo author msg True
             gotany <- liftIO $ doesFileExist tentativePristinePath
             deps <- if gotany then liftIO $
                                      getUncovered `fmap`
                                        readTentativeRepo repo (repoLocation repo)
                               else return []
             let patch :: Named p wA wA
                 patch = NamedP info_ deps NilFL
             liftIO $ addToTentativeInventory (repoCache repo) GzipCompression (n2pia patch)

        -- processing items
        updateHashes = do
          let nodarcs = \(AnchoredPath (x:_)) _ -> x /= darcsdirName
              hashblobs (File blob@(T.Blob con NoHash)) =
                do hash <- sha256 `fmap` readBlob blob
                   return $ File (T.Blob con hash)
              hashblobs x = return x
          tree' <- liftIO . T.partiallyUpdateTree hashblobs nodarcs =<< gets tree
          modify $ \s -> s { tree = tree' }
          return $ T.filter nodarcs tree'

        -- Since git doesn't track directores it implicitly deletes
        -- them when they become empty. We should therefore remove any
        -- directories that become empty (except the repo-root
        -- directory!)
        deleteEmptyParents fp =
          case parent fp of
            Nothing -> return ()
            Just directParent -> do
              parentTree <- flip findTree directParent <$> gets tree
              case (null . listImmediate) <$> parentTree of
                      Just True -> do TM.unlink directParent
                                      deleteEmptyParents directParent
                      -- Either missing (not possible) or non-empty.
                      _ -> return ()

        -- generate a Hunk primitive patch from diffing
        diffCurrent :: State p -> TreeIO (State p)
        diffCurrent (InCommit mark ancestors branch start ps info_) = do
          current <- updateHashes
          Sealed diff <- unFreeLeft `fmap`
             liftIO (treeDiff PatienceDiff (const TextFile) start current)
          let newps = ps +<+ reverseFL diff
          return $ InCommit mark ancestors branch current newps info_
        diffCurrent _ = error "This is never valid outside of a commit."

        process :: State p -> Object -> TreeIO (State p)
        process s (Progress p) = do
          liftIO $ putStrLn ("progress " ++ decodeLocale p)
          return s

        process (Toplevel _ _) End = do
          tree' <- (liftIO . darcsAddMissingHashes) =<< updateHashes
          modify $ \s -> s { tree = tree' } -- lets dump the right tree, without _darcs
          let root = encodeBase16 $ treeHash tree'
          liftIO $ do
            putStrLn "\\o/ It seems we survived. Enjoy your new repo."
            B.writeFile tentativePristinePath $ BC.concat [BC.pack "pristine:", root]
          return Done

        process (Toplevel n b) (Tag tag what author msg) = do
          if Just what == n
             then addtag author msg
             else liftIO $ putStrLn $
                    "WARNING: Ignoring out-of-order tag " ++ decodeLocale tag
          return (Toplevel n b)

        process (Toplevel n _) (Reset branch from) =
          do case from of
               (Just (MarkId k)) | Just k == n ->
                 addtag (BC.pack "Anonymous Tagger <> 0 +0000") branch
               _ -> liftIO $ putStrLn $ "WARNING: Ignoring out-of-order tag " ++
                                        decodeLocale branch
             return $ Toplevel n branch

        process (Toplevel n b) (Blob (Just m) bits) = do
          TM.writeFile (markpath m) (BLC.fromChunks [bits])
          return $ Toplevel n b

        process x (Gitlink link) = do
          liftIO $ putStrLn $ "WARNING: Ignoring gitlink " ++ decodeLocale link
          return x

        process (Toplevel previous pbranch) (Commit branch mark author message) = do
          when (pbranch /= branch) $ do
            liftIO $ putStrLn ("Tagging branch: " ++ decodeLocale pbranch)
            addtag author pbranch
          info_ <- makeinfo author message False
          startstate <- updateHashes
          return $ InCommit mark (previous, []) branch startstate NilRL info_

        process s@InCommit {} (Modify (Left m) path) = do
          TM.copy (markpath m) (decodePath path)
          diffCurrent s

        process s@InCommit {} (Modify (Right bits) path) = do
          TM.writeFile (decodePath path) (BLC.fromChunks [bits])
          diffCurrent s

        process s@InCommit {} (Delete path) = do
          let floatedPath = decodePath path
          TM.unlink floatedPath
          deleteEmptyParents floatedPath
          diffCurrent s

        process (InCommit mark (prev, current) branch start ps info_) (From from) =
          return $ InCommit mark (prev, from:current) branch start ps info_

        process (InCommit mark (prev, current) branch start ps info_) (Merge from) =
          return $ InCommit mark (prev, from:current) branch start ps info_

        process s@InCommit {} (Copy names) = do
            (from, to) <- extractNames names
            TM.copy (decodePath from) (decodePath to)
            -- We can't tell Darcs that a file has been copied, so it'll
            -- show as an addfile.
            diffCurrent s

        process s@(InCommit mark ancestors branch start _ info_) (Rename names) = do
          (from, to) <- extractNames names
          let uFrom = decodePath from
              uTo = decodePath to
          case parent uTo of
            Nothing ->
              -- no parents i.e. target is root => nothing to do
              return ()
            Just parentDir -> do
              targetDirExists <- liftIO $ treeHasDir start uTo
              targetFileExists <- liftIO $ treeHasFile start uTo
              parentDirExists <-
                  liftIO $ treeHasDir start parentDir
              -- If the target exists, remove it; if it doesn't, add all
              -- its parent directories.
              if targetDirExists || targetFileExists
                  then TM.unlink uTo
                  else unless parentDirExists $ TM.createDirectory parentDir
          (InCommit _ _ _ _ newPs _) <- diffCurrent s
          TM.rename uFrom uTo
          let ps' = newPs :<: move uFrom uTo
          current <- updateHashes
          -- ensure empty dirs get deleted
          deleteEmptyParents uFrom
          -- run diffCurrent to add the dir deletions prims
          diffCurrent (InCommit mark ancestors branch current ps' info_)

        -- When we leave the commit, create a patch for the cumulated
        -- prims.
        process (InCommit mark ancestors branch _ ps info_) x = do
          case ancestors of
            (_, []) -> return () -- OK, previous commit is the ancestor
            (Just n, list)
              | n `elem` list -> return () -- OK, we base off one of the ancestors
              | otherwise -> liftIO $ putStrLn $
                               "WARNING: Linearising non-linear ancestry:" ++
                               " currently at " ++ show n ++ ", ancestors " ++ show list
            (Nothing, list) ->
              liftIO $ putStrLn $ "WARNING: Linearising non-linear ancestry " ++ show list

          {- current <- updateHashes -} -- why not?
          (prims :: FL (PrimOf p) cX cY)  <- return $ sortCoalesceFL $ reverseRL ps
          let patch :: Named p cX cY
              patch = infopatch info_ prims
          liftIO $ addToTentativeInventory (repoCache repo)
                                                  GzipCompression (n2pia patch)
          case mark of
            Nothing -> return ()
            Just n -> case getMark marks n of
              Nothing -> liftIO $ modifyIORef marksref $ \m -> addMark m n (patchHash $ n2pia patch)
              Just n' -> fail $ "FATAL: Mark already exists: " ++ decodeLocale n'
          process (Toplevel mark branch) x

        process state obj = do
          liftIO $ print obj
          fail $ "Unexpected object in state " ++ show state

        extractNames :: CopyRenameNames
                     -> TreeIO (BC.ByteString, BC.ByteString)
        extractNames names = case names of
            Quoted f t -> return (f, t)
            Unquoted uqNames -> do
                let spaceIndices = BC.elemIndices ' ' uqNames
                    splitStr = second (BC.drop 1) . flip BC.splitAt uqNames
                    -- Reverse the components, so we find the longest
                    -- prefix existing name.
                    spaceComponents = reverse $ map splitStr spaceIndices
                    componentCount = length spaceComponents
                if componentCount == 1
                    then return $ head spaceComponents
                    else do
                        let dieMessage = unwords
                                [ "Couldn't determine move/rename"
                                , "source/destination filenames, with the"
                                , "data produced by this (old) version of"
                                , "git, since it uses unquoted, but"
                                , "special-character-containing paths."
                                ]
                            lPathExists (l,_) =
                                TM.fileExists $ decodePath l
                            finder [] = error dieMessage
                            finder (x : rest) = do
                                xExists <- lPathExists x
                                if xExists then return x else finder rest
                        finder spaceComponents

    void $ hashedTreeIO (go initial B.empty) pristine pristineDirPath

parseObject :: BC.ByteString -> TreeIO ( BC.ByteString, Object )
parseObject = next' mbObject
  where mbObject = A.parse p_maybeObject

        p_maybeObject = Just `fmap` p_object
                        <|> (A.endOfInput >> return Nothing)

        lex p = p >>= \x -> A.skipSpace >> return x
        lexString s = A.string (BC.pack s) >> A.skipSpace
        line = lex $ A.takeWhile (/='\n')

        optional p = Just `fmap` p <|> return Nothing

        p_object = p_blob
                   <|> p_reset
                   <|> p_commit
                   <|> p_tag
                   <|> p_modify
                   <|> p_rename
                   <|> p_copy
                   <|> p_from
                   <|> p_merge
                   <|> p_delete
                   <|> (lexString "progress" >> Progress `fmap` line)

        p_author name = lexString name >> line

        p_reset = do lexString "reset"
                     branch <- line
                     refid <- optional $ lexString "from" >> p_refid
                     return $ Reset branch refid

        p_commit = do lexString "commit"
                      branch <- line
                      mark <- optional p_mark
                      _ <- optional $ p_author "author"
                      committer <- p_author "committer"
                      message <- p_data
                      return $ Commit branch mark committer message

        p_tag = do _ <- lexString "tag"
                   tag <- line
                   lexString "from"
                   mark <- p_marked
                   author <- p_author "tagger"
                   message <- p_data
                   return $ Tag tag mark author message

        p_blob = do lexString "blob"
                    mark <- optional p_mark
                    Blob mark `fmap` p_data
                  <?> "p_blob"

        p_mark = do lexString "mark"
                    p_marked
                  <?> "p_mark"

        p_refid = MarkId `fmap` p_marked
                  <|> (lexString "inline" >> return Inline)
                  <|> HashId `fmap` p_hash

        p_data = do lexString "data"
                    len <- A.decimal
                    _ <- A.char '\n'
                    lex $ A.take len
                  <?> "p_data"

        p_marked = lex $ A.char ':' >> A.decimal
        p_hash = lex $ A.takeWhile1 (A.inClass "0123456789abcdefABCDEF")
        p_from = lexString "from" >> From `fmap` p_marked
        p_merge = lexString "merge" >> Merge `fmap` p_marked
        p_delete = lexString "D" >> Delete `fmap` p_maybeQuotedName
        p_rename = do lexString "R"
                      names <- p_maybeQuotedCopyRenameNames
                      return $ Rename names
        p_copy = do lexString "C"
                    names <- p_maybeQuotedCopyRenameNames
                    return $ Copy names
        p_modify = do lexString "M"
                      mode <- lex $ A.takeWhile (A.inClass "01234567890")
                      mark <- p_refid
                      path <- p_maybeQuotedName
                      case mark of
                        HashId hash | mode == BC.pack "160000" -> return $ Gitlink hash
                                    | otherwise -> fail ":(("
                        MarkId n -> return $ Modify (Left n) path
                        Inline -> do bits <- p_data
                                     return $ Modify (Right bits) path
        p_maybeQuotedCopyRenameNames =
            p_lexTwoQuotedNames <|> Unquoted `fmap` line
        p_lexTwoQuotedNames = do
            n1 <- lex p_quotedName
            n2 <- lex p_quotedName
            return $ Quoted n1 n2
        p_maybeQuotedName = lex (p_quotedName <|> line)
        p_quotedName = do
          _ <- A.char '"'
          bytes <- many (p_escaped <|> p_unescaped)
          _ <- A.char '"'
          return $ B.concat bytes
        p_unescaped = A.takeWhile1 (\c->c/='"' && c/='\\')
        p_escaped = do
          _ <- A.char '\\'
          p_escaped_octal <|> p_escaped_char
        p_escaped_octal = do
          let octals :: [Char]
              octals = "01234567"
          s <- A.takeWhile1 (`elem` octals)
          let x :: Word8
              x = read ("0o" ++ BC.unpack s)
          return $ B.singleton $ fromIntegral x
        p_escaped_char =
          fmap BC.singleton $
          '\r' <$ A.char 'r' <|> '\n' <$ A.char 'n' <|> A.char '"' <|> A.char '\\'

        next' :: (B.ByteString -> A.Result (Maybe Object)) -> B.ByteString -> TreeIO (B.ByteString, Object)
        next' parser rest =
          do chunk <- if B.null rest then liftIO $ B.hGet stdin (64 * 1024)
                                     else return rest
             next_chunk parser chunk

        next_chunk :: (B.ByteString -> A.Result (Maybe Object)) -> B.ByteString -> TreeIO (B.ByteString, Object)
        next_chunk parser chunk =
          case parser chunk of
             A.Done rest result -> return (rest, maybe End id result) -- not sure about the maybe
             A.Partial cont -> next' cont B.empty
             A.Fail _ ctx err -> do
               liftIO $ putStrLn $ "=== chunk ===\n" ++ decodeLocale chunk ++ "\n=== end chunk ===="
               fail $ "Error parsing stream. " ++ err ++ "\nContext: " ++ show ctx

decodePath :: BC.ByteString -> AnchoredPath
decodePath = floatPath . decodeLocale
