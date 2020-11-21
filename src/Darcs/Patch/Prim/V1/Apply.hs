{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Apply () where

import Darcs.Prelude

import Control.Exception ( throw )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )

import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..),
      DirPatchType(..), FilePatchType(..) )
import Darcs.Patch.Prim.V1.Show ( showHunk )

import Darcs.Util.Path ( AnchoredPath, anchorPath )
import Darcs.Patch.Format ( FileNameFormat(FileNameFormatDisplay) )
import Darcs.Patch.TokenReplace ( tryTokReplace )

import Darcs.Patch.ApplyMonad ( ApplyMonadTree(..) )
import Darcs.Util.Tree( Tree )

import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL_FL, spanFL, (:>)(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePStart )

import Darcs.Util.ByteString ( unlinesPS )
import Darcs.Util.Printer( renderString )

import qualified Data.ByteString as B
    ( ByteString
    , drop
    , empty
    , null
    , concat
    , isPrefixOf
    , length
    , splitAt
    )
import qualified Data.ByteString.Char8 as BC (pack, unpack, unlines, elemIndices)

type FileContents = B.ByteString

ap2fp :: AnchoredPath -> FilePath
ap2fp = anchorPath ""

instance Apply Prim where
    type ApplyState Prim = Tree
    apply (FP f RmFile) = mRemoveFile f
    apply (FP f AddFile) = mCreateFile f
    apply (FP f (Hunk l o n)) = mModifyFilePS f $ applyHunk f (l, o, n)
    apply (FP f (TokReplace t o n)) = mModifyFilePS f doreplace
        where doreplace fc =
                  case tryTokReplace t (BC.pack o) (BC.pack n) fc of
                  Nothing -> throw $ userError $ "replace patch to " ++ ap2fp f
                             ++ " couldn't apply."
                  Just fc' -> return fc'
    apply (FP f (Binary o n)) = mModifyFilePS f doapply
        where doapply oldf = if o == oldf
                             then return n
                             else throw $ userError
                                  $ "binary patch to " ++ ap2fp f
                                  ++ " couldn't apply."
    apply (DP d AddDir) = mCreateDirectory d
    apply (DP d RmDir) = mRemoveDirectory d
    apply (Move f f') = mRename f f'
    apply (ChangePref p f t) = mChangePref p f t

instance RepairToFL Prim where
    applyAndTryToFixFL (FP f RmFile) =
        do x <- mReadFilePS f
           mRemoveFile f
           return $ if B.null x
                        then Nothing
                        else Just ("WARNING: Fixing removal of non-empty file "++ap2fp f,
                                   -- No need to coerce because the content
                                   -- removal patch has freely decided contexts
                                   FP f (Binary x B.empty) :>: FP f RmFile :>: NilFL )
    applyAndTryToFixFL (FP f AddFile) =
        do exists <- mDoesFileExist f
           if exists
             then return $
                     Just ("WARNING: Dropping add of existing file "++ap2fp f,
                           -- the old context was wrong, so we have to coerce
                           unsafeCoercePStart NilFL
                          )
             else do mCreateFile f
                     return Nothing
    applyAndTryToFixFL (DP f AddDir) =
        do exists <- mDoesDirectoryExist f
           if exists
             then return $
                     Just ("WARNING: Dropping add of existing directory "++ap2fp f,
                           -- the old context was wrong, so we have to coerce
                           unsafeCoercePStart NilFL
                          )
             else do mCreateDirectory f
                     return Nothing
    applyAndTryToFixFL (FP f (Binary old new)) =
        do x <- mReadFilePS f
           mModifyFilePS f (\_ -> return new)
           if x /= old
             then return $
                     Just ("WARNING: Fixing binary patch to "++ap2fp f,
                           FP f (Binary x new) :>: NilFL
                          )
             else return Nothing
    applyAndTryToFixFL p = do apply p; return Nothing

instance PrimApply Prim where
    applyPrimFL NilFL = return ()
    applyPrimFL (FP f h@(Hunk{}):>:the_ps)
     = case spanFL f_hunk the_ps of
           (xs :> ps') ->
               do let foo = h :>: mapFL_FL (\(FP _ h') -> h') xs
                  mModifyFilePS f $ hunkmod foo
                  applyPrimFL ps'
        where f_hunk (FP f' (Hunk{})) = f == f'
              f_hunk _ = False
              -- TODO there should be a HOF that abstracts
              -- over this recursion scheme
              hunkmod :: Monad m => FL FilePatchType wX wY
                      -> B.ByteString -> m B.ByteString
              hunkmod NilFL content = return content
              hunkmod (Hunk line old new:>:hs) content =
                  applyHunk f (line, old, new) content >>= hunkmod hs
              hunkmod _ _ = error "impossible case"
    applyPrimFL (p:>:ps) = apply p >> applyPrimFL ps

applyHunk :: Monad m
          => AnchoredPath
          -> (Int, [B.ByteString], [B.ByteString])
          -> FileContents
          -> m FileContents
applyHunk f h fc =
  case applyHunkLines h fc of
    Right fc' -> return fc'
    Left msg ->
      throw $ userError $
      "### Error applying:\n" ++ renderHunk h ++
      "\n### to file " ++ ap2fp f ++ ":\n" ++ BC.unpack fc ++
      "### Reason: " ++ msg
  where
    renderHunk (l, o, n) = renderString (showHunk FileNameFormatDisplay f l o n)

{- The way darcs handles newlines is not easy to understand.

Everything seems pretty logical and conventional as long as files end in a
newline. In this case, the lines in a hunk can be regarded as newline
terminated, too. However, this view breaks down if we consider files that
are not newline terminated.

Here is a different view that covers the general case and explains,
conceptually, the algorithm below.

* Ever line (in a hunk or file) is regarded as being /preceded/ by a newline
  character.

* Every file starts out containing a single newline character, that is, a
  single empty line. A first empty line at the start of a file (if present)
  is /invisible/.

* When lines are appended to a file by a hunk, they are inserted /before/ a
  final empty line, if there is one. This results in a file that remains
  being terminated by a newline.

* In particular, when we start with an empty file and add a line, we push
  the invisible newline back, making it visible, and the newline that
  initiates our new content becomes invisible instead. This results in a
  newline terminated file, as above.

* However, if there is a newline at the end of a file (remember that this
  includes the case of an empty file), a hunk can /remove/ it by removing an
  empty line before adding anything. This results in a file that is /not/
  newline terminated.

The invisible newline character at the front is, of course, not present
anywhere in the representation of files, it is just a conceptual tool.

The algorithm below is highly optimized to minimize allocation of
intermediate ByteStrings. -}

applyHunkLines :: (Int, [B.ByteString], [B.ByteString])
               -> FileContents
               -> Either String FileContents
applyHunkLines (line, old, new) content
  | line == 1 =
      {- This case is subtle because here we have to deal with any invisible
      newline at the front of a file without it actually being present. We
      first try to drop everything up to the (length old)'th newline. 

      If this fails, we know that the content was not newline terminated. So
      we replace everything with the new content, interspersing but not
      terminating the lines with newline characters.

      If it succeeds, we insert the new content, interspersing /and/
      terminating the lines with newline characters before appending the
      rest of the content. -}
      case breakAfterNthNewline (length old) content of
        Nothing
          -- old content is not newline terminated
          | content == unlinesPS old -> Right $ unlinesPS new
          | otherwise -> Left "Hunk wants to remove content that isn't there"
        Just (should_be_old, suffix)
          -- old content is newline terminated
          | should_be_old == BC.unlines old ->
              Right $ unlinesPS $ new ++ [suffix]
          | otherwise ->
              Left "Hunk wants to remove content that isn't there"
  | line >= 2 = do
      {- This is the simpler case. We can be sure that we have at least one
      newline character at the point where we modify the file. This means we
      can apply the conceptual view literally, i.e. replace old content with
      new content /before/ this newline, where the lines in the old and new
      content are /preceded/ by newline characters. -}
      (pre, start) <- breakBeforeNthNewline (line-2) content
      let hunkContent ls = unlinesPS (B.empty:ls)
      post <- dropPrefix (hunkContent old) start
      return $ B.concat [pre, hunkContent new, post]
  | otherwise = Left "Hunk has zero or negative line number"
  where
    dropPrefix x y
      | x `B.isPrefixOf` y = Right $ B.drop (B.length x) y
      | otherwise =
        Left $ "Hunk wants to remove content that isn't there"

breakAfterNthNewline :: Int -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
breakAfterNthNewline 0 the_ps = Just (B.empty, the_ps)
breakAfterNthNewline n _ | n < 0 = error "precondition of breakAfterNthNewline"
breakAfterNthNewline n the_ps = go n (BC.elemIndices '\n' the_ps)
  where
    go _ [] = Nothing -- we have fewer than n newlines
    go 1 (i:_) = Just $ B.splitAt (i + 1) the_ps
    go !m (_:is) = go (m - 1) is

breakBeforeNthNewline :: Int -> B.ByteString -> Either String (B.ByteString, B.ByteString)
breakBeforeNthNewline n _ | n < 0 = error "precondition of breakBeforeNthNewline"
breakBeforeNthNewline n the_ps = go n (BC.elemIndices '\n' the_ps)
  where
    go 0 [] = Right (the_ps, B.empty)
    go 0 (i:_) = Right $ B.splitAt i the_ps
    go !m (_:is) = go (m - 1) is
    go _ [] = Left "Line number does not exist"
