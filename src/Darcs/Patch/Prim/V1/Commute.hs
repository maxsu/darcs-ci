{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Commute
    ( Perhaps(..)
    , toPerhaps
    , CommuteFunction
    , speedyCommute
    , cleverCommute
    , commuteFiledir
    , commuteFilepatches
    ) where

import Darcs.Prelude

import Control.Monad ( MonadPlus, msum, mzero, mplus )
import Control.Applicative ( Alternative(..) )

import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

import Darcs.Util.Path ( AnchoredPath, movedirfilename, isPrefix )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..) )
import Darcs.Patch.Prim.V1.Core
     ( Prim(..), FilePatchType(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( primCleanMerge )
import Darcs.Patch.TokenReplace ( tryTokReplace )

isSuperdir :: AnchoredPath -> AnchoredPath -> Bool
isSuperdir d1 d2 = isPrefix d1 d2 && d1 /= d2

{-
This is the original definition.
Note that it explicitly excludes equality:

isSuperdir d1 d2 = isd (fn2fp d1) (fn2fp d2)
  where
    isd s1 s2 =
      length s2 >= length s1 + 1 && take (length s1 + 1) s2 == s1 ++ "/"
-}

isInDirectory :: AnchoredPath -> AnchoredPath -> Bool
isInDirectory = isPrefix
{-
Again, here is the orginial definition:
isInDirectory d f = iid (fn2fp d) (fn2fp f)
    where iid (cd:cds) (cf:cfs)
              | cd /= cf = False
              | otherwise = iid cds cfs
          iid [] ('/':_) = True
          iid [] [] = True -- Count directory itself as being in directory...
          iid _ _ = False
-}

data Perhaps a = Unknown | Failed | Succeeded a

instance Functor Perhaps where
    fmap _ Unknown = Unknown
    fmap _ Failed = Failed
    fmap f (Succeeded x) = Succeeded (f x)

instance Applicative Perhaps where
    pure = Succeeded
    _ <*> Failed = Failed
    _ <*> Unknown = Unknown
    Failed <*> _ = Failed
    Unknown <*> _ = Unknown
    Succeeded f <*> Succeeded x = Succeeded (f x)

instance  Monad Perhaps where
    (Succeeded x) >>= k =  k x
    Failed   >>= _      =  Failed
    Unknown  >>= _      =  Unknown
    return              =  Succeeded

instance Alternative Perhaps where
    empty = Unknown
    Unknown <|> ys    = ys
    Failed  <|> _     = Failed
    (Succeeded x) <|> _ = Succeeded x

instance  MonadPlus Perhaps where
    mzero = Unknown
    mplus = (<|>)

toMaybe :: Perhaps a -> Maybe a
toMaybe (Succeeded x) = Just x
toMaybe _ = Nothing

toPerhaps :: Maybe a -> Perhaps a
toPerhaps (Just x) = Succeeded x
toPerhaps Nothing = Failed

cleverCommute :: CommuteFunction -> CommuteFunction
cleverCommute c (p1:>p2) =
    case c (p1 :> p2) of
    Succeeded x -> Succeeded x
    Failed -> Failed
    Unknown -> case c (invert p2 :> invert p1) of
               Succeeded (p1' :> p2') -> Succeeded (invert p2' :> invert p1')
               Failed -> Failed
               Unknown -> Unknown
--cleverCommute c (p1,p2) = c (p1,p2) `mplus`
--    (case c (invert p2,invert p1) of
--     Succeeded (p1', p2') -> Succeeded (invert p2', invert p1')
--     Failed -> Failed
--     Unknown -> Unknown)

speedyCommute :: CommuteFunction  -- Deal with common cases quickly!
    -- Two file-patches modifying different files trivially commute.
speedyCommute (p1@(FP f1 _) :> p2@(FP f2 _))
  | f1 /= f2 = Succeeded (unsafeCoerceP p2 :> unsafeCoerceP p1)
speedyCommute _other = Unknown

everythingElseCommute :: CommuteFunction
everythingElseCommute = eec
  where
    eec :: CommuteFunction
    eec (p1 :> ChangePref p f t) = Succeeded (ChangePref p f t :> unsafeCoerceP p1)
    eec (ChangePref p f t :> p2) = Succeeded (unsafeCoerceP p2 :> ChangePref p f t)
    eec xx = cleverCommute commuteFiledir xx

{-
Note that it must be true that

commutex (A^-1 A, P) = Just (P, A'^-1 A')

and

if commutex (A, B) == Just (B', A')
then commutex (B^-1, A^-1) == Just (A'^-1, B'^-1)
-}

instance Commute Prim where
    commute x = toMaybe $ msum [speedyCommute x,
                                everythingElseCommute x
                               ]

commuteFiledir :: CommuteFunction
commuteFiledir (FP f1 p1 :> FP f2 p2) =
  if f1 /= f2 then Succeeded ( FP f2 (unsafeCoerceP p2) :> FP f1 (unsafeCoerceP p1) )
  else commuteFP f1 (p1 :> p2)
commuteFiledir (DP d1 p1 :> DP d2 p2) =
  if not (isInDirectory d1 d2 || isInDirectory d2 d1) && d1 /= d2
  then Succeeded ( DP d2 (unsafeCoerceP p2) :> DP d1 (unsafeCoerceP p1) )
  else Failed
commuteFiledir (FP f fp :> DP d dp) =
    if not $ isInDirectory d f
    then Succeeded (DP d (unsafeCoerceP dp) :> FP f (unsafeCoerceP fp))
    else Failed

-- FIXME using isSuperdir here makes no sense, should use just isPrefix

commuteFiledir (FP f1 p1 :> Move d d')
    | f1 == d' = Failed
    | (p1 == AddFile || p1 == RmFile) && d == f1 = Failed
    | otherwise = Succeeded (Move d d' :> FP (movedirfilename d d' f1) (unsafeCoerceP p1))
commuteFiledir (DP d1 p1 :> Move d d')
    | isSuperdir d1 d' || isSuperdir d1 d = Failed
    | d == d1 = Failed  -- The exact guard is p1 == AddDir && d == d1
                        -- but note d == d1 suffices because we know p1 != RmDir
                        -- (and hence p1 == AddDir) since patches must be sequential.
    | d1 == d' = Failed
    | otherwise = Succeeded (Move d d' :> DP (movedirfilename d d' d1) (unsafeCoerceP p1))
commuteFiledir (Move f f' :> Move d d')
    | f == d' || f' == d = Failed
    | f == d || f' == d' = Failed
    | d `isSuperdir` f && f' `isSuperdir` d' = Failed
    | otherwise =
        Succeeded (Move (movedirfilename f' f d) (movedirfilename f' f d') :>
                   Move (movedirfilename d d' f) (movedirfilename d d' f'))

commuteFiledir _ = Unknown

type CommuteFunction = forall wX wY . (Prim :> Prim) wX wY -> Perhaps ((Prim :> Prim) wX wY)

commuteFilepatches :: CommuteFunction
commuteFilepatches (FP f1 p1 :> FP f2 p2) | f1 == f2 = commuteFP f1 (p1 :> p2)
commuteFilepatches _ = Unknown

commuteFP :: AnchoredPath -> (FilePatchType :> FilePatchType) wX wY
          -> Perhaps ((Prim :> Prim) wX wY)
commuteFP f (p1 :> Hunk line1 [] []) =
    Succeeded (FP f (Hunk line1 [] []) :> FP f (unsafeCoerceP p1))
commuteFP f (Hunk line1 [] [] :> p2) =
    Succeeded (FP f (unsafeCoerceP p2) :> FP f (Hunk line1 [] []))
commuteFP f (Hunk line1 old1 new1 :> Hunk line2 old2 new2) =
    case commuteHunkLines line1 (length old1) (length new1) line2 (length old2) (length new2) of
      Just (line2', line1') ->
        Succeeded (FP f (Hunk line2' old2 new2) :> FP f (Hunk line1' old1 new1))
      Nothing -> Failed
commuteFP f (Hunk line1 old1 new1 :> TokReplace t o n) =
    let po = BC.pack o; pn = BC.pack n in
    case tryTokReplaces t po pn old1 of
    Nothing -> Failed
    Just old1' ->
      case tryTokReplaces t po pn new1 of
        Nothing -> Failed
        Just new1' -> Succeeded (FP f (TokReplace t o n) :>
                                 FP f (Hunk line1 old1' new1'))
commuteFP f (TokReplace t1 o1 n1 :> TokReplace t2 o2 n2)
    | t1 /= t2 = Failed
    | o1 == o2 = Failed
    | n1 == o2 = Failed
    | o1 == n2 = Failed
    | n1 == n2 = Failed
    | otherwise = Succeeded (FP f (TokReplace t2 o2 n2) :>
                             FP f (TokReplace t1 o1 n1))
commuteFP _ _ = Unknown

commuteHunkLines :: Int -> Int -> Int -> Int -> Int -> Int
                 -> Maybe (Int, Int)
commuteHunkLines line1 len_old1 len_new1 line2 len_old2 len_new2
  | line1 + len_new1 < line2  = Just (line2 - len_new1 + len_old1, line1)
  | line2 + len_old2 < line1  = Just (line2, line1 + len_new2 - len_old2)
  | len_old2 /= 0
  , len_old1 /= 0
  , len_new2 /= 0
  , len_new1 /= 0
  , line1 + len_new1 == line2 = Just (line2 - len_new1 + len_old1, line1)
  | len_old2 /= 0
  , len_old1 /= 0
  , len_new2 /= 0
  , len_new1 /= 0
  , line2 + len_old2 == line1 = Just (line2, line1 + len_new2 - len_old2)
  | otherwise                 = Nothing

tryTokReplaces :: String -> B.ByteString -> B.ByteString
               -> [B.ByteString] -> Maybe [B.ByteString]
tryTokReplaces t o n = mapM (tryTokReplace t o n)

instance CleanMerge Prim where
  cleanMerge = primCleanMerge
