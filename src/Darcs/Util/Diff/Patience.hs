--  Copyright (C) 2002,2008-2009 David Roundy
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
--  along with this program; if not, write to the Free Software Foundation,
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

module Darcs.Util.Diff.Patience
    ( getChanges
    ) where

import Darcs.Prelude

import Data.List ( sort )
import Data.Maybe ( fromJust )
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Set as S
import qualified Data.ByteString as B ( ByteString, elem )
import qualified Data.ByteString.Char8 as BC ( pack )
import qualified Data.Map.Strict as M
    ( Map, lookup, insertWith, empty, elems )
import qualified Data.Hashable as H ( hash )
import Darcs.Util.Diff.Myers (initP, aLen, PArray, getSlice)

empty :: HunkMap
empty = HunkMapInfo 0 M.empty

getChanges ::  [B.ByteString] -> [B.ByteString]
           -> [(Int,[B.ByteString],[B.ByteString])]
getChanges a b = dropStart (initP a) (initP b) 1

dropStart ::  PArray -> PArray -> Int
           -> [(Int,[B.ByteString],[B.ByteString])]
dropStart a b off
  | off > aLen a = [(off - 1, [], getSlice b off (aLen b))]
  | off > aLen b = [(off - 1, getSlice a off (aLen a), [])]
  | a!off == b!off = dropStart a b (off + 1)
  | otherwise      = dropEnd a b off 0

dropEnd ::  PArray -> PArray -> Int -> Int
        -> [(Int,[B.ByteString],[B.ByteString])]
dropEnd a b off end
    | off > alast        = [(off - 1, [], getSlice b off blast)]
    | off > blast        = [(off - 1, getSlice a off alast, [])]
    | a!alast == b!blast = dropEnd a b off (end + 1)
    | otherwise          = getChanges' (off-1) (getSlice a off (aLen a - end')) (getSlice b off (aLen b - end'))
  where end' = addBorings end -- don't drop Borings just in case. See hidden_conflict2.sh
        addBorings e | e > 0 && a!(aLen a - (e-1)) `elem` borings' = addBorings (e-1)
                     | otherwise = e
        alast = aLen a - end
        blast = aLen b - end

getChanges' :: Int -> [B.ByteString] -> [B.ByteString]
              -> [(Int, [B.ByteString], [B.ByteString])]
getChanges' off o n = convertLBS [] $ genNestedChanges [byparagraph, bylines] off oh nh
            where
              (_,m') = listToHunk borings' empty
              (oh,m) = listToHunk o m'
              (nh,lmap) = listToHunk n m
              convertLBS ys [] = reverse ys
              convertLBS ys ((i,os,ns):xs) = convertLBS ((i, hunkToBS os, hunkToBS ns):ys) xs
              hunkToBS hs = map (\h -> (!) harray (abs h)) hs
              harray = getBArray lmap

type HMap = M.Map
type Hash = Int
type Hunk = Int
data HunkMap = HunkMapInfo Int (HMap Hash [(Hunk, B.ByteString)])

getMap :: HunkMap -> HMap Hash [(Hunk, B.ByteString)]
getMap (HunkMapInfo _ m) = m

getSize :: HunkMap -> Int
getSize (HunkMapInfo s _) = s

getBArray :: HunkMap -> Array Hunk B.ByteString
getBArray (HunkMapInfo size b) = array (1,size) $ map (\(x,a) -> (abs x, a)) $ concat $ M.elems b

insert :: Hash -> B.ByteString -> HunkMap -> (Hunk, HunkMap)
insert h bs hmap = (hunknumber, HunkMapInfo newsize (M.insertWith (\_ o -> (hunknumber,bs):o) h [(hunknumber,bs)] $ getMap hmap))
      where hunknumber = if B.elem nl bs then -newsize -- used by bylines
                                         else newsize
            newsize = getSize hmap+1
            nl = 10 -- '\n'

--Given a HunkMap, check collisions and return the line with an updated Map
toHunk' :: HunkMap -> B.ByteString -> (Hunk, HunkMap)
toHunk' lmap bs | oldbs == Nothing || null oldhunkpair = insert hash bs lmap
                | otherwise = (fst $ head oldhunkpair, lmap)
                    where hash = H.hash bs
                          oldbs = M.lookup hash (getMap lmap)
                          oldhunkpair = filter ((== bs) . snd) $ fromJust oldbs

listToHunk :: [B.ByteString] -> HunkMap -> ([Hunk], HunkMap)
listToHunk [] hmap = ([], hmap)
listToHunk (x:xs) hmap = let (y, hmap') = toHunk' hmap x
                             (ys, hmap'') = listToHunk xs hmap'
                         in (y:ys, hmap'')

--listToHunk :: [B.ByteString] -> HunkMap -> ([Hunk], HunkMap)
--listToHunk = listToHunk' []
--      where listToHunk' xs [] hmap = (reverse xs, hmap)
--            listToHunk' xs (y:ys) hmap = let (h,hmap') = toHunk' hmap y
--                                         in listToHunk' (h:xs) ys hmap'


genNestedChanges :: [[Hunk] -> [[Hunk]]]
                 -> Int -> [Hunk] -> [Hunk]
                 -> [(Int, [Hunk], [Hunk])]
genNestedChanges (br:brs) i0 o0 n0 = nc i0 (lcus ol nl) ol nl
    where nl = br n0
          ol = br o0
          nc i [] o n = easydiff i o n
          nc i (x:xs) o n =
              case break (==x) o of
                (oa, _:ob) ->
                    case break (==x) n of
                      (na, _:nb) ->
                         i' `seq` easydiff i oa na ++ nc i' xs ob nb
                             where i' = i + length (concat na) + length x
                      (_,[]) -> error "impossible case"
                (_,[]) -> error "impossible case"
          easydiff i o n = genNestedChanges brs i oo nn
              where (oo, nn) = (concat o, concat n)
genNestedChanges [] i o n = mkdiff (all (`elem` borings)) i mylcs o n
        where mylcs = patientLcs (filter (`notElem` borings) o)
                                 (filter (`notElem` borings) n)

borings :: [Hunk]
borings = fst $ listToHunk borings' empty

borings' :: [B.ByteString]
borings' = map BC.pack ["", "\n", " ", ")", "(", ","]

byparagraph :: [Hunk] -> [[Hunk]]
byparagraph = reverse . map reverse . byparagraphAcc []
    where byparagraphAcc xs [] = xs
          byparagraphAcc [] (a:b:c:d)
              | a == nl && c == nl && b == hnull = case d of
                                                   [] -> [[c,b,a]]
                                                   _  -> byparagraphAcc [[],[c,b,a]] d
          byparagraphAcc [] (a:as) = byparagraphAcc [[a]] as
          byparagraphAcc (x:xs) (a:b:c:d)
              | a == nl && c == nl && b == hnull = case d of
                                                   [] -> (c:b:a:x):xs
                                                   _  -> byparagraphAcc ([]:((c:b:a:x):xs)) d
          byparagraphAcc (x:xs) (a:as) = byparagraphAcc ((a:x):xs) as
          nl = -1 -- "\n" hunk
          hnull = 1 -- "" hunk toHunk $ BC.pack ""

bylines :: [Hunk] -> [[Hunk]]
bylines = reverse . bylinesAcc []
  where bylinesAcc !ys [] = ys
        bylinesAcc !ys xs = case break (<0) xs of
                              (_,[]) -> xs:ys
                              (a,n:b) -> bylinesAcc ((a++[n]):ys) b


-- | the longest common subsequence of unique items

lcus :: Ord a => [a] -> [a] -> [a]
lcus xs0 ys0 = lcs (filter (`S.member`u) xs0) (filter (`S.member`u) ys0)
    where uxs = findUnique xs0
          uys = findUnique ys0
          u = S.intersection uxs uys
          findUnique xs = S.fromList $ gru $ sort xs
          gru (x:x':xs) | x == x' = gru (dropWhile (==x) xs)
          gru (x:xs) = x : gru xs
          gru [] = []


mkdiff :: Ord a =>
          ([a] -> Bool) -> Int -> [a] -> [a] -> [a] -> [(Int,[a],[a])]
mkdiff b ny (l:ls) (x:xs) (y:ys)
    | l == x && l == y = mkdiff b (ny+1) ls xs ys
mkdiff boring ny (l:ls) xs ys
  | rmd == add = mkdiff boring (ny+length add+1) ls restx resty
  | boring rmd && boring add =
      case lcs rmd add of
        [] -> prefixPostfixDiff ny rmd add ++
              mkdiff boring (ny+length add+1) ls restx resty
        ll -> mkdiff (const False) ny ll rmd add ++
              mkdiff boring  (ny+length add+1) ls restx resty
  | otherwise = prefixPostfixDiff ny rmd add ++
                mkdiff boring (ny+length add+1) ls restx resty
    where rmd = takeWhile (/= l) xs
          add = takeWhile (/= l) ys
          restx = drop (length rmd + 1) xs
          resty = drop (length add + 1) ys
mkdiff _ _ [] [] [] = []
mkdiff boring ny [] rmd add
  | boring rmd && boring add =
      case lcs rmd add of
        [] -> prefixPostfixDiff ny rmd add
        ll -> mkdiff (const False) ny ll rmd add
  | otherwise = prefixPostfixDiff ny rmd add

prefixPostfixDiff :: Ord a => Int -> [a] -> [a] -> [(Int,[a],[a])]
prefixPostfixDiff _ [] [] = []
prefixPostfixDiff ny [] ys = [(ny,[],ys)]
prefixPostfixDiff ny xs [] = [(ny,xs,[])]
prefixPostfixDiff ny (x:xs) (y:ys)
    | x == y = prefixPostfixDiff (ny+1) xs ys
    | otherwise = [(ny, reverse rxs', reverse rys')]
    where (rxs',rys') = dropPref (reverse (x:xs)) (reverse (y:ys))
          dropPref (a:as) (b:bs) | a == b = dropPref as bs
          dropPref as bs = (as,bs)

-- | The patientLcs algorithm is inspired by the "patience" algorithm
-- (for which I don't have a reference handy), in that it looks for
-- unique lines, and uses them to subdivide the problem.  I use lcs to
-- diff the unique lines.  It is slower, but should lead to "better"
-- diffs, in the sense of ones that better align with what humans
-- think changed.
-- 
-- Note that when compared with the Meyers algorithm used in darcs,
-- this is somewhat slower (maybe 4x in some of my tests), but is
-- lacking its stack overflow problem.  I'm not sure how it scales in
-- general, but it scales fine (just 10x slower than GNU diff) when
-- comparing a 6M american english dictionary with a british english
-- dictionary of the same size (which isn't a great test, but is the
-- largest pair of somewhat-differing files I could find).
-- 
-- Note that the patientLcs algorithm is slower than the one used in
-- lcs for sequences with mostly unique elements (as is common in text
-- files), but much *faster* when the sequence has a high degree of
-- redundancy.  i.e. lines /usr/share/dict/words vs lines (cat
-- /usr/share/dict/words | tr 'a-z' 'a')

{-# SPECIALIZE patientLcs ::[Hunk] -> [Hunk] -> [Hunk] #-}
patientLcs :: Ord a => [a] -> [a] -> [a]
patientLcs [] _ = []
patientLcs _ [] = []
patientLcs (c1:c1s) (c2:c2s)
    | c1 == c2 = c1: patientLcs c1s c2s
    | otherwise =
        reverse $ patientLcs0 (reverse (c1:c1s)) (reverse (c2:c2s))

patientLcs0 :: Ord a => [a] -> [a] -> [a]
patientLcs0 xs0@(cc1:cc1s) ys0@(cc2:cc2s)
    | cc1 == cc2 = cc1 : patientLcs0 cc1s cc2s
    | otherwise = case (filter (`S.member`uys) xs0, filter (`S.member`uxs) ys0) of
                    ([],_) -> lcs xs0 ys0
                    (_,[]) -> lcs xs0 ys0
                    (xs',ys') -> joinU (lcs xs' ys') xs0 ys0
    where uxs = findUnique xs0
          uys = findUnique ys0
          joinU [] x y = lcs x y
          joinU (b:bs) cs ds =
                 case break (==b) cs of
                   ([],_:c2) -> b : joinU bs c2 (drop 1 $ dropWhile (/= b) ds)
                   (c1,_:c2) -> case break (==b) ds of
                                  ([],_:d2) -> b : joinU bs c2 d2
                                  (d1,_:d2) -> lcs c1 d1 ++ b : joinU bs c2 d2
                                  _ -> error "impossible case"
                   _ -> error "impossible case"
          findUnique xs = S.fromList $ gru $ sort xs
          gru (x:x':xs) | x == x' = gru (dropWhile (==x) xs)
          gru (x:xs) = x : gru xs
          gru [] = []
          --findUnique xs = fu S.empty S.empty xs
          --    where fu _ uni [] = uni
          --          fu multi uni (y:ys)
          --              | y `S.member` multi = fu multi uni ys
          --              | y `S.member` uni = fu (S.insert y multi) (S.delete y uni) ys
          --              | otherwise = fu multi (S.insert y uni) ys
patientLcs0 [] _ = []
patientLcs0 _ [] = []

-- | ``LCS'' stands for ``Longest Common Subsequence,'' and it is a relatively
-- challenging problem to find an LCS efficiently.  I'm not going to explain
-- here what an LCS is, but will point out that it is useful in finding how
-- two sequences (lists, in this case) differ.  This module implements the
-- Hunt-Szymanski algorithm, which is appropriate for applications in which
-- the sequence is on an infinite alphabet, such as diffing the lines in two
-- files, where many, or most lines are unique.  In the best case scenario, a
-- permutation of unique lines, this algorithm is $O(n\log n)$.  In the worst
-- case scenario, that of a finite alphabet (i.e.\ where the number of elements
-- in the sequence is much greater than the number of unique elements), it is
-- an $O(n^2\log n)$ algorithm, which is pretty terrible.

{-# SPECIALIZE lcs ::[Hunk] -> [Hunk] -> [Hunk] #-}
lcs :: Ord a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs (c1:c1s) (c2:c2s)
    | c1 == c2 = c1: lcs c1s c2s
    | otherwise =
        reverse $ lcsSimple (reverse (c1:c1s)) (reverse (c2:c2s))

lcsSimple :: Ord a => [a] -> [a] -> [a]
lcsSimple [] _ = []
lcsSimple _ [] = []
lcsSimple s1@(c1:c1s) s2@(c2:c2s)
    | c1 == c2 = c1: lcs c1s c2s
    | otherwise = hunt $ pruneMatches s1 $! findMatches s1 s2

pruneMatches :: [a] -> [[Int]] -> [(a, [Int])]
pruneMatches _ [] = []
pruneMatches [] _ = []
pruneMatches (_:cs) ([]:ms) = pruneMatches cs ms
pruneMatches (c:cs) (m:ms) = (c,m): pruneMatches cs ms

type Threshold s a = STArray s Int (Int,[a])

hunt :: [(a, [Int])] -> [a]
hunt [] = []
hunt csmatches =
    runST ( do th <- emptyThreshold (length csmatches) l
               huntInternal csmatches th
               huntRecover th (-1) l )
    where l = maximum (0 : concat (map snd csmatches))

huntInternal :: [(a, [Int])] -> Threshold s a -> ST s ()
huntInternal [] _ = return ()
huntInternal ((c,m):csms) th = do
    huntOneChar c m th
    huntInternal csms th

huntOneChar :: a -> [Int] ->  Threshold s a -> ST s ()
huntOneChar _ [] _ = return ()
huntOneChar c (j:js) th = do
    index_k <- myBs j th
    case index_k of
      Nothing -> return ()
      Just k -> do
        (_, rest) <- readArray th (k-1)
        writeArray th k (j, c:rest)
    huntOneChar c js th

-- This is O(n), which is stupid.
huntRecover :: Threshold s a -> Int -> Int -> ST s [a]
huntRecover th n limit =
 do (_, th_max) <- getBounds th
    if n < 0
       then huntRecover th th_max limit
       else if n == 0 || n > th_max
            then return []
            else do (thn, sn) <- readArray th n
                    if thn <= limit
                      then return $ reverse sn
                      else huntRecover th (n-1) limit

emptyThreshold :: Int -> Int -> ST s (Threshold s a)
emptyThreshold l th_max = do
  th <- newArray (0,l) (th_max+1, [])
  writeArray th 0 (0, [])
  return th

myBs :: Int -> Threshold s a -> ST s (Maybe Int)
myBs j th = do bnds <- getBounds th
               myHelperBs j bnds th

myHelperBs :: Int -> (Int,Int) -> Threshold s a ->
                ST s (Maybe Int)
myHelperBs j (th_min,th_max) th =
    if th_max - th_min > 1 then do
       (midth, _) <- readArray th th_middle
       if j > midth
         then myHelperBs j (th_middle,th_max) th
         else myHelperBs j (th_min,th_middle) th
    else do
       (minth, _) <- readArray th th_min
       (maxth, _) <- readArray th th_max
       if minth < j && maxth > j
          then return $ Just th_max
          else if j < minth then return $ Just th_min
               else return Nothing
    where th_middle = (th_max+th_min) `div` 2



findMatches :: Ord a => [a] -> [a] -> [[Int]]
findMatches [] [] = []
findMatches [] (_:bs) = []: findMatches [] bs
findMatches _ [] = []
findMatches a b =
    unzipIndexed $ sort $ findSortedMatches indexeda indexedb [] []
    where indexeda = sort $ zip a [1..]
          indexedb = sort $ zip b [1..]

unzipIndexed :: [(Int,[a])] -> [[a]]
unzipIndexed s = unzipIndexedHelper 1 s
    where unzipIndexedHelper _ [] = []
          unzipIndexedHelper thisl ((l,c):rest)
           | thisl == l = c: unzipIndexedHelper (l+1) rest
           | otherwise = []: unzipIndexedHelper (thisl+1) ((l,c):rest)

findSortedMatches :: Ord a => [(a, Int)] -> [(a, Int)] -> [a] -> [Int]
                             -> [(Int, [Int])]
findSortedMatches [] _ _ _ = []
findSortedMatches _ [] _ _ = []
findSortedMatches ((a,na):as) ((b,nb):bs) aold aoldmatches
    | [a] == aold = (na, aoldmatches) :
                    findSortedMatches as ((b,nb):bs) aold aoldmatches
    | a > b = findSortedMatches ((a,na):as) bs aold aoldmatches
    | a < b = findSortedMatches as ((b,nb):bs) aold aoldmatches
-- following line is inefficient if a line is repeated many times.
findSortedMatches ((a,na):as) bs _ _ -- a == b
      = (na, matches) : findSortedMatches as bs [a] matches
    where matches = reverse $ map snd $ filter ((==a) . fst) bs
