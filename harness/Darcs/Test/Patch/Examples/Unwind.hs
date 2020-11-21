-- BSD3
--
-- This file contains examples found during the development of Darcs.Patch.Unwind

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Darcs.Test.Patch.Examples.Unwind where

import Darcs.Prelude

import Darcs.Patch.FromPrim
import Darcs.Patch.Info
import Darcs.Patch.Merge
import Darcs.Patch.Named
import Darcs.Patch.V1 ()
import Darcs.Patch.V1.Core
import Darcs.Patch.Prim.Class
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed

import Darcs.Util.Path
import Darcs.Util.Tree

import Darcs.Test.HashedStorage ( unsafeMakeName )
import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Named ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.V1Model
import Darcs.Test.Patch.WithState
import Darcs.Test.TestOnly.Instance ()

#if MIN_VERSION_base(4,12,0) && !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif
import Data.ByteString.Char8 ( pack )
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Constraint
import Data.String

examples
  :: forall p
   . (ArbitraryRepoPatch p, ArbitraryPrim (OnlyPrim p))
   => [Sealed2 (WithStartState2 (MergeableSequence (Named p)))]
examples =
  case (hasPrimConstruct @(OnlyPrim p), usesV1Model @(PrimOf p), notRepoPatchV1 @p) of
    (Just Dict, Just Dict, Just _) -> [example1, example2, example3, example4]
    (Just Dict, Just Dict, Nothing) -> [example1, example2, example3]
    _ -> []

mkNamed :: String -> FL p wX wY -> Named p wX wY
mkNamed hash = NamedP (rawPatchInfo "" "" "" ["Ignore-this: "++hash] False) []

path :: String -> AnchoredPath
path s = AnchoredPath [unsafeMakeName s]

repo :: [(String, [BLC.ByteString])] -> V1Model wX
repo entries =
  makeRepo [ (unsafeMakeName s, RepoItem (File (makeBlob (BLC.unlines thelines))))
           | (s, thelines) <- entries
           ]

example1
  :: forall p
   . (FromPrim p, PrimConstruct (OnlyPrim p), ModelOf p ~ V1Model)
   => Sealed2 (WithStartState2 (MergeableSequence (Named p)))
example1 =
  Sealed2
    (WithStartState2
      (repo [("a", [])])
      (ParMS
        (SeqMS
          NilMS
          (mkNamed "d4c5605cd6b83371fec990c1835c6416a187ba0"
            (hunk (path "a") 1 [] [pack "x"] :>: NilFL)))
        (SeqMS
          NilMS
          (mkNamed "bb201b95265f199e7fd98a7e2216a0add4fece68"
            (hunk (path "a") 1 [] [pack "y"] :>:
             hunk (path "a") 1 [pack "y"] [pack "z"] :>:
             NilFL)))))

example2
  :: forall p
   . (FromPrim p, PrimConstruct (OnlyPrim p), ModelOf p ~ V1Model)
   => Sealed2 (WithStartState2 (MergeableSequence (Named p)))
example2 =
  let s3,s4 :: forall s . IsString s => [s]
      s3 = ["f"]
      s4 = ["g"]
  in
  Sealed2
    (WithStartState2
      (repo [("a", s3++s4)])
      (ParMS
        (ParMS
          (SeqMS
            NilMS
              (mkNamed "e8204d03b3f785675ce4bd676adbfe89c4979399"
                (hunk (path "a") 1 (map pack s3) [] :>: NilFL)))
          (SeqMS
            NilMS
              (mkNamed "ac22106fec2b81ae4f75039210f0601dbc9f677d"
                (hunk (path "a") 1 (map pack (s3++s4)) [] :>: NilFL))))
        (SeqMS
          NilMS
          (mkNamed "4f5a9665900931228596a8241ff03dea1a54805f"
            (
             hunk (path "a") (1 + length s3 + length s4) [] [pack "d"] :>:
             hunk (path "a") 1 (map pack s3) [] :>:
            NilFL)))))


example3
  :: forall p
   . (FromPrim p, PrimConstruct (OnlyPrim p), ModelOf p ~ V1Model)
   => Sealed2 (WithStartState2 (MergeableSequence (Named p)))
example3 =
  let
    s4 :: IsString s => [s]
    s4 = ["w"]
  in
  Sealed2
    (WithStartState2
      (repo [("a", s4)])
      (ParMS
        (SeqMS
          (ParMS
            (SeqMS NilMS
               (mkNamed "e631e70c43b4e609830053068eef382a4b2fec7"
                 (
                  hunk (path "a") (1+length s4) [] [pack "t"] :>:
                  NilFL
                 )))
            (SeqMS NilMS
               (mkNamed "cc39750d852a02487e2854be4c2b28f6cadf0957"
                 (
                  hunk (path "a") 1 (map pack (s4)) [] :>:
                  NilFL
                 ))))
          (mkNamed "3e2975afd211888617b20a58455926411f1ebaab"
            (
              hunk (path "a") (1+length s4) (map pack ([])) [pack "d"] :>:
              NilFL
            )))
        (SeqMS NilMS
          (mkNamed "441f412b978df5bd8febfe1f4baa5cb8d35f6e4c"
            (
             hunk (path "a") 1 (map pack []) [pack "U"] :>: 
             hunk (path "a") 2 (map pack s4) [] :>:
             NilFL
          )
        )
      )
    ))

example4guts :: forall p prim . (PrimConstruct prim, ModelOf p ~ V1Model, OnlyPrim p ~ prim) => Sealed2 (WithStartState2 (MergeableSequence p))
example4guts =
  let
    s3,s5,s7,s9,s10,s11,s12 :: IsString a => [a]
    s3 = ["A","F","l"]
    s5 = ["w"]
    s7 = ["y"]
    s9 = ["u"]
    s10 = ["x"]
    s11 = ["S"]
    s12 = ["e"]
    off :: [[a]] -> Int
    off xs = 1 + sum (map length xs)
  in
   Sealed2
     (WithStartState2
       (repo [("a", s3++s5++s7)])
       (
         (NilMS
           `SeqMS` hunk (path "a") (off [s3,s5]) [] (map pack s10)
           `SeqMS` hunk (path "a") (off [s3,s5,s10,s7]) [] (map pack s11)
         )
         `ParMS`
         (NilMS
            `SeqMS` hunk (path "a") (off []) (map pack s3) (map pack s9)
            `SeqMS` hunk (path "a") (off []) (map pack (s9++s5++s7)) []
         )
         `ParMS`
         (NilMS
           `SeqMS` hunk (path "a") (off []) (map pack (s3)) []
           `SeqMS` hunk (path "a") (off []) [] (map pack s12)
         )
       )
     )
         

example4
  :: forall p
   . (FromPrim p, PrimConstruct (OnlyPrim p), ModelOf p ~ V1Model)
   => Sealed2 (WithStartState2 (MergeableSequence (Named p)))
example4 =
  case example4guts @p of
   Sealed2 (WithStartState2 model (((NilMS `SeqMS` a1 `SeqMS` a2) `ParMS` (NilMS `SeqMS` a3 `SeqMS` a4)) `ParMS` (NilMS `SeqMS` a5 `SeqMS` a6))) ->
     Sealed2
       (WithStartState2 model
         ((NilMS
            `SeqMS`
            mkNamed "91b78b39ea6649b6e43ea74a57070480c87d7053"
                   (a1 :>: NilFL)
            `SeqMS`
            mkNamed "80da177d495a63bc9d80b8c9bc56045b23b629f7"
                   (a2 :>: NilFL)
          )
          `ParMS`
          (NilMS
            `SeqMS`
            mkNamed "cdd591a73493d39bd763bd71acac4c1e3078a4a"
                   (a3 :>: a4 :>: NilFL)
          )
          `ParMS`
          (NilMS
            `SeqMS`
            mkNamed "9b7b08b417d62868eb92d2cac7512b899c32f722"
               (a5 :>: a6 :>: NilFL)
          )
         ))

-- like Identity but with a MonadFail instance, just to make it easier
-- to write 'brokenV1Merge' below
newtype ErrorFail a = ErrorFail { runErrorFail :: a }

instance Functor ErrorFail where
  fmap f = ErrorFail . f . runErrorFail
instance Applicative ErrorFail where
  pure = ErrorFail
  liftA2 f (ErrorFail v1) (ErrorFail v2) = ErrorFail (f v1 v2)
instance Monad ErrorFail where
  ErrorFail v >>= f = f v
#if MIN_VERSION_base(4,12,0)
instance MonadFail ErrorFail where
#endif
  fail = error

-- For now this code isn't used, it just demonstrates how example4 is broken in V1
-- x4;x5' =\/= x5;x4' but not (effect (x4;x5') =\/= effect (x5;x4'))
brokenV1Merge :: forall prim . (OnlyPrim (RepoPatchV1 prim) ~ prim, ModelOf (RepoPatchV1 prim) ~ V1Model, PrimPatch prim) => ()
brokenV1Merge =
  let
    x4, x4', x4'', x5, x5', x6, x6' :: Sealed2 (RepoPatchV1 prim)
    (x4, x4', x4'', x5, x5', x6, x6') =
      case example4guts @(RepoPatchV1 prim) of
        Sealed2
          (WithStartState2 _
            ((NilMS `SeqMS` a1 `SeqMS` a2) `ParMS` (NilMS `SeqMS` a3 `SeqMS` a4) `ParMS` (NilMS `SeqMS` a5 `SeqMS` a6))
          )
         -> runErrorFail $ do
          (a3' :>: a4' :>: NilFL) :/\: _ <- return $ merge ((PP a1 :>: PP a2 :>: NilFL) :\/: (PP a3 :>: PP a4 :>: NilFL))
          (a5' :>: a6' :>: NilFL) :/\: _ <- return $ merge ((PP a1 :>: PP a2 :>: a3' :>: NilFL) :\/: (PP a5 :>: PP a6 :>: NilFL))
          a5'' :/\: a4'' <- return $ merge (a4' :\/: a5')
          a6'' :/\: a4''' <- return $ merge (a4'' :\/: a6')
          return (Sealed2 a4', Sealed2 a4'', Sealed2 a4''', Sealed2 a5', Sealed2 a5'', Sealed2 a6', Sealed2 a6'')
  in x4 `seq` x4' `seq` x4'' `seq` x5 `seq` x5' `seq` x6 `seq` x6' `seq` ()
