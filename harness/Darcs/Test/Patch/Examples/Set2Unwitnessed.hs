-- Copyright (C) 2007 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}
module Darcs.Test.Patch.Examples.Set2Unwitnessed
       ( primPermutables, primPatches
       , commutables, commutablesFL
       , repov2Commutables , repov2Mergeables, repov2Triples
       , repov2NonduplicateTriples, repov2Patches, repov2PatchLoopExamples
       ) where

import Darcs.Prelude

import Data.Maybe ( catMaybes )
import qualified Data.ByteString.Char8 as BC ( pack )
import Data.String ( IsString(..) )

import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch ( invert, hunk )
import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.FromPrim ( fromAnonymousPrim )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.V2 ( RepoPatchV2 )
-- import Darcs.Test.Patch.Test () -- for instance Eq Patch
-- import Darcs.Test.Patch.Examples.Set2Unwitnessed
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import qualified Darcs.Test.Patch.Arbitrary.Generic as W ( notDuplicatestriple )
import Darcs.Test.Patch.Arbitrary.RepoPatchV2 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
--import Darcs.Util.Printer ( greenText )
--import Darcs.Util.Printer.Color ( traceDoc )
--import Darcs.Util.Printer.Color ( errorDoc )
import Darcs.Util.Printer.Color () -- for instance Show Doc
import Darcs.Test.Patch.WSub


import qualified Darcs.Patch.Witnesses.Ordered as W ( (:>), (:\/:) )
import qualified Data.ByteString as B ( ByteString )
import Darcs.Test.Patch.V1Model ( V1Model, Content
                                , makeRepo, makeFile)
import Darcs.Test.Patch.WithState ( WithStartState(..) )
import Darcs.Util.Path ( AnchoredPath, floatPath, makeName )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), FromPrim )
import Darcs.Patch.Merge ( Merge )
import Darcs.Test.Patch.Arbitrary.PatchTree
    ( Tree(..)
    , TreeWithFlattenPos(..)
    , commutePairFromTree, commuteTripleFromTree
    , mergePairFromCommutePair, commutePairFromTWFP
    , canonizeTree
    )

instance IsString AnchoredPath where
  fromString = floatPath


-- import Debug.Trace

type Patch = RepoPatchV2 Prim2

makeSimpleRepo :: String -> Content -> V1Model wX
makeSimpleRepo filename content =
    makeRepo [(either error id $ makeName filename, makeFile content)]

withStartState :: s wX -> p wX -> Sealed (WithStartState s p)
withStartState s p = seal (WithStartState s p)

w_tripleExamples :: (FromPrim p, Merge p, PrimPatchBase p)
                 => [Sealed2 (p W.:> p W.:> p)]
w_tripleExamples = catMaybes [commuteTripleFromTree seal2 $
                   withStartState (makeSimpleRepo "file" [])
                   (ParTree
                    (SeqTree (hunk "file" 1 [] [BC.pack "g"])
                     (SeqTree (hunk "file" 2 [] [BC.pack "j"])
                      (SeqTree (hunk "file" 1 [] [BC.pack "s"]) NilTree)))
                    (SeqTree (hunk "file" 1 [] [BC.pack "e"]) NilTree))
                  ,commuteTripleFromTree seal2 $
                   withStartState (makeSimpleRepo "file" [BC.pack "j"])
                   (ParTree
                    (SeqTree (hunk "file" 1 [] [BC.pack "s"])
                     (ParTree
                      (SeqTree (hunk "file" 2 [BC.pack "j"] []) NilTree)
                      (SeqTree (hunk "file" 2 [BC.pack "j"] []) NilTree)))
                    (SeqTree (hunk "file" 1 [BC.pack "j"] []) NilTree))
                  ]


w_mergeExamples :: (FromPrim p, Commute p, Merge p, PrimPatchBase p)
                => [Sealed2 (p W.:\/: p)]
w_mergeExamples = map (unseal2 (mergePairFromCommutePair seal2)) w_commuteExamples

w_commuteExamples :: (FromPrim p, Merge p, PrimPatchBase p) => [Sealed2 (p W.:> p)]
w_commuteExamples = catMaybes [
                   commutePairFromTWFP seal2 $
                   withStartState (makeSimpleRepo "file" [])
                   (TWFP 3
                    (ParTree
                     (SeqTree (hunk "file" 1 [] [BC.pack "h"]) NilTree)
                     (SeqTree (hunk "file" 1 [] [BC.pack "b"])
                       (SeqTree (hunk "file" 1 [] [BC.pack "f"])
                         (SeqTree (hunk "file" 1 [] [BC.pack "v"])
                           (SeqTree (hunk "file" 2 [BC.pack "f"] []) NilTree)))))),
                   commutePairFromTWFP seal2 $
                   withStartState
                   (makeSimpleRepo "file" [BC.pack "f",BC.pack "s",BC.pack "d"])
                   (TWFP 3
                    (ParTree
                     (SeqTree (hunk "file" 3 [BC.pack "d"] []) NilTree)
                     (ParTree
                      (SeqTree (hunk "file" 1 [BC.pack "f"] []) NilTree)
                      (SeqTree (hunk "file" 1 [BC.pack "f"] [])
                        (SeqTree (hunk "file" 1 [BC.pack "s",BC.pack "d"] [])
                          (SeqTree (hunk "file" 1 [] [BC.pack "v"]) NilTree)))))),
{-                   commutePairFromTWFP seal2 $
                   withStartState
                   (makeSimpleRepo "file" [BC.pack "f",BC.pack "u",
                                            BC.pack "s",BC.pack "d"])
                   (TWFP 5
                    (ParTree
                     (SeqTree (hunk "file" 5 [] [BC.pack "x"])
                      (SeqTree (hunk "file" 4 [BC.pack "d"] []) NilTree))
                     (ParTree
                      (SeqTree (hunk "file" 1 [BC.pack "f",BC.pack "u"] []) NilTree)
                      (SeqTree (hunk "file" 1 [BC.pack "f"] [])
                       (SeqTree (hunk "file" 1 [BC.pack "u",BC.pack "s",BC.pack "d"] [])
                        (SeqTree (hunk "file" 1 [] [BC.pack "a"])
                         (SeqTree (hunk "file" 1 [BC.pack "a"] []) NilTree))))))),-}
                   commutePairFromTree seal2 $
                   withStartState (makeSimpleRepo "file" [BC.pack "n",BC.pack "t",BC.pack "h"])
                   (ParTree
                    (SeqTree (hunk "file" 1 [BC.pack "n",BC.pack "t",BC.pack "h"] [])
                     NilTree)
                    (SeqTree (hunk "file" 3 [BC.pack "h"] [])
                     (SeqTree (hunk "file" 1 [BC.pack "n"] [])
                      (SeqTree (hunk "file" 1 [BC.pack "t"] []) NilTree)))),
                  commutePairFromTree seal2 $
                  withStartState (makeSimpleRepo "file" [])
                  (ParTree
                   (SeqTree (hunk "file" 1 [] [BC.pack "n"]) NilTree)
                   (SeqTree (hunk "file" 1 [] [BC.pack "i"])
                                (SeqTree (hunk "file" 1 [] [BC.pack "i"]) NilTree))),
                  commutePairFromTree seal2 $
                  withStartState (makeSimpleRepo "file" [])
                  (ParTree
                   (SeqTree (hunk "file" 1 [] [BC.pack "c"])
                     (ParTree
                       (SeqTree (hunk "file" 1 [BC.pack "c"] [BC.pack "r"]) NilTree)
                       (SeqTree (hunk "file" 1 [] [BC.pack "h"])
                        (SeqTree (hunk "file" 1 [] [BC.pack "d"]) NilTree))))
                   (SeqTree (hunk "file" 1 [] [BC.pack "f"]) NilTree)),
                  commutePairFromTWFP seal2 $
                  withStartState (makeSimpleRepo "file" [])
                  (TWFP 1
                  (ParTree
                   (ParTree
                    (SeqTree (hunk "file" 1 [] [BC.pack "t"]) NilTree)
                    (SeqTree (hunk "file" 1 [] [BC.pack "t"]) NilTree))
                   (SeqTree (hunk "file" 1 [] [BC.pack "f"]) NilTree))),
                   commutePairFromTWFP seal2 $
                   withStartState (makeSimpleRepo "file" [BC.pack "f",BC.pack " r",
                                                            BC.pack "c",BC.pack "v"])
                   (TWFP 4
                    (ParTree
                     (SeqTree (hunk "file" 3 [BC.pack "c",BC.pack "v"] [])
                        (ParTree
                         (SeqTree (hunk "file" 2 [BC.pack "r"] [])
                          (SeqTree (hunk "fi le" 1 [BC.pack "f"] []) NilTree))
                         (SeqTree (hunk "file" 1 [BC.pack "f",BC.pack "r"] [])
                          (SeqTree (hunk "file" 1 [] [BC.pack "y"]) NilTree))))
                     (SeqTree (hunk "file" 4 [BC.pack "v"] []) NilTree))),
                   commutePairFromTree seal2 $
                   withStartState (makeSimpleRepo "file" [])
                   (ParTree
                    (SeqTree (hunk "file" 1 [] [BC.pack "z"]) NilTree)
                    (ParTree
                     (SeqTree (hunk "file" 1 [] [BC.pack "f"]) NilTree)
                     (ParTree
                      (SeqTree (hunk "file" 1 [] [BC.pack "r"]) NilTree)
                      (SeqTree (hunk "file" 1 [] [BC.pack "d"]) NilTree))))
                 , commutePairFromTree seal2 $
                   withStartState (makeSimpleRepo "file" [BC.pack "t",BC.pack "r",BC.pack "h"])
                   (ParTree
                    (ParTree
                     (SeqTree (hunk "file" 1 [BC.pack "t",BC.pack "r",BC.pack "h"] [])
                              NilTree)
                     (SeqTree (hunk "file" 1 [] [BC.pack "o"]) NilTree))
                    (SeqTree (hunk "file" 1 [BC.pack "t"] [])
                     (SeqTree (hunk "file" 2 [BC.pack "h"] []) NilTree)))
                 , commutePairFromTWFP seal2 $
                   withStartState (makeSimpleRepo "file" []) $
                   TWFP 2
                   (ParTree
                    (SeqTree (hunk "file" 1 [] [BC.pack "h"]) NilTree)
                    (SeqTree (hunk "file" 1 [] [BC.pack "y"])
                     (SeqTree (hunk "file" 2 [] [BC.pack "m"])
                      (SeqTree (hunk "file" 1 [] [BC.pack "v"]) NilTree))))
                 , commutePairFromTree seal2 $
                 withStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (hunk "file" 1 [] [BC.pack "p"])
                   (SeqTree (hunk "file" 1 [BC.pack "p"] [])
                    (SeqTree (hunk "file" 1 [] [BC.pack "c"]) NilTree)))
                  (SeqTree (hunk "file" 1 [] [BC.pack "z"]) NilTree))
                 , commutePairFromTree seal2 $
                 withStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (hunk "file" 1 [] [BC.pack "j" ])
                   (SeqTree (hunk "file" 1 [BC.pack "j"] []) NilTree))
                  (SeqTree (hunk "file" 1 [] [BC.pack "v"]) NilTree))
                 , commutePairFromTree seal2 $
                 withStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (hunk "file" 1 [] [BC.pack "v"]) NilTree)
                  (SeqTree (hunk "file" 1 [] [BC.pack "j" ])
                   (SeqTree (hunk "file" 1 [BC.pack "j"] []) NilTree)))
                 , commutePairFromTree seal2 $
                 withStartState (makeSimpleRepo "file" [BC.pack "x",BC.pack "c"])
                 (ParTree
                  (SeqTree (hunk "file" 1 [] [BC.pack "h"])
                   (ParTree
                    (SeqTree (hunk "file" 3 [BC.pack "c"] []) NilTree)
                    (SeqTree (hunk "file" 2 [BC.pack "x"] [])
                     (SeqTree (hunk "file" 1 [] [BC.pack "j"]) NilTree))))
                  (SeqTree (hunk "file" 1 [] [BC.pack "l"]) NilTree))
                 , commutePairFromTree seal2 $
                 withStartState (makeSimpleRepo "file" [])
                 (ParTree
                  (SeqTree (hunk "file" 1 [] (packStringLetters "s")) NilTree)
                  (SeqTree (hunk "file" 1 [] (packStringLetters "k"))
                   (SeqTree (hunk "file" 1 (packStringLetters "k") [])
                    (SeqTree (hunk "file" 1 [] (packStringLetters "m"))
                     (SeqTree (hunk "file" 1 (packStringLetters "m") []) NilTree)))))
                 ]

packStringLetters :: String -> [B.ByteString]
packStringLetters = map (BC.pack . (:[]))

w_repov2PatchLoopExamples :: [Sealed (WithStartState V1Model (Tree Prim2))]
w_repov2PatchLoopExamples =
    [Sealed (WithStartState (makeSimpleRepo fx [])
     $ canonizeTree
     (ParTree
      (SeqTree (hunk fx 1 [] (packStringLetters "pkotufogbvdabnmbzajvolwviqebieonxvcvuvigkfgybmqhzuaaurjspd"))
       (ParTree
        (SeqTree (hunk fx 47 (packStringLetters "qhzu") (packStringLetters "zafybdcokyjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmh"))
         (ParTree
          (ParTree
           NilTree
           (ParTree
            (ParTree
             (ParTree
              (SeqTree (hunk fx 15 (packStringLetters "mbzajvolwviqebieonxvcvuvigkfgyb") (packStringLetters "vujnxnhvybvpouyciaabszfmgssezlwwjgnethvrpnfrkubphzvdgymjjoacppqps"))
               (ParTree
                NilTree
                (ParTree
                 (SeqTree (hunk fx 40 (packStringLetters "ssezlwwjgnethvrpnfrkubphzvdgymjjoacppqpsmzafybdcokyjskcgnvhkbz") (packStringLetters "wnesidpccwoiqiichxaaejdsyrhrusqljlcoro"))
                  (ParTree
                   (ParTree
                    (SeqTree (hunk fx 12 (packStringLetters "abnvujnxnhvybvpouyciaabszfmgwnesidpccwoiqii") (packStringLetters "czfdhqkipdstfjycqaxwnbxrihrufdeyneqiiiafwzlmg")) NilTree)
                    NilTree)
                   NilTree))
                 (SeqTree (hunk fx 25 [] (packStringLetters "dihgmsotezucqdgxczvcivijootyvhlwymbiueufnvpwpeukmskqllalfe")) NilTree))))
              (SeqTree (hunk fx 56 (packStringLetters "yjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmhaaurjsp") (packStringLetters "xldhrutyhcyaqeezwujiguawfyawjjqlirxshjddvq")) NilTree))
             (SeqTree (hunk fx 20 [] (packStringLetters "ooygwiyogqrqnytixqtmvdxx"))
              (SeqTree (hunk fx 26 (packStringLetters "yogqrqnytixqtmvdxxvolwviqebieonxvcvuvigkfgybmzafybdcokyjskcgnvhkbz") (packStringLetters "akhsmlbkdxnvfoikmiatfbpzdrsyykkpoxvvddeaspzxe"))
               (SeqTree (hunk fx 39 [] (packStringLetters "ji"))
                (ParTree
                 NilTree
                 (ParTree
                  NilTree
                  (ParTree
                   (ParTree
                    NilTree
                    (SeqTree (hunk fx 26 (packStringLetters "akhsmlbkdxnvfjioikmiatfbpzdrsyykkpoxvvddeaspzxepysaafnjjhcstgrczplxs") (packStringLetters "onjbhddskcj"))
                     (SeqTree (hunk fx 39 [] (packStringLetters "fyscunxxxjjtyqpfxeznhtwvlphmp")) NilTree)))
                   (ParTree
                    NilTree
                    (SeqTree (hunk fx 44 [] (packStringLetters "xcchzwmzoezxkmkhcmesplnjpqriypshgiqklgdnbmmkldnydiy"))
                     (ParTree
                      NilTree
                      (SeqTree (hunk fx 64 (packStringLetters "plnjpqriypshgiqklgdnbmmkldnydiymiatfbpzdrsyykkpoxvvddeaspzxepysaafn") (packStringLetters "anjlzfdqbjqbcplvqvkhwjtkigp")) NilTree)))))))))))
            (ParTree
             NilTree
             NilTree)))
          NilTree))
        NilTree))
      (ParTree
       NilTree
       (SeqTree (hunk fx 1 [] (packStringLetters "ti"))
        (SeqTree (hunk fx 1 (packStringLetters "t") (packStringLetters "ybcop"))
         (SeqTree (hunk fx 2 [] (packStringLetters "dvlhgwqlpaeweerqrhnjtfolczbqbzoccnvdsyqiefqitrqneralf"))
          (SeqTree (hunk fx 15 [] (packStringLetters "yairbjphwtnaerccdlfewujvjvmjakbc"))
           (SeqTree (hunk fx 51 [] (packStringLetters "xayvfuwaiiogginufnhsrmktpmlbvxiakjwllddkiyofyfw"))
            (ParTree
             NilTree
             NilTree)))))))))]
  where
      fx :: IsString a => a
      fx = "F"

mergeExamples :: [Sealed2 (Patch :\/: Patch)]
mergeExamples = map (mapSeal2 fromW) w_mergeExamples

repov2PatchLoopExamples :: [Sealed (WithStartState V1Model (Tree Prim2))]
repov2PatchLoopExamples = w_repov2PatchLoopExamples

commuteExamples :: [Sealed2 (Patch :> Patch)]
commuteExamples = map (mapSeal2 fromW) w_commuteExamples

tripleExamples :: [Sealed2 (Patch :> Patch :> Patch)]
tripleExamples = map (mapSeal2 fromW) w_tripleExamples

notDuplicatestriple :: (Patch :> Patch :> Patch) wX wY -> Bool
notDuplicatestriple = W.notDuplicatestriple . toW

quickhunk :: PrimPatch prim => Int -> String -> String -> prim wX wY
quickhunk l o n = hunk "test" l (map (\c -> BC.pack [c]) o)
                                (map (\c -> BC.pack [c]) n)

primPermutables :: [(Prim2 :> Prim2 :> Prim2) wX wY]
primPermutables =
    [quickhunk 0 "e" "bo" :> quickhunk 3 "" "x" :> quickhunk 2 "f" "qljo"]

mergeables :: [(Prim2 :\/: Prim2) wX wY]
mergeables = [quickhunk 1 "a" "b" :\/: quickhunk 1 "a" "c",
              quickhunk 1 "a" "b" :\/: quickhunk 3 "z" "c",
              quickhunk 0 "" "a" :\/: quickhunk 1 "" "b",
              quickhunk 0 "a" "" :\/: quickhunk 1 "" "b",
              quickhunk 0 "a" "" :\/: quickhunk 1 "b" "",
              quickhunk 0 "" "a" :\/: quickhunk 1 "b" ""
             ]

mergeablesFL :: [(FL Prim2 :\/: FL Prim2) wX wY]
mergeablesFL = map (\ (x:\/:y) -> (x :>: NilFL) :\/: (y :>: NilFL)) mergeables ++
           [] --    [(quickhunk 1 "a" "b" :>: quickhunk 3 "z" "c" :>: NilFL)
              --  :\/: (quickhunk 1 "a" "z" :>: NilFL),
              --  (quickhunk 1 "a" "b" :>: quickhunk 1 "b" "c" :>: NilFL)
              --  :\/: (quickhunk 1 "a" "z" :>: NilFL)]

mergeable2commutable :: Invert p => (p :\/: p) wX wY -> (p :> p) wX wY
mergeable2commutable (x :\/: y) = unsafeCoerceP (invert x) :> y

commutablesFL :: [(FL Prim2 :> FL Prim2) wX wY]
commutablesFL = map mergeable2commutable mergeablesFL
commutables :: [(Prim2 :> Prim2) wX wY]
commutables = map mergeable2commutable mergeables

primPatches :: [Prim2 wX wY]
primPatches = concatMap mergeable2patches mergeables
    where mergeable2patches (x:\/:y) = [x,y]

repov2Patches :: [Patch wX wY]
repov2Patches = concatMap commutable2patches repov2Commutables
    where commutable2patches (x:>y) = [x,y]

repov2Triples :: [(Patch :> Patch :> Patch) wX wY]
repov2Triples = [ob' :> oa2 :> a2'',
                 oa' :> oa2 :> a2'']
               ++ map (unseal2 unsafeCoerceP) tripleExamples
               ++ map (unseal2 unsafeCoerceP) (concatMap getTriples repov2FLs)
    where oa = fromAnonymousPrim $ quickhunk 1 "o" "aa"
          oa2 = oa
          a2 = fromAnonymousPrim $ quickhunk 2 "a34" "2xx"
          ob = fromAnonymousPrim $ quickhunk 1 "o" "bb"
          ob' :/\: oa' = merge (oa :\/: ob)
          a2' :/\: _ = merge (ob' :\/: a2)
          a2'' :/\: _ = merge (oa2 :\/: a2')

repov2NonduplicateTriples :: [(Patch :> Patch :> Patch) wX wY]
repov2NonduplicateTriples = filter (notDuplicatestriple) repov2Triples

repov2FLs :: [FL (Patch) wX wY]
repov2FLs = [oa :>: invert oa :>: oa :>: invert oa :>: ps +>+ oa :>: invert oa :>: NilFL]
    where oa = fromAnonymousPrim $ quickhunk 1 "o" "a"
          ps :/\: _ = merge (oa :>: invert oa :>: NilFL :\/: oa :>: invert oa :>: NilFL)

repov2Commutables :: [(Patch :> Patch) wX wY]
repov2Commutables = map (unseal2 unsafeCoerceP) commuteExamples++
                     map mergeable2commutable repov2Mergeables++
                     [invert oa :> ob'] ++ map (unseal2 unsafeCoerceP) (concatMap getPairs repov2FLs)
    where oa = fromAnonymousPrim $ quickhunk 1 "o" "a"
          ob = fromAnonymousPrim $ quickhunk 1 "o" "b"
          _ :/\: ob' = mergeFL (ob :\/: oa :>: invert oa :>: NilFL)

repov2Mergeables :: [(Patch :\/: Patch) wX wY]
repov2Mergeables = map (\ (x :\/: y) -> fromAnonymousPrim x :\/: fromAnonymousPrim y) mergeables
                        ++ repov2IglooMergeables
                        ++ repov2QuickcheckMergeables
                        ++ map (unseal2 unsafeCoerceP) mergeExamples
                        ++ catMaybes (map pair2m (concatMap getPairs repov2FLs))
                        ++ [(oa :\/: od),
                            (oa :\/: a2'),
                            (ob' :\/: od''),
                            (oe :\/: od),
                            (of' :\/: oe'),
                            (ob' :\/: oe'),
                            (oa :\/: oe'),
                            (ob' :\/: oc'),
                            (b2' :\/: oc'''),
                            (ob' :\/: a2),
                            (b2' :\/: og'''),
                            (oc''' :\/: og'''),
                            (oc'' :\/: og''),
                            (ob'' :\/: og''),
                            (ob'' :\/: oc''),
                            (oc' :\/: od'')]
    where oa = fromAnonymousPrim $ quickhunk 1 "o" "aa"
          a2 = fromAnonymousPrim $ quickhunk 2 "a34" "2xx"
          og = fromAnonymousPrim $ quickhunk 3 "4" "g"
          ob = fromAnonymousPrim $ quickhunk 1 "o" "bb"
          b2 = fromAnonymousPrim $ quickhunk 2 "b" "2"
          oc = fromAnonymousPrim $ quickhunk 1 "o" "cc"
          od = fromAnonymousPrim $ quickhunk 7 "x" "d"
          oe = fromAnonymousPrim $ quickhunk 7 "x" "e"
          pf = fromAnonymousPrim $ quickhunk 7 "x" "f"
          od'' = fromAnonymousPrim $ quickhunk 8 "x" "d"
          ob' :>: b2' :>: NilFL :/\: _ = mergeFL (oa :\/: ob :>: b2 :>: NilFL)
          a2' :/\: _ = merge (ob' :\/: a2)
          ob'' :/\: _ = merge (a2 :\/: ob')
          og' :/\: _ = merge (oa :\/: og)
          og'' :/\: _ = merge (a2 :\/: og')
          og''' :/\: _ = merge (ob' :\/: og')
          oc' :/\: _ = merge (oa :\/: oc)
          oc'' :/\: _ = merge (a2 :\/: oc)
          oc''' :/\: _ = merge (ob' :\/: oc')
          oe' :/\: _ = merge (od :\/: oe)
          of' :/\: _ = merge (od :\/: pf)
          pair2m :: Sealed2 (Patch :> Patch)
                 -> Maybe ((Patch :\/: Patch) wX wY)
          pair2m (Sealed2 (xx :> y)) = do y' :> _ <- commute (xx :> y)
                                          return $ unsafeCoerceP (xx :\/: y')

repov2IglooMergeables :: [(Patch :\/: Patch) wX wY]
repov2IglooMergeables = [(a :\/: b),
                    (b :\/: c),
                    (a :\/: c),
                    (x :\/: a),
                    (y :\/: b),
                    (z :\/: c),
                    (x' :\/: y'),
                    (z' :\/: y'),
                    (x' :\/: z'),
                    (a :\/: a)]
    where a = fromAnonymousPrim $ quickhunk 1 "1" "A"
          b = fromAnonymousPrim $ quickhunk 2 "2" "B"
          c = fromAnonymousPrim $ quickhunk 3 "3" "C"
          x = fromAnonymousPrim $ quickhunk 1 "1BC" "xbc"
          y = fromAnonymousPrim $ quickhunk 1 "A2C" "ayc"
          z = fromAnonymousPrim $ quickhunk 1 "AB3" "abz"
          x' :/\: _ = merge (a :\/: x)
          y' :/\: _ = merge (b :\/: y)
          z' :/\: _ = merge (c :\/: z)

repov2QuickcheckMergeables :: [(Patch :\/: Patch) wX wY]
repov2QuickcheckMergeables = [-- invert k1 :\/: n1
                             --, invert k2 :\/: n2
                               hb :\/: k
                             , b' :\/: b'
                             , n' :\/: n'
                             , b :\/: d
                             , k' :\/: k'
                             , k3 :\/: k3
                             ] ++ catMaybes (map pair2m pairs)
    where hb = fromAnonymousPrim $ quickhunk 0 "" "hb"
          k = fromAnonymousPrim $ quickhunk 0 "" "k"
          n = fromAnonymousPrim $ quickhunk 0 "" "n"
          b = fromAnonymousPrim $ quickhunk 1 "b" ""
          d = fromAnonymousPrim $ quickhunk 2 "" "d"
          d':/\:_ = merge (b :\/: d)
          --k1 :>: n1 :>: NilFL :/\: _ = mergeFL (hb :\/: k :>: n :>: NilFL)
          --k2 :>: n2 :>: NilFL :/\: _ =
          --    merge (hb :>: b :>: NilFL :\/: k :>: n :>: NilFL)
          k' :>: n' :>: NilFL :/\: _ :>: b' :>: _ = merge (hb :>: b :>: d' :>: NilFL :\/: k :>: n :>: NilFL)
          pairs = getPairs (hb :>: b :>: d' :>: k' :>: n' :>: NilFL)
          pair2m :: Sealed2 (Patch :> Patch)
                 -> Maybe ((Patch :\/: Patch) wX wY)
          pair2m (Sealed2 (xx :> y)) = do y' :> _ <- commute (xx :> y)
                                          return $ unsafeCoerceP (xx :\/: y')

          i = fromAnonymousPrim $ quickhunk 0 "" "i"
          x = fromAnonymousPrim $ quickhunk 0 "" "x"
          xi = fromAnonymousPrim $ quickhunk 0 "xi" ""
          d3 :/\: _ = merge (xi :\/: d)
          _ :/\: k3 = mergeFL (k :\/: i :>: x :>: xi :>: d3 :>: NilFL)

