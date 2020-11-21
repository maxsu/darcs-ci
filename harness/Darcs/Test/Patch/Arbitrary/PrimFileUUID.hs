{-# LANGUAGE OverloadedStrings #-}
module Darcs.Test.Patch.Arbitrary.PrimFileUUID where

import Prelude ()
import Darcs.Prelude

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Shrink
import Darcs.Test.Patch.RepoModel

import Test.QuickCheck
import Darcs.Test.Patch.WithState
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Prim.FileUUID ()
import Darcs.Patch.Prim.FileUUID.Core ( Prim(..), Location(..), Hunk(..), UUID(..) )

import Darcs.Test.Patch.FileUUIDModel
import Darcs.Test.Util.QuickCheck ( notIn, maybeOf )

import qualified Data.ByteString as B
import Data.Maybe ( fromJust, isJust )
import qualified Data.Map as M
import Darcs.Util.Hash( Hash(..) )

type instance ModelOf Prim = FileUUIDModel
instance ArbitraryPrim Prim where
    runCoalesceTests = Nothing
    hasPrimConstruct = Nothing
    usesV1Model = Nothing

-- TODO add some useful shrinking, at least to
-- shrinkAtEnd/shrinkAtStart
instance Shrinkable Prim where
  shrinkInternally _ = []
  shrinkAtEnd _ = []
  shrinkAtStart _ = []

instance MightBeEmptyHunk Prim
instance MightHaveDuplicate Prim

instance NullPatch Prim where
  nullPatch Identity = IsEq
  nullPatch (Hunk _ (H _ old new))
    | old == new = unsafeCoerceP IsEq
  nullPatch _ = NotEq

instance PropagateShrink Prim Prim where
  propagateShrink = propagatePrim

instance ShrinkModel Prim where
  -- no shrinking for now
  shrinkModelPatch _ = []

----------------------------------------------------------------------
-- * QuickCheck generators

aHunk :: B.ByteString -> Gen (Hunk wX wY)
aHunk content = do
  pos <- choose (0, B.length content)
  oldLen <- choose (0, B.length content - pos)
  new <- scale (`div` 8) aContent
  let old = B.take oldLen $ B.drop pos $ content
  return $ H pos old new

aTextHunk :: (UUID, Object Fail) -> Gen (Prim wX wY)
aTextHunk (uuid, (Blob text _)) =
  do h <- aHunk (unFail text)
     return $ Hunk uuid h
aTextHunk _ = error "impossible case"

aManifest :: UUID -> (UUID, Object Fail) -> Gen (Prim wX wY)
aManifest uuid (dirId, Directory dir) =
  do filename <- aFilename `notIn` (M.keys dir)
     return $ Manifest uuid (L dirId filename)
aManifest _ _ = error "impossible case"

aDemanifest :: UUID -> Location -> Gen (Prim wX wY)
aDemanifest uuid loc = return $ Demanifest uuid loc

-- | Generates any type of 'Prim' patch, except binary and setpref patches.
aPrim :: FileUUIDModel wX -> Gen (WithEndState FileUUIDModel (Prim wX) wY)
aPrim repo
  = do mbFile <- maybeOf repoFiles -- some file, not necessarily manifested
       dir <- elements repoDirs -- some directory, not necessarily manifested
       -- note, the root directory always exists and is never manifested nor demanifested
       mbDemanifested <- maybeOf notManifested -- something not manifested
       mbManifested <- maybeOf manifested -- something manifested
       fresh <- anUUID `notIn` repoIds repo -- a fresh uuid
       let whenjust m x = if isJust m then x else 0
           whenfile = whenjust mbFile
           whendemanifested = whenjust mbDemanifested
           whenmanifested = whenjust mbManifested
       patch <- frequency
                  [ ( whenfile 12, aTextHunk $ fromJust mbFile ) -- edit an existing file
                  , ( 2, aTextHunk (fresh, Blob (return "") NoHash) ) -- edit a new file
                  , ( whendemanifested 2 -- manifest an existing object
                    , aManifest (fromJust mbDemanifested) dir
                    )
                  , ( whenmanifested 2
                    , uncurry aDemanifest $ fromJust mbManifested
                    )
                  ]
       let repo' = unFail $ repoApply repo patch
       return $ WithEndState patch repo'
  where
      manifested = [ (uuid, (L dirid name)) | (dirid, Directory dir) <- repoDirs
                                          , (name, uuid) <- M.toList dir ]
      notManifested = [ uuid | (uuid, _) <- nonRootObjects
                           , not (uuid `elem` map fst manifested) ]
      repoFiles = [ (uuid, Blob x y) | (uuid, Blob x y) <- repoObjects repo ]
      repoDirs  = [ (uuid, Directory x) | (uuid, Directory x) <- repoObjects repo ]
      nonRootObjects = filter notRoot $ repoObjects repo where
        notRoot (uuid, _) = uuid == rootId

----------------------------------------------------------------------
-- *** Pairs of primitive patches

-- Try to generate commutable pairs of hunks
hunkPair :: (UUID, Object Fail) -> Gen ((Prim :> Prim) wX wY)
hunkPair (uuid, (Blob file _)) =
  do h1@(H off1 old1 new1) <- aHunk (unFail file)
     (delta, content') <- selectChunk h1 (unFail file)
     H off2' old2 new2 <- aHunk content'
     let off2 = off2' + delta
     return (Hunk uuid (H off1 old1 new1) :> Hunk uuid (H off2 old2 new2))
  where
     selectChunk (H off old new) content = elements [prefix, suffix]
       where prefix = (0, B.take off content)
             suffix = (off + B.length new, B.drop (off + B.length old) content)
hunkPair _ = error "impossible case"

aPrimPair :: FileUUIDModel wX
          -> Gen (WithEndState FileUUIDModel ((Prim :> Prim) wX) wY)
aPrimPair repo
  = do mbFile <- maybeOf repoFiles
       frequency
          [ ( if isJust mbFile then 1 else 0
            , do p1 :> p2 <- hunkPair $ fromJust mbFile
                 let repo'  = unFail $ repoApply repo  p1
                     repo'' = unFail $ repoApply repo' p2
                 return $ WithEndState (p1 :> p2) repo''
            )
          , ( 1
            , do Sealed wesP <- arbitraryState repo
                 return $ unsafeCoerceP1 wesP
            )
          ]
  where
      repoFiles = [ (uuid, Blob x y) | (uuid, Blob x y) <- repoObjects repo ]

----------------------------------------------------------------------
-- Arbitrary instances

instance ArbitraryState Prim where
  arbitraryState s = seal <$> aPrim s


-- use the special generator for pairs
arbitraryPair :: Gen (Sealed2 (WithState (Prim :> Prim)))
arbitraryPair = do
  repo <- aSmallRepo
  WithEndState pp repo' <- aPrimPair repo
  return $ seal2 $ WithState repo pp repo'

instance Arbitrary (Sealed2 Prim) where
  arbitrary = makeS2Gen aSmallRepo

instance Arbitrary (Sealed2 (Prim :> Prim)) where
  arbitrary = mapSeal2 wsPatch <$> arbitraryPair

instance Arbitrary (Sealed2 (WithState Prim)) where
  arbitrary = makeWS2Gen aSmallRepo

instance Arbitrary (Sealed2 (WithState (Prim :> Prim))) where
  arbitrary = arbitraryPair
