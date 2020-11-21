module Darcs.Patch.Rebase.PushFixup
  ( PushFixupFn, dropFixups
  , pushFixupFLFL_FLFLFL
  , pushFixupFLFL_FLFLFLFL
  , pushFixupFLMB_FLFLMB
  , pushFixupIdFL_FLFLFL
  , pushFixupIdMB_FLFLMB
  , pushFixupIdMB_FLIdFLFL
  ) where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Maybe ( Maybe2(..) )
import Darcs.Patch.Witnesses.Ordered
  ( (:>)(..), FL(..), (+>+)
  )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )

-- | During a rebase, we use "fixup" patches to maintain the correct
-- context for the real "items" that are being stored in the rebase
-- that the user wants to keep. As the context of the rebase changes,
-- new fixups get added to the beginning that then need to be pushed
-- past as many items as possible.
--
-- There are multiple fixup types and multiple ways of representing
-- the items being stored in the rebase, so this is polymorphic in
-- both types. Also, the structure of the results varies - in some
-- cases it will be a single value, sometimes an FL, or sometimes
-- zero or one values (Maybe2), so the output types are separate
-- variables. A typical instantiation would be something like
-- PushFixupFn Fixup Item (FL Item) (FL Fixup).
type PushFixupFn fixupIn itemIn itemOut fixupOut
  =  forall wX wY
  .  (fixupIn :> itemIn  ) wX wY
  -> (itemOut :> fixupOut) wX wY

dropFixups :: (item :> fixup) wX wY -> Sealed (item wX)
dropFixups (item :> _) = Sealed item

{-
The collection below of pushFixup combinators is quite annoying, but
there's no obvious generalisation, and inlining them at each use site
would be even worse.
-}

pushFixupFLFL_FLFLFL
  :: PushFixupFn fixup     item  (FL item) (FL fixup)
  -> PushFixupFn fixup (FL item) (FL item) (FL fixup)
pushFixupFLFL_FLFLFL _pushOne (fixup :> NilFL)
  = NilFL :> (fixup :>: NilFL)
pushFixupFLFL_FLFLFL pushOne (fixup :> (item1 :>: items2))
  = case pushOne (fixup :> item1) of
      items1' :> fixups' ->
        case pushFixupFLFL_FLFLFLFL pushOne (fixups' :> items2) of
          items2' :> fixups'' -> (items1' +>+ items2') :> fixups''

pushFixupFLFL_FLFLFLFL
  :: PushFixupFn     fixup      item  (FL item) (FL fixup)
  -> PushFixupFn (FL fixup) (FL item) (FL item) (FL fixup)
pushFixupFLFL_FLFLFLFL _pushOne (NilFL :> items)
  = items :> NilFL
pushFixupFLFL_FLFLFLFL pushOne ((fixup1 :>: fixups2) :> items)
  = case pushFixupFLFL_FLFLFLFL pushOne (fixups2 :> items) of
      items' :> fixups2' ->
        case pushFixupFLFL_FLFLFL pushOne (fixup1 :> items') of
          items'' :> fixups1' -> items'' :> (fixups1' +>+ fixups2')

pushFixupFLMB_FLFLMB
  :: PushFixupFn fixup     item  (FL item) (Maybe2 fixup)
  -> PushFixupFn fixup (FL item) (FL item) (Maybe2 fixup)
pushFixupFLMB_FLFLMB _pushOne (fixup :> NilFL)
  = NilFL :> Just2 fixup
pushFixupFLMB_FLFLMB pushOne (fixup :> (item1 :>: items2))
  = case pushOne (fixup :> item1) of
      items1' :> Nothing2 -> items1' +>+ items2 :> Nothing2
      items1' :> Just2 fixup' ->
        case pushFixupFLMB_FLFLMB pushOne (fixup' :> items2) of
          items2' :> fixup'' -> items1' +>+ items2' :> fixup''

pushFixupIdFL_FLFLFL
  :: PushFixupFn fixup     item      item  (FL fixup)
  -> PushFixupFn fixup (FL item) (FL item) (FL fixup)
pushFixupIdFL_FLFLFL pushOne
  = pushFixupFLFL_FLFLFL (mkList . pushOne)
  where
    mkList :: (item :> FL fixup) wX wY -> (FL item :> FL fixup) wX wY
    mkList (item :> fixups) = (item :>: NilFL) :> fixups

pushFixupIdMB_FLFLMB
  :: PushFixupFn fixup     item      item  (Maybe2 fixup)
  -> PushFixupFn fixup (FL item) (FL item) (Maybe2 fixup)
pushFixupIdMB_FLFLMB pushOne
  = pushFixupFLMB_FLFLMB (mkList . pushOne)
  where
    mkList :: (item :> Maybe2 fixup) wX wY -> (FL item :> Maybe2 fixup) wX wY
    mkList (item :> fixups) = (item :>: NilFL) :> fixups

pushFixupIdMB_FLIdFLFL
  :: PushFixupFn     fixup  item     item (Maybe2 fixup)
  -> PushFixupFn (FL fixup) item     item (FL     fixup)
pushFixupIdMB_FLIdFLFL _pushOne (NilFL :> item)
  = item :> NilFL
pushFixupIdMB_FLIdFLFL pushOne ((fixup :>: fixups) :> item)
  = case pushFixupIdMB_FLIdFLFL pushOne (fixups :> item) of
      item' :> fixups2' ->
        case pushOne (fixup :> item') of
          item'' :> Nothing2      -> item'' :>             fixups2'
          item'' :> Just2 fixup1' -> item'' :> fixup1' :>: fixups2'
