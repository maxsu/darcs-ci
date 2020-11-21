module Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCanonize(..)
    , PrimClassify(..), PrimDetails(..)
    , PrimSift(..)
    , PrimShow(..), PrimRead(..)
    , PrimApply(..)
    , PrimPatch
    , PrimMangleUnravelled(..)
    , Mangled
    , Unravelled
    , primCleanMerge
    )
    where

import Darcs.Prelude

import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.FileHunk ( FileHunk, IsHunk )
import Darcs.Patch.Format ( FileNameFormat, PatchListFormat )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.CommuteFn ( PartialMergeFn )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Repair ( RepairToFL )
import Darcs.Patch.Show ( ShowPatch, ShowContextPatch )
import Darcs.Patch.SummaryData ( SummDetail )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered ( FL, (:>)(..), (:\/:)(..), (:/\:)(..) )
import Darcs.Patch.Witnesses.Show ( Show2 )
import Darcs.Patch.Witnesses.Sealed ( Sealed )

import Darcs.Util.Parser ( Parser )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Printer ( Doc )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm )

import qualified Data.ByteString as B ( ByteString )


type PrimPatch prim =
    ( Apply prim
    , CleanMerge prim
    , Commute prim
    , Invert prim
    , Eq2 prim
    , IsHunk prim
    , PatchInspect prim
    , RepairToFL prim
    , Show2 prim
    , PrimConstruct prim
    , PrimCanonize prim
    , PrimClassify prim
    , PrimDetails prim
    , PrimApply prim
    , PrimSift prim
    , PrimMangleUnravelled prim
    , ReadPatch prim
    , ShowPatch prim
    , ShowContextPatch prim
    , PatchListFormat prim
    )

class PrimClassify prim where
   primIsAddfile :: prim wX wY -> Bool
   primIsRmfile :: prim wX wY -> Bool
   primIsAdddir :: prim wX wY -> Bool
   primIsRmdir :: prim wX wY -> Bool
   primIsMove :: prim wX wY -> Bool
   primIsHunk :: prim wX wY -> Bool
   primIsTokReplace :: prim wX wY -> Bool
   primIsBinary :: prim wX wY -> Bool
   primIsSetpref :: prim wX wY -> Bool
   is_filepatch :: prim wX wY -> Maybe AnchoredPath

class PrimConstruct prim where
   addfile :: AnchoredPath -> prim wX wY
   rmfile :: AnchoredPath -> prim wX wY
   adddir :: AnchoredPath -> prim wX wY
   rmdir :: AnchoredPath -> prim wX wY
   move :: AnchoredPath -> AnchoredPath -> prim wX wY
   changepref :: String -> String -> String -> prim wX wY
   hunk :: AnchoredPath -> Int -> [B.ByteString] -> [B.ByteString] -> prim wX wY
   tokreplace :: AnchoredPath -> String -> String -> String -> prim wX wY
   binary :: AnchoredPath -> B.ByteString -> B.ByteString -> prim wX wY
   primFromHunk :: FileHunk wX wY -> prim wX wY

class PrimCanonize prim where
   -- | @tryToShrink ps@ simplifies @ps@ by getting rid of self-cancellations
   --   or coalescing patches
   --
   --   Question (Eric Kow): what properties should this have?  For example,
   --   the prim1 implementation only gets rid of the first self-cancellation
   --   it finds (as far as I can tell).  Is that OK? Can we try harder?
   tryToShrink :: FL prim wX wY -> FL prim wX wY

   -- | 'sortCoalesceFL' @ps@ coalesces as many patches in @ps@ as
   --   possible, sorting the results in some standard order.
   sortCoalesceFL :: FL prim wX wY -> FL prim wX wY

   -- | It can sometimes be handy to have a canonical representation of a given
   -- patch.  We achieve this by defining a canonical form for each patch type,
   -- and a function 'canonize' which takes a patch and puts it into
   -- canonical form.  This routine is used by the diff function to create an
   -- optimal patch (based on an LCS algorithm) from a simple hunk describing the
   -- old and new version of a file.
   canonize :: D.DiffAlgorithm -> prim wX wY -> FL prim wX wY

   -- | 'canonizeFL' @ps@ puts a sequence of primitive patches into
   -- canonical form. Even if the patches are just hunk patches,
   -- this is not necessarily the same set of results as you would get
   -- if you applied the sequence to a specific tree and recalculated
   -- a diff.
   --
   -- Note that this process does not preserve the commutation behaviour
   -- of the patches and is therefore not appropriate for use when
   -- working with already recorded patches (unless doing amend-record
   -- or the like).
   canonizeFL :: D.DiffAlgorithm -> FL prim wX wY -> FL prim wX wY

   -- | Either 'primCoalesce' or cancel inverses.
   --
   -- prop> primCoalesce (p :> q) == Just r => apply r = apply p >> apply q
   -- prop> primCoalesce (p :> q) == Just r => lengthFL r < 2
   coalesce :: (prim :> prim) wX wY -> Maybe (FL prim wX wY)

   -- | Coalesce adjacent patches to one with the same effect.
   --
   -- prop> apply (primCoalesce p q) == apply p >> apply q
   primCoalesce :: prim wX wY -> prim wY wZ -> Maybe (prim wX wZ)

   -- | If 'primCoalesce' is addition, then this is subtraction.
   --
   -- prop> Just r == primCoalesce p q => primDecoalesce r p == Just q
   primDecoalesce :: prim wX wZ -> prim wX wY -> Maybe (prim wY wZ)

-- TODO This has been cut'n'pasted from Darcs.Repository.Pending.
--      It is not a good interface and should be re-designed.
class PrimSift prim where
  -- | @siftForPending ps@ simplifies the candidate pending patch @ps@
  --   through a combination of looking for self-cancellations
  --   (sequences of patches followed by their inverses), coalescing,
  --   and getting rid of any hunk/binary patches we can commute out
  --   the back
  --
  --   The visual image of sifting can be quite helpful here.  We are
  --   repeatedly tapping (shrinking) the patch sequence and
  --   shaking it (sift). Whatever falls out is the pending we want
  --   to keep. We do this until the sequence looks about as clean as
  --   we can get it
  siftForPending :: FL prim wX wY -> Sealed (FL prim wX)

class PrimDetails prim where
   summarizePrim :: prim wX wY -> [SummDetail]

class PrimShow prim where
   showPrim :: FileNameFormat -> prim wA wB -> Doc
   showPrimCtx :: ApplyMonad  (ApplyState prim) m => FileNameFormat -> prim wA wB -> m Doc

class PrimRead prim where
   readPrim :: FileNameFormat -> Parser (Sealed (prim wX))

class PrimApply prim where
   applyPrimFL :: ApplyMonad (ApplyState prim) m => FL prim wX wY -> m ()

-- | A list of conflicting alternatives. They form a connected
-- component of the conflict graph i.e. one transitive conflict.
type Unravelled prim wX = [Sealed (FL prim wX)]

-- | Result of mangling a single Unravelled.
type Mangled prim wX = Sealed (FL prim wX)

class PrimMangleUnravelled prim where
  -- | Mangle conflicting alternatives if possible.
  mangleUnravelled :: Unravelled prim wX -> Maybe (Mangled prim wX)

primCleanMerge :: (Commute prim, Invert prim) => PartialMergeFn prim prim
primCleanMerge (p :\/: q) = do
  q' :> ip' <- commute (invert p :> q)
  return $ q' :/\: invert ip'
