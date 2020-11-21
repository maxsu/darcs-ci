{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Darcs.Test.Patch.Properties.Check ( Check(..), checkAPatch ) where

import Prelude ()
import Darcs.Prelude
import Control.Monad ( liftM )

import Darcs.Test.Patch.Check ( PatchCheck,
                                checkMove, removeDir, createDir,
                                isValid, insertLine, fileEmpty, fileExists,
                                deleteLine, modifyFile, createFile, removeFile,
                                doCheck, inconsistent, FileContents(..)
                              )
import Darcs.Patch.RegChars ( regChars )
import Darcs.Util.ByteString ( linesPS )
import qualified Data.ByteString as B ( ByteString, null, concat )
import qualified Data.ByteString.Char8 as BC ( break, pack )
import qualified Data.IntMap as M ( mapMaybe )

import Darcs.Patch ( invert, effect, PrimPatch )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.V1 ( )
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )
import Darcs.Patch.V2.RepoPatch ( RepoPatchV2, isConsistent )
import Darcs.Patch.V3.Core ( RepoPatchV3 )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim(..) )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim(..) )
import qualified Darcs.Patch.Prim.FileUUID as FileUUID ( Prim )
import Darcs.Patch.Prim.WithName ( PrimWithName(..) )
import Darcs.Patch.Prim.V1.Core ( Prim(..), DirPatchType(..), FilePatchType(..) )
import Darcs.Patch.Witnesses.Ordered

type Prim1 = V1.Prim
type Prim2 = V2.Prim

class Check p where
   checkPatch :: p wX wY -> PatchCheck ()

instance Check p => Check (FL p) where
   checkPatch NilFL = isValid
   checkPatch (p :>: ps) = checkPatch p >> checkPatch ps

instance Check p => Check (p:>p) where
   checkPatch (p1 :> p2) = checkPatch p1 >> checkPatch p2

checkAPatch :: (Invert p, Check p) => p wX wY -> Bool
checkAPatch p = doCheck $ do checkPatch p
                             checkPatch $ invert p

instance PrimPatch prim => Check (RepoPatchV2 prim) where
  checkPatch p = maybe isValid (const inconsistent) $ isConsistent p

instance (PrimPatch prim, Check prim) => Check (RepoPatchV1 prim) where
  checkPatch = checkPatch . effect

instance Check prim => Check (RepoPatchV3 name prim) where
  checkPatch = checkPatch . effect

deriving instance Check Prim1
deriving instance Check Prim2

instance Check prim => Check (PrimWithName name prim) where
  checkPatch = checkPatch . wnPatch

instance Check FileUUID.Prim where
  checkPatch _ = isValid -- XXX

instance Check Prim where

   checkPatch (FP f RmFile) = removeFile f
   checkPatch (FP f AddFile) =  createFile f
   -- This is stupid but was designed that way ages ago:
   -- empty hunks commute with everything, so the file need
   -- not even exist, nor the line in the file.
   -- Perhaps we should avoid generating empty hunks.
   checkPatch (FP _ (Hunk _ [] [])) = isValid
   checkPatch (FP f (Hunk line old new)) = do
       fileExists f
       mapM_ (deleteLine f line) old
       mapM_ (insertLine f line) (reverse new)
   checkPatch (FP f (TokReplace t old new)) =
       modifyFile f (tryTokPossibly t old new)
   -- note that the above isn't really a sure check, as it leaves PSomethings
   -- and PNothings which may have contained new...
   checkPatch (FP f (Binary o n)) = do
       fileExists f
       mapM_ (deleteLine f 1) (linesPS o)
       fileEmpty f
       mapM_ (insertLine f 1) (reverse $ linesPS n)

   checkPatch (DP d AddDir) = createDir d
   checkPatch (DP d RmDir) = removeDir d

   checkPatch (Move f f') = checkMove f f'
   checkPatch (ChangePref _ _ _) = isValid

tryTokPossibly :: String -> String -> String
                -> (Maybe FileContents) -> (Maybe FileContents)
tryTokPossibly t o n = liftM $ \contents ->
        let lines' = M.mapMaybe (liftM B.concat
                                  . tryTokInternal t (BC.pack o) (BC.pack n))
                                (fcLines contents)
        in contents { fcLines = lines' }

tryTokInternal :: String -> B.ByteString -> B.ByteString
                 -> B.ByteString -> Maybe [B.ByteString]
tryTokInternal _ _ _ s | B.null s = Just []
tryTokInternal t o n s =
    case BC.break (regChars t) s of
    (before,s') ->
        case BC.break (not . regChars t) s' of
        (tok,after) ->
            case tryTokInternal t o n after of
            Nothing -> Nothing
            Just rest ->
                if tok == o
                then Just $ before : n : rest
                else if tok == n
                     then Nothing
                     else Just $ before : tok : rest
