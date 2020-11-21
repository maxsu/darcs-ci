module Darcs.Test.Patch.Arbitrary.PrimV1
    ( aPrim
    , aPrimPair
    ) where

import Prelude ()
import Darcs.Prelude

import qualified Darcs.Test.Patch.Arbitrary.Generic as T
import Darcs.Test.Patch.Arbitrary.Generic
    ( NullPatch(..)
    , MightBeEmptyHunk
    , MightHaveDuplicate
    , ArbitraryPrim
    )
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.Arbitrary.Shrink

import Control.Applicative ( (<|>) )
import Test.QuickCheck
import Darcs.Test.Patch.WithState
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Prim.V1.Core ( FilePatchType( Hunk ), isIdentity )
import qualified Darcs.Patch.Prim.V1.Core as Prim ( Prim( FP ) )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim(..) )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim(..) )

import Darcs.Test.Patch.V1Model
import Darcs.Util.Path
import Darcs.Util.Tree ( Tree )
import Darcs.Test.Util.QuickCheck ( alpha, notIn, maybeOf )

import Darcs.UI.Commands.Replace ( defaultToks )
import Darcs.Patch.Prim ( PrimPatch, PrimConstruct(..) )
import Darcs.Patch.Apply ( ApplyState )

import Control.Monad ( guard )
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( fromJust, isJust )

type Prim1 = V1.Prim
type Prim2 = V2.Prim

type instance ModelOf Prim1 = V1Model
type instance ModelOf Prim2 = V1Model

instance ArbitraryPrim Prim1
instance ArbitraryPrim Prim2

instance NullPatch Prim.Prim where
  nullPatch (Prim.FP _ fp) = nullPatch fp
  nullPatch p | IsEq <- isIdentity p = IsEq
  nullPatch _ = NotEq
deriving instance NullPatch Prim1
deriving instance NullPatch Prim2

instance NullPatch FilePatchType where
  nullPatch (Hunk _ [] []) = unsafeCoerceP IsEq -- is this safe?
  nullPatch _ = NotEq

instance MightBeEmptyHunk Prim.Prim where
  isEmptyHunk (Prim.FP _ (Hunk _ [] [])) = True
  isEmptyHunk _ = False
deriving instance MightBeEmptyHunk Prim1
deriving instance MightBeEmptyHunk Prim2

instance MightHaveDuplicate Prim1
instance MightHaveDuplicate Prim2

-- TODO add some useful shrinking, at least to
-- shrinkAtEnd/shrinkAtStart
instance Shrinkable Prim.Prim where
  shrinkInternally _ = []
  shrinkAtEnd _ = []
  shrinkAtStart _ = []

deriving instance Shrinkable V1.Prim
deriving instance Shrinkable V2.Prim

----------------------------------------------------------------------
-- * QuickCheck generators

----------------------------------------------------------------------
-- ** FilePatchType generators

aHunk :: Content -> Gen (Int, [BC.ByteString], [BC.ByteString])
aHunk content
 = (sized $ \n ->
     do pos <- choose (1, contentLen+1)
        let prefixLen = pos-1
            restLen   = contentLen-prefixLen
        oldLen <- frequency
                      [ (75, choose (0, min restLen n))
                        -- produces small hunks common in real editing
                      , (25, choose (0, min 10 restLen))
                      ]
        -- newLen choice aims to cover all possibilities, that is,
        -- remove less/the same/more than added and empty the file.
        newLen <- frequency
                      [ ( 54
                        , choose (1,min 1 n)
                        )
                      , ( if oldLen /= 0 then 42 else 0
                        , choose (1,min 1 oldLen)
                        )
                      , ( if oldLen /= 0 then 2 else 0
                        , return oldLen
                        )
                      , ( if oldLen /= 0 then 2 else 0
                        , return 0
                        )
                      ]
        new <- vectorOf newLen aLine
        let old = take oldLen $ drop prefixLen $ content
        return (pos, old, new)) `suchThat` notEmptyHunk
  where
      contentLen = length content
      notEmptyHunk (_,old,new) = not (null old && null new)

aTokReplace :: Content -> Gen (String, String, String)
aTokReplace []
  = do w <- vectorOf 1 alpha
       w' <- vectorOf 1 alpha
       return (defaultToks, w, w')
aTokReplace content
  = do let fileWords = concatMap BC.words content
       wB <- elements fileWords
       w' <- alphaBS `notIn` fileWords
       return (defaultToks, BC.unpack wB, BC.unpack w')
  where
      alphaBS = do x <- alpha; return $ BC.pack [x]

----------------------------------------------------------------------
-- ** Prim generators

aHunkP :: PrimPatch prim => (AnchoredPath, File) -> Gen (prim wX wY)
aHunkP (path,file)
  = do (pos, old, new) <- aHunk content
       return $ hunk path pos old new
  where
      content = fileContent file

aTokReplaceP :: PrimPatch prim => (AnchoredPath,File) -> Gen (prim wX wY)
aTokReplaceP (path,file)
  = do (tokchars, old, new) <- aTokReplace content
       return $ tokreplace path tokchars old new
  where
      content = fileContent file

anAddFileP :: PrimPatch prim => (AnchoredPath,Dir) -> Gen (prim wX wY)
anAddFileP (path,dir)
  = do newFilename <- aFilename `notIn` existing
       let newPath = path `appendPath` newFilename
       return $ addfile newPath
  where
      existing = map fst $ filterFiles $ dirContent dir

aRmFileP :: PrimPatch prim
         => AnchoredPath   -- ^ Path of an empty file
         -> prim wX wY
aRmFileP path = rmfile path

anAddDirP :: PrimPatch prim => (AnchoredPath,Dir) -> Gen (prim wX wY)
anAddDirP (path,dir)
  = do newDirname <- aDirname `notIn` existing
       let newPath = path `appendPath` newDirname
       return $ adddir newPath
  where
      existing = map fst $ filterDirs $ dirContent dir

aRmDirP :: PrimPatch prim
        => AnchoredPath    -- ^ Path of an empty directory
        -> prim wX wY
aRmDirP path = rmdir path

aMoveP :: PrimPatch prim
       => Gen Name -> AnchoredPath -> (AnchoredPath,Dir) -> Gen (prim wX wY)
aMoveP nameGen oldPath (dirPath,dir)
  = do newName <- nameGen `notIn` existing
       let newPath = dirPath `appendPath` newName
       return $ move oldPath newPath
  where
      existing = map fst $ dirContent dir

aModelShrink :: V1Model wX -> [Sealed (Prim.Prim wX)]
aModelShrink repo =
  aModelShrinkName repo <|>
  aModelDeleteFile repo <|>
  aModelDeleteDir repo <|>
  aModelShrinkFileContent repo

shrinkPath :: AnchoredPath -> [AnchoredPath]
shrinkPath (AnchoredPath ps) = do
  ps' <- shrinkList shrinkName ps
  guard (not $ null ps')
  return $ AnchoredPath ps'

shrinkName :: Name -> [Name]
shrinkName n = do
  n' <- shrink (BC.unpack . encodeWhiteName $ n)
  guard (n' /= ".")
  guard (not $ null n')
  return $ decodeWhiteName $ BC.pack n'

aModelShrinkName :: V1Model wX -> [Sealed (Prim.Prim wX)]
aModelShrinkName repo = do
  (oldPath, _) <- list repo
  newPath <- shrinkPath oldPath
  guard (newPath `notElem` map fst (list repo))
  return $ Sealed $ move oldPath newPath

aModelDeleteFile :: V1Model wX -> [Sealed (Prim.Prim wX)]
aModelDeleteFile repo = do
  (path, _) <- filterFiles (list repo)
  return $ Sealed $ rmfile path

aModelDeleteDir :: V1Model wX -> [Sealed (Prim.Prim wX)]
aModelDeleteDir repo = do
  (path, _) <- filterDirs (list repo)
  return $ Sealed $ rmdir path

aModelShrinkFileContent :: V1Model wX -> [Sealed (Prim.Prim wX)]
aModelShrinkFileContent repo = do
  (path, file) <- filterFiles (list repo)
  (pos, lineToRemove) <- zip [1..] $ fileContent file
  (return (Sealed $ hunk path pos [lineToRemove] [])
   <|>
   do
    smaller <- BC.pack <$> shrink (BC.unpack lineToRemove)
    return $ Sealed $ hunk path pos [lineToRemove] [smaller])


-- | Generates any type of 'prim' patch, except binary and setpref patches.
aPrim :: forall prim wX wY . (PrimPatch prim, ApplyState prim ~ RepoState V1Model)
      => V1Model wX -> Gen (WithEndState V1Model (prim wX) wY)
aPrim repo
  = do mbFile <- maybeOf repoFiles
       mbEmptyFile <- maybeOf $ filter (isEmpty . snd) repoFiles
       dir  <- elements (rootDir:repoDirs)
       mbOldDir <- maybeOf repoDirs
       mbEmptyDir <- maybeOf $ filter (isEmpty . snd) repoDirs
       patch <- frequency
                  [ ( if isJust mbFile then 12 else 0
                    , aHunkP $ fromJust mbFile
                    )
                  , ( if isJust mbFile then 6 else 0
                    , aTokReplaceP $ fromJust mbFile
                    )
                  , ( 2
                    , anAddFileP dir
                    )
                  , ( if isJust mbEmptyFile then 12 else 0
                    , return $ aRmFileP $ fst $ fromJust mbEmptyFile
                    )
                  , ( 2
                    , anAddDirP dir
                    )
                  , ( if isJust mbEmptyDir then 10 else 0
                    , return $ aRmDirP $ fst $ fromJust mbEmptyDir
                    )
                  , ( if isJust mbFile then 3 else 0
                    , aMoveP aFilename (fst $ fromJust mbFile) dir
                    )
                  , let oldPath = fst $ fromJust mbOldDir in
                    ( if isJust mbOldDir
                         && not (oldPath `isPrefix` fst dir)
                        then 4 else 0
                    , aMoveP aDirname oldPath dir
                    )
                  ]
       let repo' = unFail $ repoApply repo patch
       return $ WithEndState patch repo'
  where
      repoItems = list repo
      repoFiles = filterFiles repoItems
      repoDirs  = filterDirs repoItems
      rootDir   = (anchoredRoot,root repo)

{- [COVERAGE OF aPrim]

  PLEASE,
  if you change something that may affect the coverage of aPrim then
      a) recalculate it, or if that is not possible;
      b) indicate the need to do it.

  Patch type
  ----------
  42% hunk
  22% tokreplace
  14% move
   6% rmdir
   6% addfile
   6% adddir
   4% rmfile
-}

----------------------------------------------------------------------
-- *** Pairs of primitive patches

-- Try to generate commutable pairs of hunks
hunkPairP :: PrimPatch prim => (AnchoredPath, File) -> Gen ((prim :> prim) wX wY)
hunkPairP (path,file)
  = do (l1, old1, new1) <- aHunk content
       (delta, content') <- selectChunk (Hunk l1 old1 new1) content
       (l2', old2, new2) <- aHunk content'
       let l2 = l2'+delta
       return (hunk path l1 old1 new1 :> hunk path l2 old2 new2)
  where
      content = fileContent file
      selectChunk (Hunk l old new) content_
        = elements [prefix, suffix]
        where
            start = l - 1
            prefix = (0, take start content_)
            suffix = (start + length new, drop (start + length old) content_)
      selectChunk _ _ = error "impossible case"

aPrimPair :: ( PrimPatch prim
             , ArbitraryState prim
             , ApplyState prim ~ RepoState V1Model
             , ModelOf prim ~ V1Model
             )
          => V1Model wX
          -> Gen (WithEndState V1Model ((prim :> prim) wX) wY)
aPrimPair repo
  = do mbFile <- maybeOf repoFiles
       frequency
          [ ( if isJust mbFile then 1 else 0
            , do p1 :> p2 <- hunkPairP $ fromJust mbFile
                 let repo'  = unFail $ repoApply repo p1
                     repo'' = unFail $ repoApply repo' p2
                 return $ WithEndState (p1 :> p2) repo''
            )
          , ( 1
            , do Sealed wesP <- arbitraryState repo
                 return $ unsafeCoerceP1 wesP
            )
          ]
  where
      repoItems = list repo
      repoFiles = filterFiles repoItems

{- [COVERAGE OF aPrimPair]

  PLEASE,
  if you change something that may affect the coverage of aPrimPair then
      a) recalculate it, or if that is not possible;
      b) indicate the need to do it.

  Rate of ommutable pairs
  -----------------------
  67% commutable

  Commutable coverage (for 1000 tests)
  -------------------
  21% hunks-B
  20% hunks-A
  14% file:>dir
  12% file:>move
   8% trivial-FP
   8% hunk:>tok
   4% hunks-D
   3% tok:>tok
   2% hunks-C
   1% move:>move
   1% dir:>move
   1% dir:>dir
   0% emptyhunk:>file
-}

----------------------------------------------------------------------
-- Arbitrary instances

type instance ModelOf Prim.Prim = V1Model

instance ShrinkModel Prim.Prim where
  shrinkModelPatch s = aModelShrink s

-- use the special generator for pairs
arbitraryPair :: ( PrimPatch prim
                 , ApplyState prim ~ Tree
                 , ArbitraryState prim
                 , ModelOf prim ~ V1Model
                 )
              => Gen (Sealed2 (WithState (prim :> prim)))
arbitraryPair = do
  repo <- aSmallRepo
  WithEndState pp repo' <- aPrimPair repo
  return $ seal2 $ WithState repo pp repo'

-- Prim1

instance ArbitraryState Prim1 where
  arbitraryState s = seal <$> aPrim s

instance ShrinkModel Prim1 where
  shrinkModelPatch s = map (mapSeal V1.Prim) $ shrinkModelPatch s

instance PropagateShrink Prim1 Prim1 where
  propagateShrink = propagatePrim


instance Arbitrary (Sealed2 Prim1) where
  arbitrary = makeS2Gen aSmallRepo

instance Arbitrary (Sealed2 (Prim1 :> Prim1)) where
  arbitrary = mapSeal2 wsPatch <$> arbitraryPair

instance Arbitrary (Sealed2 (WithState Prim1)) where
  arbitrary = makeWS2Gen aSmallRepo

instance Arbitrary (Sealed2 (WithState (Prim1 :> Prim1))) where
  arbitrary = arbitraryPair

-- Prim2

instance ArbitraryState Prim2 where
  arbitraryState s = seal <$> aPrim s

instance ShrinkModel Prim2 where
  shrinkModelPatch s = map (mapSeal V2.Prim) $ shrinkModelPatch s

instance PropagateShrink Prim2 Prim2 where
  propagateShrink = propagatePrim


instance Arbitrary (Sealed2 Prim2) where
  arbitrary = makeS2Gen aSmallRepo

instance Arbitrary (Sealed2 (Prim2 :> Prim2)) where
  arbitrary = mapSeal2 wsPatch <$> arbitraryPair

instance Arbitrary (Sealed2 (WithState Prim2)) where
  arbitrary = makeWS2Gen aSmallRepo

instance Arbitrary (Sealed2 (WithState (Prim2 :> Prim2))) where
  arbitrary = arbitraryPair
