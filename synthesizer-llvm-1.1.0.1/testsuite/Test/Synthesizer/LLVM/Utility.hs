{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Synthesizer.LLVM.Utility where

import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as SerialCode
import Synthesizer.LLVM.Causal.Process ()

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Causal.Class as CausalClass
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as SigS
import qualified Synthesizer.Zip as Zip

import Control.Monad (liftM, liftM2)
import Control.Applicative ((<$>))

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Data.StorableVector.Lazy (ChunkSize)
import Foreign.Storable (Storable)

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value as MultiValue

import qualified Type.Data.Num.Decimal as TypeNum

import System.Random (Random, randomRs, StdGen, mkStdGen)

import Data.Tuple.HT (mapPair)

import qualified Test.QuickCheck as QC

import qualified System.Unsafe as Unsafe

import qualified Algebra.RealRing as RealRing
import qualified Algebra.Absolute as Absolute

import NumericPrelude.Numeric
import NumericPrelude.Base


genRandomVectorParam :: QC.Gen (Int, StdGen)
genRandomVectorParam =
   liftM2 (,) (QC.choose (1,100)) (mkStdGen <$> QC.arbitrary)

randomStorableVector ::
   (Storable a, Random a) =>
   (a, a) -> (Int, StdGen) -> SV.Vector a
randomStorableVector range (len, seed) =
   fst $ SV.packN len $ randomRs range seed

randomStorableVectorLoop ::
   (Storable a, Random a) =>
   (a, a) -> (Int, StdGen) -> SVL.Vector a
randomStorableVectorLoop range param =
   SVL.cycle $ SVL.fromChunks [randomStorableVector range param]


render ::
   (Render.RunArg p) =>
   (Storable.C a, MultiValue.T a ~ al) =>
   (SVL.Vector a -> sig) ->
   (Render.DSLArg p -> Sig.T al) -> IO (ChunkSize -> p -> sig)
render limit sig =
   fmap (\func chunkSize -> limit . func chunkSize) $ Render.run sig

render2 ::
   (Render.RunArg p) =>
   (Storable.C a, MultiValue.T a ~ al) =>
   (Storable.C b, MultiValue.T b ~ bl) =>
   ((SVL.Vector a, SVL.Vector b) -> sig) ->
   (Render.DSLArg p -> Sig.T (al, bl)) -> IO (ChunkSize -> p -> sig)
render2 limit sig = do
   proc <- CausalRender.run (CausalClass.fromSignal . sig)
   return $ \(SVL.ChunkSize chunkSize) p ->
      limit . mapPair (SVL.fromChunks, SVL.fromChunks) .
      unzip . map (\(Zip.Cons a b) -> (a,b)) $
      Unsafe.performIO (PIO.runCont (proc p)) (const [])
         (repeat $ SigG.LazySize chunkSize)


data CheckSimilarityState a =
   CheckSimilarityState a (SVL.Vector a) (SigS.T a)

instance (Storable a, Ord a, Absolute.C a) =>
      QC.Testable (CheckSimilarityState a) where
   property (CheckSimilarityState tol xs ys) =
      QC.property $
         SigS.foldR (&&) True $
         -- dangerous, since shortened signals would be tolerated
         SigS.zipWith (\x y -> abs(x-y) < tol)
            (SigS.fromStorableSignal xs) ys

{-# INLINE checkSimilarityState #-}
checkSimilarityState ::
   (Render.RunArg p) =>
   (RealRing.C a, Storable.C a, MultiValue.T a ~ av) =>
   a ->
   (SVL.Vector a -> SVL.Vector a) ->
   (Render.DSLArg p -> Sig.T av) ->
   (p -> SigS.T a) ->
   IO (ChunkSize -> p -> CheckSimilarityState a)
checkSimilarityState tol limit gen0 sig1 =
   liftM
      (\sig0 chunkSize p ->
         CheckSimilarityState tol (sig0 chunkSize p) (sig1 p))
      (render limit gen0)


data CheckSimilarity a =
   CheckSimilarity a (SVL.Vector a) (SVL.Vector a)

instance
   (Storable a, Ord a, Absolute.C a) =>
      QC.Testable (CheckSimilarity a) where
   property (CheckSimilarity tol xs ys) =
      QC.property $
         SigS.foldR (&&) True $
         -- dangerous, since shortened signals would be tolerated
         SigS.zipWith (\x y -> abs(x-y) < tol)
            (SigS.fromStorableSignal xs)
            (SigS.fromStorableSignal ys)

{-# INLINE checkSimilarity #-}
checkSimilarity ::
   (Render.RunArg p) =>
   (RealRing.C b, Storable.C b,
    Storable.C a, MultiValue.T a ~ av) =>
   b ->
   (SVL.Vector a -> SVL.Vector b) ->
   (Render.DSLArg p -> Sig.T av) ->
   (Render.DSLArg p -> Sig.T av) ->
   IO (ChunkSize -> p -> CheckSimilarity b)
checkSimilarity tol limit gen0 gen1 =
   liftM2
      (\sig0 sig1 chunkSize p ->
         CheckSimilarity tol (sig0 chunkSize p) (sig1 chunkSize p))
      (render limit gen0)
      (render limit gen1)

checkSimilarityPacked ::
   (Render.RunArg p) =>
   Float ->
   (SVL.Vector Float -> SVL.Vector Float) ->
   (Render.DSLArg p -> Sig.T (MultiValue.T Float)) ->
   (Render.DSLArg p -> Sig.T (SerialCode.Value TypeNum.D4 Float)) ->
   IO (ChunkSize -> p -> CheckSimilarity Float)
checkSimilarityPacked tol limit scalar vector =
   checkSimilarity tol limit scalar (SigPS.unpack . vector)


{- |
Instead of testing on equality immediately
we use this interim data type.
This allows us to inspect the signals that are compared.
-}
data CheckEqualityGen a = CheckEqualityGen a a

type CheckEquality a = CheckEqualityGen (SVL.Vector a)
type CheckEquality2 a b = CheckEqualityGen (SVL.Vector a, SVL.Vector b)

instance (Eq a) => QC.Testable (CheckEqualityGen a) where
   property (CheckEqualityGen x y) = QC.property (x==y)

checkEquality ::
   (Render.RunArg p) =>
   (Eq a, Storable.C a, MultiValue.T a ~ av) =>
   (SVL.Vector a -> SVL.Vector a) ->
   (Render.DSLArg p -> Sig.T av) ->
   (Render.DSLArg p -> Sig.T av) ->
   IO (ChunkSize -> p -> CheckEquality a)
checkEquality limit gen0 gen1 =
   liftM2
      (\sig0 sig1 chunkSize p ->
         CheckEqualityGen (sig0 chunkSize p) (sig1 chunkSize p))
      (render limit gen0)
      (render limit gen1)

checkEquality2 ::
   (Render.RunArg p) =>
   (Eq a, Storable.C a, MultiValue.T a ~ al) =>
   (Eq b, Storable.C b, MultiValue.T b ~ bl) =>
   (SVL.Vector a -> SVL.Vector a) ->
   (SVL.Vector b -> SVL.Vector b) ->
   (Render.DSLArg p -> Sig.T (al,bl)) ->
   (Render.DSLArg p -> Sig.T (al,bl)) ->
   IO (ChunkSize -> p -> CheckEquality2 a b)
checkEquality2 limitA limitB gen0 gen1 =
   liftM2
      (\sig0 sig1 chunkSize p ->
         CheckEqualityGen (sig0 chunkSize p) (sig1 chunkSize p))
      (render2 (mapPair (limitA, limitB)) gen0)
      (render2 (mapPair (limitA, limitB)) gen1)
