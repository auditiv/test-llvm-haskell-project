{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Synthesizer.LLVM.Helix (tests) where

import qualified Synthesizer.LLVM.Causal.Helix as Helix
import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Source as Source
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import Synthesizer.LLVM.Causal.Functional (($&), (&|&))
import Synthesizer.LLVM.Causal.Process (($*))

import qualified Data.StorableVector.Lazy as SVL
import Data.StorableVector.Lazy (ChunkSize)

import Test.Synthesizer.LLVM.Generator (checkWithParam)
import Test.Synthesizer.LLVM.Utility
          (CheckSimilarity, checkSimilarity,
           genRandomVectorParam, randomStorableVectorLoop)

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value as MultiValue

import Foreign.Storable (Storable)

import qualified System.Random as Rnd
import Data.Word (Word32)

import Control.Applicative (liftA2)

-- import qualified Graphics.Gnuplot.Simple as Gnuplot
import qualified Test.QuickCheck as QC

import qualified Algebra.Ring as Ring
import NumericPrelude.Numeric
import NumericPrelude.Base


type SimFloat = CheckSimilarity Float

signalLength :: Int
signalLength = 500


limitFloat :: (Storable a) => SVL.Vector a -> SVL.Vector a
limitFloat = SVL.take signalLength


randomSpeed :: (Int, Rnd.StdGen) -> SVL.Vector Float
randomSpeed = randomStorableVectorLoop (0,10)

randomPhase :: (Int, Rnd.StdGen) -> SVL.Vector Float
randomPhase = randomStorableVectorLoop (0,1)

genStaticDynamic ::
   QC.Gen (((Int, Rnd.StdGen), (Int, Rnd.StdGen)), (Float, Word32))
genStaticDynamic =
   liftA2 (,)
      (liftA2 (,) genRandomVectorParam genRandomVectorParam)
      (liftA2 (,) (QC.choose (1,32)) QC.arbitrary)

staticDynamic ::
   IO (ChunkSize ->
       (((Int, Rnd.StdGen), (Int, Rnd.StdGen)), (Float, Word32)) -> SimFloat)
staticDynamic =
   let len :: (Ring.C a) => a
       len = 1000
       noise :: Exp Word32 -> Sig.T (MultiValue.T Float)
       noise seed = Sig.noise seed 1

       static, dynamic ::
          ((Sig.T (MultiValue.T Float), Sig.T (MultiValue.T Float)),
           Exp Float,
           (Exp Word32, Exp (Source.StorableVector Float))) ->
          Func.T inp (MultiValue.T Float)
       static ((speedSig, phaseSig), period, (_, noiseSig)) =
          Helix.static Interpolation.linear Interpolation.linear
             (Expr.roundToIntFast period) period noiseSig
          $&
          Func.fromSignal (Causal.integrate zero $* speedSig)
          &|&
          Func.fromSignal phaseSig

       dynamic ((speedSig, phaseSig), period, (noiseParam, _)) =
          Helix.dynamic Interpolation.linear Interpolation.linear
             (Expr.roundToIntFast period) period
             (Causal.take len $* noise noiseParam)
          $&
          Func.fromSignal speedSig
          &|&
          Func.fromSignal phaseSig

   in liftA2
         (\noiseSig f chunkSize
               ((speedParam, phaseParam), (period, noiseParam)) ->
            f chunkSize
               ((randomSpeed speedParam, randomPhase phaseParam),
                period,
                (noiseParam, Render.buffer (noiseSig len noiseParam))))
         (Render.run noise)
         (checkSimilarity 5e-3 limitFloat
            (Func.compileSignal . static)
            (Func.compileSignal . dynamic))

{-
plot :: IO ()
plot = do
   render <- staticDynamic
   case render (SVL.chunkSize 1)
            (((76, Rnd.mkStdGen 0),(84, Rnd.mkStdGen 23)),(8.901705,11)) of
      CheckSimilarity _tol xs ys ->
         Gnuplot.plotLists [] [SVL.unpack xs, SVL.unpack ys]
         >>
         Gnuplot.plotList [] (zipWith (-) (SVL.unpack xs) (SVL.unpack ys))
-}


tests :: [(String, IO QC.Property)]
tests =
   ("staticDynamic", checkWithParam (genStaticDynamic, staticDynamic)) :
   []
