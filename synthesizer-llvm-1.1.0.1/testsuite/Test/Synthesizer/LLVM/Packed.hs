{-# LANGUAGE NoImplicitPrelude #-}
module Test.Synthesizer.LLVM.Packed (tests) where

import qualified Test.Synthesizer.LLVM.Generator as Gen
import Test.Synthesizer.LLVM.Generator
   (Test, checkWithParam, arg, pair, withGenArgs)
import Test.Synthesizer.LLVM.Utility
   (checkSimilarity, checkEquality,
    CheckSimilarity, CheckEquality, checkSimilarityPacked)

import qualified Synthesizer.LLVM.Wave as Wave
import LLVM.DSL.Expression (Exp)

import Type.Data.Num.Decimal (D4)
import qualified Type.Data.Num.Decimal as TypeNum

import qualified Synthesizer.LLVM.Frame.SerialVector.Plain as SerialPlain
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as SerialCode
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Core as SigCore
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Causal.Exponential2 as Exp
import qualified Synthesizer.LLVM.Causal.Process as Causal
import Synthesizer.LLVM.Causal.Process (($*))

import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Data.StorableVector.Lazy as SVL
import Data.StorableVector.Lazy (ChunkSize)

import Control.Arrow ((<<<))
import Control.Applicative ((<$>))

import Data.Word (Word, Word32)

import qualified Test.QuickCheck as QC

import qualified Algebra.Ring as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base


type SimFloat = CheckSimilarity Float
type VectorValue = SerialCode.Value D4 Float

signalLength :: Int
signalLength = 10000


limitFloat :: SVL.Vector Float -> SVL.Vector Float
limitFloat = SVL.take signalLength


withDur :: (Ring.C a) => IO (ChunkSize -> a -> b) -> Test a b
withDur =
   withGenArgs (arg (fromIntegral <$> QC.choose (signalLength, 2*signalLength)))

{-
limitPackedFloat ::
   SVL.Vector (SerialPlain.T D4 Float) -> SVL.Vector (SerialPlain.T D4 Float)
limitPackedFloat = SVL.take (div signalLength 4)
-}

constant :: Test Float SimFloat
constant =
   withGenArgs (Gen.choose (-1, 1)) $
      checkSimilarityPacked 1e-3 limitFloat
         (\y -> Sig.constant y) (\y -> SigPS.constant y)

ramp :: Test Float SimFloat
ramp =
   withDur $
      checkSimilarityPacked 1e-3 limitFloat
         (\dur -> Sig.rampInf dur) (\dur -> SigPS.rampInf dur)

parabolaFadeIn :: Test Float SimFloat
parabolaFadeIn =
   withDur $
      checkSimilarityPacked 1e-3 limitFloat
         (\dur -> Sig.parabolaFadeInInf dur)
         (\dur -> SigPS.parabolaFadeInInf dur)

parabolaFadeOut :: Test Float SimFloat
parabolaFadeOut =
   withDur $
      checkSimilarityPacked 1e-3 limitFloat
         (\dur -> Sig.parabolaFadeOutInf dur)
         (\dur -> SigPS.parabolaFadeOutInf dur)

parabolaFadeInMap :: Test Word SimFloat
parabolaFadeInMap =
   withDur $
      checkSimilarity 1e-3 limitFloat
          (\dur -> Sig.parabolaFadeIn dur)
          (\dur -> Sig.parabolaFadeInMap dur)

parabolaFadeOutMap :: Test Word SimFloat
parabolaFadeOutMap =
   withDur $
      checkSimilarity 1e-3 limitFloat
          (\dur -> Sig.parabolaFadeOut dur)
          (\dur -> Sig.parabolaFadeOutMap dur)


genExp :: QC.Gen (Float, Float)
genExp = pair (Gen.choose (1000,10000)) (Gen.choose (-1,1))

exponential2 :: Test (Float,Float) SimFloat
exponential2 =
   withGenArgs genExp $
      checkSimilarityPacked 1e-3 limitFloat
         (\(halfLife,start) -> Sig.exponential2 halfLife start)
         (\(halfLife,start) -> SigPS.exponential2 halfLife start)

exponential2Static :: Test (Float,Float) SimFloat
exponential2Static =
   withGenArgs genExp $
      checkSimilarity 1e-3 limitFloat
          (\(halfLife,start) -> Sig.exponential2 halfLife start)
          (\(halfLife,start) ->
           Exp.causal start <<<
           Causal.map Exp.parameterPlain $*
           Sig.constant halfLife)

exponential2PackedStatic :: Test (Float,Float) SimFloat
exponential2PackedStatic =
   withGenArgs genExp $
      checkSimilarity 1e-3 (limitFloat . SigStL.unpack)
         (\(halfLife,start) ->
            SigPS.exponential2 halfLife start :: Sig.T VectorValue)
         (\(halfLife,start) ->
           Exp.causalPacked start <<<
           Causal.map Exp.parameterPackedExp $*
           Sig.constant halfLife)

exponential2Controlled :: Test ((Float,Float), (Float,Float)) SimFloat
exponential2Controlled =
   withGenArgs
      (pair genExp
         (pair (Gen.choose (0.0001, 0.001)) (Gen.choose (0, 0.99 :: Float)))) $

   let lfo halfLife freq phase =
          Causal.mapExponential 2 halfLife $*
          Sig.osci Wave.approxSine2 phase freq
   in  checkSimilarityPacked 1e-3 limitFloat
          (-- 'freq' is the LFO frequency measured at vector-rate
           \((halfLife,start), (freq,phase)) ->
           Exp.causal start <<<
           Causal.map Exp.parameterPlain $*
           Sig.interpolateConstant
              (TypeNum.integralFromProxy TypeNum.d4 :: Exp Float)
              (lfo halfLife freq phase))
          (\((halfLife,start), (freq,phase)) ->
           Exp.causalPacked start <<<
           Causal.map Exp.parameterPackedExp $* lfo halfLife freq phase)

osci :: Test (Float,Float) SimFloat
osci =
   withGenArgs
      (pair
         (Gen.choose (0.001, 0.01))
         (Gen.choose (0, 0.99))) $
   checkSimilarityPacked 1e-2 limitFloat
      (\(freq,phase) -> Sig.osci Wave.approxSine2 phase freq)
      (\(freq,phase) -> SigPS.osci Wave.approxSine2 phase freq)



limitWord32 :: SVL.Vector Word32 -> SVL.Vector Word32
limitWord32 = SVL.take signalLength

limitPackedWord32 ::
   SVL.Vector (SerialPlain.T D4 Word32) -> SVL.Vector (SerialPlain.T D4 Word32)
limitPackedWord32 = SVL.take (div signalLength 4)


noise :: IO (ChunkSize -> Word32 -> CheckEquality Word32)
noise =
   checkEquality limitWord32 SigCore.noise SigCore.noiseAlt

noiseVector ::
   IO (ChunkSize -> Word32 -> CheckEquality (SerialPlain.T D4 Word32))
noiseVector =
   checkEquality limitPackedWord32 SigPS.noiseCore SigPS.noiseCoreAlt

noiseScalarVector ::
   IO (ChunkSize -> Word32 -> CheckEquality (SerialPlain.T D4 Word32))
noiseScalarVector =
   checkEquality limitPackedWord32
      SigPS.noiseCore
      (SigPS.packSmall . SigCore.noise)


tests :: [(String, IO QC.Property)]
tests =
   ("constant", checkWithParam constant) :
   ("ramp", checkWithParam ramp) :
   ("parabolaFadeIn", checkWithParam parabolaFadeIn) :
   ("parabolaFadeOut", checkWithParam parabolaFadeOut) :
   ("parabolaFadeInMap", checkWithParam parabolaFadeInMap) :
   ("parabolaFadeOutMap", checkWithParam parabolaFadeOutMap) :
   ("exponential2", checkWithParam exponential2) :
   ("exponential2Static", checkWithParam exponential2Static) :
   ("exponential2PackedStatic", checkWithParam exponential2PackedStatic) :
   ("exponential2Controlled", checkWithParam exponential2Controlled) :
   ("osci", checkWithParam osci) :
   ("noise", QC.property <$> noise) :
   ("noiseVector", QC.property <$> noiseVector) :
   ("noiseScalarVector", QC.property <$> noiseScalarVector) :
   []
