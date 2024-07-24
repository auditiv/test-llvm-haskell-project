{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Synthesizer.LLVM.Filter (tests) where

import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked
                                                            as ComplexFilterP
import qualified Synthesizer.LLVM.Filter.ComplexFirstOrder as ComplexFilter
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.FirstOrder as FirstOrder
import qualified Synthesizer.LLVM.Filter.SecondOrder as SecondOrder
import qualified Synthesizer.LLVM.Filter.SecondOrderPacked as SecondOrderP
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter
import qualified Synthesizer.LLVM.Filter.NonRecursive as FiltNR

import qualified Synthesizer.Plain.Filter.Recursive.Allpass    as AllpassCore
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as FirstOrderCore
import qualified Synthesizer.Plain.Filter.Recursive.Universal  as UniFilterCore
import qualified Synthesizer.Plain.Filter.Recursive.Moog       as MoogCore
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrderComplex
                                                            as ComplexFilterCore

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial
import qualified Synthesizer.LLVM.Wave as Wave
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Core as Core
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import Synthesizer.LLVM.Causal.Process (($<), ($*))

import Synthesizer.Plain.Filter.Recursive (Pole(Pole))
import qualified Synthesizer.Interpolation.Module as Ip
import qualified Synthesizer.Causal.Interpolation as InterpC
import qualified Synthesizer.Causal.Filter.NonRecursive as FiltC
import qualified Synthesizer.Causal.Displacement as DispC
import qualified Synthesizer.Causal.Process as CausalS
import qualified Synthesizer.State.Displacement as DispS
import qualified Synthesizer.State.Oscillator as OsciS
import qualified Synthesizer.State.Signal as SigS
import qualified Synthesizer.Basic.Wave as WaveCore
import qualified Synthesizer.Basic.Phase as Phase

import qualified Data.StorableVector.Lazy as SVL

import qualified Test.Synthesizer.LLVM.Generator as Gen
import Test.Synthesizer.LLVM.Generator
   (checkWithParam, arg, pair, withGenArgs)
import Test.Synthesizer.LLVM.Utility
   (checkSimilarity, checkSimilarityState,
    CheckSimilarity, CheckSimilarityState,
    randomStorableVector, checkSimilarityPacked)

import qualified Control.Category as Cat
import Control.Category ((.), (<<<))
import Control.Arrow ((&&&), (^<<))
import Control.Applicative (liftA2, (<$>))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Memory as Memory

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (D4)
import Type.Base.Proxy (Proxy)

import qualified Number.Complex as Complex
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified System.Random as Rnd
import Data.Word (Word32)

import qualified Test.QuickCheck as QC

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.))


type SimFloat = CheckSimilarity Float
type SimStateFloat = CheckSimilarityState Float
type VectorValue = Serial.Value D4 Float

signalLength :: Int
signalLength = 10000


limitFloat :: SVL.Vector Float -> SVL.Vector Float
limitFloat = SVL.take signalLength

{-
limitStereoFloat :: SVL.Vector (Stereo.T Float) -> SVL.Vector (Stereo.T Float)
limitStereoFloat = SVL.take signalLength
-}


lfoSine ::
   (Memory.C a, Expr.Aggregate ae a) =>
   (Exp Float -> ae) ->
   Exp Float ->
   Sig.T a
lfoSine f reduct =
   Sig.interpolateConstant reduct $
   (Causal.map f . Causal.mapExponential 2 0.01 $*
      Sig.osci Wave.sine 0 (reduct * (0.1/44100)))

allpassControl ::
   (TypeNum.Natural n) =>
   Proxy n ->
   Exp Float ->
   Sig.T (Allpass.CascadeParameter n (MultiValue.T Float))
allpassControl order =
   lfoSine (Allpass.flangerParameter order)

allpassPhaserCausal, allpassPhaserPipeline ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (MultiValue.T Float)
allpassPhaserCausal reduct xs =
   Allpass.phaser
      $< allpassControl TypeNum.d16 reduct
      $* xs

allpassPhaserPipeline reduct xs =
   let order = TypeNum.d16
   in  (Sig.drop (TypeNum.integralFromProxy order)) $
       (Allpass.phaserPipeline
         $< allpassControl order reduct
         $* xs)


genOsci :: QC.Gen (Float, Float)
genOsci = pair (Gen.choose (0.001, 0.01)) (Gen.choose (0, 0.99))

genOsciReduct :: QC.Gen ((Float, Float), Float)
genOsciReduct = pair genOsci (Gen.choose (10, 100))

genOsciReductPacked :: QC.Gen ((Float, Float), Float)
genOsciReductPacked = pair genOsci (arg $ (4*) <$> QC.choose (1, 25))

allpassPipeline :: Gen.Test ((Float,Float), Float) SimFloat
allpassPipeline =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci Wave.triangle phase freq
   in checkSimilarity 1e-2 limitFloat
         (\(freqPhase, reduct) ->
            allpassPhaserCausal reduct $ tone freqPhase)
         (\(freqPhase, reduct) ->
            allpassPhaserPipeline reduct $ tone freqPhase)



{- |
Shrink control signal in time
since we can only handle one control parameter per vector chunk.
-}
applyPacked ::
   (Memory.C c) =>
   Causal.T (c, VectorValue) VectorValue ->
   Sig.T c ->
   Sig.T VectorValue ->
   Sig.T VectorValue
applyPacked proc cs xs =
   proc
      $< Sig.interpolateConstant
            (recip $ TypeNum.integralFromProxy TypeNum.d4 :: Exp Float) cs
      $* xs


allpassPhaserPacked ::
   Exp Float ->
   Sig.T VectorValue ->
   Sig.T VectorValue
allpassPhaserPacked reduct =
   applyPacked Allpass.phaserPacked
      (allpassControl TypeNum.d16 reduct)

allpassPacked :: Gen.Test ((Float,Float), Float) SimFloat
allpassPacked =
   withGenArgs genOsciReductPacked $
   let tone  (freq,phase) = Sig.osci  Wave.triangle phase freq
       toneP (freq,phase) = SigPS.osci Wave.triangle phase freq
   in  checkSimilarityPacked 1e-2 limitFloat
          (\(freqPhase, reduct) -> allpassPhaserCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) -> allpassPhaserPacked reduct $ toneP freqPhase)


interpolateConstant :: Float -> SigS.T a -> SigS.T a
interpolateConstant reduct xs =
   CausalS.apply (InterpC.relative Ip.constant 0 xs) $
   SigS.repeat $ recip reduct


{-# INLINE lfoSineCore #-}
lfoSineCore ::
   (Float -> a) ->
   Float ->
   SigS.T a
lfoSineCore f reduct =
   interpolateConstant reduct $
   SigS.map f $
   DispS.mapExponential 2 0.01 $
   OsciS.static WaveCore.sine zero (reduct * 0.1/44100)

{-# INLINE allpassPhaserCore #-}
allpassPhaserCore ::
   Float ->
   SigS.T Float ->
   SigS.T Float
allpassPhaserCore reduct =
   let order = 16
   in  CausalS.apply $
       FiltC.amplify 0.5 <<<
       DispC.mix <<<
          ((CausalS.applyFst (AllpassCore.cascadeCausal order) $
            lfoSineCore (AllpassCore.flangerParameter order) reduct)
           &&&
           Cat.id)

allpassCore :: Gen.Test ((Float,Float), Float) SimStateFloat
allpassCore =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci Wave.triangle phase freq
       toneS (freq,phase) =
          OsciS.static WaveCore.triangle
             (Phase.fromRepresentative phase) freq
   in  checkSimilarityState 1e-2 limitFloat
          (\(freqPhase, reduct) -> allpassPhaserCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) -> allpassPhaserCore reduct $ toneS freqPhase)



diracImpulse :: Sig.T (MultiValue.T Float)
diracImpulse = Causal.delay1 one $* Sig.constant zero

firstOrderConstant ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (MultiValue.T Float)
firstOrderConstant cutOff xs =
   FirstOrder.lowpassCausal
    $< Sig.constant (FirstOrderCore.parameter cutOff)
    $* xs

firstOrderExponential :: Gen.Test Float SimFloat
firstOrderExponential =
   withGenArgs (Gen.choose (0.001, 0.01)) $
   let gain cutOff = exp(-2*pi*cutOff)
   in  checkSimilarity 1e-2 limitFloat
          (\cutOff ->
             Causal.amplify (recip (1 - gain cutOff)) $*
             firstOrderConstant cutOff diracImpulse)
          (\cutOff -> Core.exponential (gain cutOff) one)

firstOrderCausal ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (MultiValue.T Float)
firstOrderCausal reduct xs =
   FirstOrder.lowpassCausal
    $< lfoSine FirstOrder.parameter reduct
    $* xs

{-# INLINE firstOrderCore #-}
firstOrderCore ::
   Float ->
   SigS.T Float ->
   SigS.T Float
firstOrderCore reduct =
   CausalS.apply $
      CausalS.applyFst FirstOrderCore.lowpassCausal $
      lfoSineCore FirstOrderCore.parameter reduct

firstOrder :: Gen.Test ((Float,Float), Float) SimStateFloat
firstOrder =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci Wave.triangle phase freq
       toneS (freq,phase) =
          OsciS.static WaveCore.triangle
             (Phase.fromRepresentative phase) freq
   in  checkSimilarityState 1e-2 limitFloat
          (\(freqPhase, reduct) -> firstOrderCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) -> firstOrderCore reduct $ toneS freqPhase)

firstOrderCausalPacked ::
   Exp Float ->
   Sig.T VectorValue ->
   Sig.T VectorValue
firstOrderCausalPacked reduct =
   applyPacked
      FirstOrder.lowpassCausalPacked
      (lfoSine FirstOrder.parameter reduct)

firstOrderPacked :: Gen.Test ((Float,Float), Float) SimFloat
firstOrderPacked =
   withGenArgs genOsciReductPacked $
   let tone  (freq,phase) = Sig.osci  Wave.triangle phase freq
       toneP (freq,phase) = SigPS.osci Wave.triangle phase freq
   in  checkSimilarityPacked 1e-2 limitFloat
          (\(freqPhase, reduct) ->
             firstOrderCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) ->
             firstOrderCausalPacked reduct $ toneP freqPhase)


secondOrderCausal ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (MultiValue.T Float)
secondOrderCausal reduct xs =
   SecondOrder.causal
    $< lfoSine (SecondOrder.bandpassParameter 10) reduct
    $* xs

secondOrderCausalPacked ::
   Exp Float ->
   Sig.T VectorValue ->
   Sig.T VectorValue
secondOrderCausalPacked reduct =
   applyPacked SecondOrder.causalPacked
      (lfoSine (SecondOrder.bandpassParameter 10) reduct)

secondOrderPacked :: Gen.Test ((Float,Float), Float) SimFloat
secondOrderPacked =
   withGenArgs genOsciReductPacked $
   let tone  (freq,phase) = Sig.osci  Wave.triangle phase freq
       toneP (freq,phase) = SigPS.osci Wave.triangle phase freq
   in  checkSimilarityPacked 1e-2 limitFloat
          (\(freqPhase, reduct) ->
             secondOrderCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) ->
             secondOrderCausalPacked reduct $ toneP freqPhase)

secondOrderCausalPacked2 ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (MultiValue.T Float)
secondOrderCausalPacked2 reduct xs =
   SecondOrderP.causal
    $< lfoSine (SecondOrderP.bandpassParameter 10) reduct
    $* xs

secondOrderPacked2 :: Gen.Test ((Float,Float), Float) SimFloat
secondOrderPacked2 =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci  Wave.triangle phase freq
   in  checkSimilarity 1e-2 limitFloat
          (\(freqPhase, reduct) ->
             secondOrderCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) ->
             secondOrderCausalPacked2 reduct $ tone freqPhase)


{-
limitUniFilter ::
   SVL.Vector (UniFilterCore.Result Float) ->
   SVL.Vector (UniFilterCore.Result Float)
limitUniFilter = SVL.take signalLength
-}

universalCausal ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (UniFilter.Result (MultiValue.T Float))
universalCausal reduct xs =
   UniFilter.causal
    $< lfoSine (UniFilter.parameter 10) reduct
    $* xs

{-# INLINE universalCore #-}
universalCore ::
   Float ->
   SigS.T Float ->
   SigS.T (UniFilterCore.Result Float)
universalCore reduct =
   CausalS.apply $
      CausalS.applyFst UniFilterCore.causal $
      lfoSineCore (UniFilterCore.parameter . Pole 10) reduct

universal :: Gen.Test ((Float,Float), Float) SimStateFloat
universal =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci Wave.triangle phase freq
       toneS (freq,phase) =
          OsciS.static WaveCore.triangle
             (Phase.fromRepresentative phase) freq
   in  checkSimilarityState 1e-2 limitFloat
          (\(freqPhase, reduct) ->
             fmap UniFilter.lowpass $
             universalCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) ->
             SigS.map UniFilterCore.lowpass $
             universalCore reduct $ toneS freqPhase)
{-
       checkSimilarityState 1e-2 limitUniFilter
          (universalCausal reduct tone)
          (\p -> universalCore (Param.get reduct p) (toneS p))
-}


moogCausal ::
   (TypeNum.Natural n) =>
   Proxy n ->
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (MultiValue.T Float)
moogCausal order reduct xs =
   Moog.causal
    $< lfoSine (Moog.parameter order 10) reduct
    $* xs

{-# INLINE moogCore #-}
moogCore ::
   Int ->
   Float ->
   SigS.T Float ->
   SigS.T Float
moogCore order reduct =
   CausalS.apply $
      CausalS.applyFst (MoogCore.lowpassCausal order) $
      lfoSineCore (MoogCore.parameter order . Pole 10) reduct

moog :: Gen.Test ((Float,Float), Float) SimStateFloat
moog =
   withGenArgs genOsciReduct $
   let order = TypeNum.d6
       tone  (freq,phase) = Sig.osci Wave.triangle phase freq
       toneS (freq,phase) =
          OsciS.static WaveCore.triangle
             (Phase.fromRepresentative phase) freq
   in  checkSimilarityState 1e-2 limitFloat
         (\(freqPhase, reduct) ->
            moogCausal order reduct $ tone freqPhase)
         (\(freqPhase, reduct) ->
            moogCore (TypeNum.integralFromProxy order) reduct $
            toneS freqPhase)


complexCausal ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (Stereo.T (MultiValue.T Float))
complexCausal reduct xs =
   ComplexFilter.causal
      $< lfoSine (ComplexFilter.parameter 10) reduct
      $* ((\x -> Stereo.cons x A.zero) <$> xs)

complexCausalPacked ::
   Exp Float ->
   Sig.T (MultiValue.T Float) ->
   Sig.T (Stereo.T (MultiValue.T Float))
complexCausalPacked reduct xs =
   ComplexFilterP.causal
      $< lfoSine (ComplexFilterP.parameter 10) reduct
      $* ((\x -> Stereo.cons x A.zero) <$> xs)

complexPacked :: Gen.Test ((Float,Float), Float) SimFloat
complexPacked =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci Wave.triangle phase freq
   in  checkSimilarity 1e-2 limitFloat
          (\(freqPhase, reduct) ->
             fmap Stereo.left $
             complexCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) ->
             fmap Stereo.left $
             complexCausalPacked reduct $ tone freqPhase)

{-# INLINE complexCore #-}
complexCore ::
   Float ->
   SigS.T Float ->
   SigS.T (Stereo.T Float)
complexCore reduct =
   CausalS.apply $
   (\x -> Stereo.cons (Complex.real x) (Complex.imag x)) ^<<
   CausalS.applyFst ComplexFilterCore.causal
      (lfoSineCore (ComplexFilterCore.parameter . Pole 10) reduct)

complex :: Gen.Test ((Float,Float), Float) SimStateFloat
complex =
   withGenArgs genOsciReduct $
   let tone (freq,phase) = Sig.osci Wave.triangle phase freq
       toneS (freq,phase) =
          OsciS.static WaveCore.triangle
             (Phase.fromRepresentative phase) freq
   in  checkSimilarityState 1e-2 limitFloat
          (\(freqPhase, reduct) ->
             fmap Stereo.left $
             complexCausal reduct $ tone freqPhase)
          (\(freqPhase, reduct) ->
             SigS.map ((0.1*) . Stereo.left) $
             complexCore reduct $ toneS freqPhase)
{-
   in  checkSimilarityState 1e-2 limitStereoFloat
          (complexCausal reduct tone)
          (\p -> complexCore (Param.get reduct p) (toneS p))
-}


convolvePacked :: Gen.Test ((Int,Rnd.StdGen), Word32) SimFloat
convolvePacked =
   withGenArgs
      (pair
         (arg $ liftA2 (,) (QC.choose (1,20)) (Rnd.mkStdGen <$> QC.arbitrary))
         Gen.arbitrary)
   $
   fmap
      (\f chunkSize (rnd, seed) ->
         f chunkSize
            (Render.buffer $ randomStorableVector (-1,1::Float) rnd, seed))
   $
   checkSimilarityPacked 1e-3 limitFloat
      (\(mask, seed) -> FiltNR.convolve mask $* Sig.noise seed 1)
      (\(mask, seed) -> FiltNR.convolvePacked mask $* SigPS.noise seed 1)


tests :: [(String, IO QC.Property)]
tests =
   ("secondOrderPacked", checkWithParam secondOrderPacked) :
   ("secondOrderPacked2", checkWithParam secondOrderPacked2) :
   ("firstOrderExponential", checkWithParam firstOrderExponential) :
   ("firstOrder", checkWithParam firstOrder) :
   ("firstOrderPacked", checkWithParam firstOrderPacked) :
   ("universal", checkWithParam universal) :
   ("allpassPacked", checkWithParam allpassPacked) :
   ("allpassPipeline", checkWithParam allpassPipeline) :
   ("allpassCore", checkWithParam allpassCore) :
   ("moog", checkWithParam moog) :
   ("complexPacked", checkWithParam complexPacked) :
   ("complex", checkWithParam complex) :
   ("convolvePacked", checkWithParam convolvePacked) :
   []
