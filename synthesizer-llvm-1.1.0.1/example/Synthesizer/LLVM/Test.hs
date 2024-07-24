{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Synthesizer.LLVM.LAC2011 ()
import Synthesizer.LLVM.ExampleUtility

import qualified Synthesizer.LLVM.Server.Default as Default
import qualified Synthesizer.LLVM.Server.SampledSound as Sample

import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked as BandPass
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.Butterworth as Butterworth
import qualified Synthesizer.LLVM.Filter.Chebyshev as Chebyshev
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Filter.SecondOrderPacked as Filt2P
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter
import qualified Synthesizer.LLVM.Filter.NonRecursive as FiltNR
import qualified Synthesizer.LLVM.Causal.Helix as Helix
import qualified Synthesizer.LLVM.Causal.ControlledPacked as CtrlPS
import qualified Synthesizer.LLVM.Causal.Controlled as Ctrl
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Core as SigCore
import qualified Synthesizer.LLVM.Generator.Source as Source
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.ConstantPiece as Const
import qualified Synthesizer.LLVM.Wave as Wave
import Synthesizer.LLVM.Causal.Functional (($&), (&|&))
import Synthesizer.LLVM.Causal.Process (($<), ($>), ($*), ($<#), ($*#))

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial

import qualified LLVM.DSL.Expression.Maybe as ExprMaybe
import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp, (>*), (&&*))

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Vector.Instance as MultiVectorI
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Maybe as Maybe

import qualified LLVM.Core as LLVM
import LLVM.Util.Arithmetic () -- Floating instance for TValue

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (D2, D4, (:*:))
import Type.Base.Proxy (Proxy)

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Causal.Class as CausalClass
import qualified Synthesizer.Zip as Zip
import qualified Synthesizer.State.Control as CtrlS
import qualified Synthesizer.State.Signal as SigS

import qualified Synthesizer.Plain.Filter.Recursive as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1Core

import Control.Arrow (Arrow, arr, first, (&&&), (^<<), (<<^), (***))
import Control.Category ((<<<), (.), id)
import Control.Applicative (liftA2)
import Control.Functor.HT (void)
import Control.Monad (when, join)

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Sound.Sox.Option.Format as SoxOption
import qualified Sound.Sox.Play as SoxPlay
-- import qualified Synthesizer.ALSA.Storable.Play as Play

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Data.Function.HT (nest)
import Data.NonEmpty ((!:))
import Data.Semigroup ((<>))
import Data.Traversable (sequenceA)
import Data.Tuple.HT (mapSnd)
import System.Path ((</>))
import System.Random (randomRs, mkStdGen)

import qualified System.Unsafe as Unsafe
import qualified System.IO as IO
import Control.Exception (bracket)

import qualified Algebra.Field as Field

import qualified NumericPrelude.Numeric as NP
import qualified Prelude as P
import NumericPrelude.Numeric (fromIntegral, sum, (+), (-), (/), (*))
import Prelude hiding (fst, snd, id, (.), fromIntegral, sum, (+), (-), (/), (*))


asStereoPacked :: Id (vector (Serial.T D4 (Stereo.T Float)))
asStereoPacked = id

asStereoInterleaved :: Id (vector (StereoInt.T D4 Float))
asStereoInterleaved = id


{- |
> playStereo (Sig.amplifyStereo 0.3 $ stereoOsciSaw 0.01)
-}
playStereo :: Sig.T (Stereo.T (MultiValue.T Float)) -> IO ()
playStereo sig =
   playStereoVector . ($ SVL.chunkSize 100000) =<<
   Render.run (Stereo.multiValue <$> sig)

playStereoVector :: SVL.Vector (Stereo.T Float) -> IO ()
playStereoVector =
   void . SoxPlay.simple SVL.hPut SoxOption.none 44100

playMono :: Sig.MV Float -> IO ()
playMono sig  =  playMonoVector . ($ SVL.chunkSize 100000) =<< Render.run sig

playMonoVector :: SVL.Vector Float -> IO ()
playMonoVector =
   void . SoxPlay.simple SVL.hPut SoxOption.none 44100


playFileMono :: FilePath -> IO ()
playFileMono fileName = do
   f <- Render.run id
   IO.withFile fileName IO.ReadMode $ \h ->
      playStereoVector . f (SVL.chunkSize 100000) .
      asStereo . snd
       =<< SVL.hGetContentsAsync (SVL.chunkSize 4321) h


frequency :: Float -> Exp Float
frequency = Expr.cons

{- |
Assist GHC-7.10.3 with determining the type of causal processes.
GHC-7.8.4 and GHC-8.0.1 do not need it.
-}
causalP :: Causal.T a b -> Causal.T a b
causalP = id


constant :: Float -> IO ()
constant y =
   (SV.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ 1000) $ Render.run $
   Sig.constant (Expr.cons y)

saw :: IO ()
saw =
   (SV.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ 10000000) $ Render.run $
   Sig.osci Wave.saw 0 0.01

exponential :: IO ()
exponential =
   (SV.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ 10000000) $ Render.run $
   Sig.exponential2 50000 1

triangle :: IO ()
triangle =
   (SV.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ 10000000) $ Render.run $
   Sig.osci Wave.triangle 0.25 0.01

trianglePack :: IO ()
trianglePack =
   (SV.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap ($ div 10000000 4) $ Render.run $
   (Causal.map (Expr.liftM Wave.triangle) $*) $
   SigPS.packSmall $
   SigCore.osci 0.25 (4.015803e-4)

trianglePacked :: IO ()
trianglePacked =
   (SV.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap ($ div 10000000 4) $ Render.run $
   (CausalPS.osci Wave.triangle
     $< SigPS.constant 0.25
     $* SigPS.constant 0.01)

triangleReplicate :: IO ()
triangleReplicate =
   (SV.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap ($ div 10000000 4) $ Render.run $
   (CausalPS.shapeModOsci
       (\k p -> do
           x <- Wave.triangle =<< Wave.replicate k p
           y <- Wave.approxSine4 =<< Wave.halfEnvelope p
           A.mul x y)
     $< SigPS.rampInf 1000000
     $< SigPS.constant 0
     $* SigPS.constant 0.01)

rationalSine :: IO ()
rationalSine =
   (SV.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap ($ div 10000000 4) $ Render.run $
   (CausalPS.shapeModOsci Wave.rationalApproxSine1
     $< (0.001 + SigPS.rampInf 10000000)
     $< SigPS.constant 0
     $* SigPS.constant 0.01)

rationalSineStereo :: IO ()
rationalSineStereo =
   (SV.writeFile "speedtest.f32" . asStereoPacked =<<) $
   fmap ($ div 10000000 4) $ Render.run $
   fmap Stereo.multiValueSerial $
   liftA2 Stereo.cons
      (CausalPS.shapeModOsci Wave.rationalApproxSine1
        $< (0.001 + SigPS.rampInf 10000000)
        $< SigPS.constant (-0.25)
        $* SigPS.constant 0.00999)
      (CausalPS.shapeModOsci Wave.rationalApproxSine1
        $< (0.001 + SigPS.rampInf 10000000)
        $< SigPS.constant 0.25
        $* SigPS.constant 0.01001)


pingSig :: Float -> Sig.MV Float
pingSig freq =
   Sig.exponential2 50000 1
   *
   Sig.osci Wave.saw 0.5 (Expr.cons freq)

pingSigP :: Exp Float -> Sig.MV Float
pingSigP freq =
   Sig.exponential2 50000 1
   *
   Sig.osci Wave.saw 0.5 freq

ping :: IO ()
ping =
   (SV.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ 10000000) $ Render.run $
   pingSig 0.01

pingSigPacked :: Exp Float -> Sig.T (CausalPS.Serial D4 Float)
pingSigPacked freq =
   SigPS.exponential2 50000 1
   *
   SigPS.osci Wave.saw 0 freq

pingPacked :: IO ()
pingPacked =
   (SV.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap (\f -> f (div 10000000 4) (0.01::Float)) $ Render.run $
   pingSigPacked

pingUnpack :: IO ()
pingUnpack =
   (SV.writeFile "speedtest.f32" . asMono =<<) $
   fmap (\f -> f 10000000 (0.01::Float)) $ Render.run $
   SigPS.unpack . pingSigPacked

pingSmooth :: IO ()
pingSmooth =
   SV.writeFile "speedtest-scalar.f32" . asMono . ($ 10000000) =<<
   Render.run
      (Filt1.lowpassCausal
         $< fmap Filt1Core.Parameter (1 - Sig.exponential2 50000 1)
         $* Sig.osci Wave.triangle 0 0.01)

pingSmoothPacked :: IO ()
pingSmoothPacked =
   SV.writeFile "speedtest-vector.f32" . asMonoPacked . ($ div 10000000 4) =<<
   Render.run
      (Filt1.lowpassCausalPacked
         $< fmap Filt1Core.Parameter (1 - Sig.exponential2 (50000/4) 1)
         $* SigPS.osci Wave.triangle 0 0.01)

stereoOsciSaw :: Exp Float -> Sig.T (Stereo.T (MultiValue.T Float))
stereoOsciSaw freq =
   liftA2 Stereo.cons
      (Sig.osci Wave.saw 0.0 (freq*1.001) +
       Sig.osci Wave.saw 0.2 (freq*1.003) +
       Sig.osci Wave.saw 0.1 (freq*0.995))
      (Sig.osci Wave.saw 0.1 (freq*1.005) +
       Sig.osci Wave.saw 0.7 (freq*0.997) +
       Sig.osci Wave.saw 0.5 (freq*0.999))

stereoOsciSawPacked :: Float -> Sig.T (Stereo.T (MultiValue.T Float))
stereoOsciSawPacked freq =
   let mix4 = Expr.liftM $ MultiVector.sum . MultiVectorI.fromMultiValue
   in  liftA2 Stereo.cons
          ((Causal.map mix4 $*) $
           Sig.osci Wave.saw
              (Expr.cons $ LLVM.consVector 0.0 0.2 0.1 0.4)
              (Expr.cons $ fmap (freq*) $
               LLVM.consVector 1.001 1.003 0.995 0.996))
          ((Causal.map mix4 $*) $
           Sig.osci Wave.saw
              (Expr.cons $ LLVM.consVector 0.1 0.7 0.5 0.7)
              (Expr.cons $ fmap (freq*) $
               LLVM.consVector 1.005 0.997 0.999 1.001))

stereoDeinterleave :: NonEmpty.T [] a -> NonEmpty.T [] (Stereo.T a)
stereoDeinterleave xt =
   case xt of
      NonEmpty.Cons _ [] -> error "stereoDeinterleave: singleton"
      NonEmpty.Cons x0 (x1:xs) ->
         Stereo.cons x0 x1 !:
            let go (y0:y1:ys) = Stereo.cons y0 y1 : go ys
                go [] = []
                go [_] = error "stereoDeinterleave: odd length"
            in go xs

mixVectorToStereo ::
   (TypeNum.Positive n, MultiVector.Additive a) =>
   MultiVector.T n a -> LLVM.CodeGenFunction r (Stereo.T (MultiValue.T a))
mixVectorToStereo =
   NonEmpty.foldBalanced (\x y -> join $ liftA2 A.add x y) .
   fmap sequenceA . stereoDeinterleave . MultiVector.dissectList1

mixVec ::
   (TypeNum.Positive n, MultiVector.Additive a) =>
   Exp (LLVM.Vector n a) -> Stereo.T (Exp a)
mixVec =
   Stereo.unExpression .
   Expr.liftM
      (fmap Stereo.multiValue . mixVectorToStereo . MultiVectorI.fromMultiValue)

stereoOsciSawPacked2 :: Float -> Sig.T (Stereo.T (MultiValue.T Float))
stereoOsciSawPacked2 freq =
   (Causal.map mixVec $*) $
   Sig.osci (Wave.trapezoidSlope (A.fromRational' 5))
      (Expr.cons $ LLVM.consVector 0.0 0.2 0.1 0.4 0.1 0.7 0.5 0.7)
      (Expr.cons $ fmap (freq*) $
       LLVM.consVector 1.001 1.003 0.995 0.996 1.005 0.997 0.999 1.001)

stereo :: IO ()
stereo =
   SV.writeFile "speedtest.f32" . asStereo .  ($ 10000000) =<<
   Render.run
      (Stereo.multiValue <$> Causal.amplifyStereo 0.25
         $* stereoOsciSawPacked2 0.01)

lazy :: IO ()
lazy =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run {- SVL.defaultChunkSize - too slow -}
      (Causal.envelope
         $< Sig.exponential2 50000 1
         $* Sig.osci Wave.sine 0.5 0.01)

lazyStereo :: IO ()
lazyStereo =
   (SVL.writeFile "speedtest.f32" . asStereo . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (Stereo.multiValue <$> Causal.amplifyStereo 0.25
         $* stereoOsciSawPacked 0.01)

packTake :: IO ()
packTake =
   (SVL.writeFile "speedtest.f32" . asMonoPacked . ($ SVL.chunkSize 1000) =<<) $
   (Render.run . SigPS.packRotate)
      (Causal.take 5 $* Sig.osci Wave.saw 0 (frequency 0.01))

chord :: Float -> Sig.T (Stereo.T (MultiValue.T Float))
chord base =
   {-
   This exceeds available vector registers
   and thus needs more stack accesses.
   Thus it needs twice as much time as the simple mixing.
   However doing all 32 oscillators in parallel
   and mix them in one go might be still faster.

   foldl1 (Sig.zipWith Frame.mixStereoV) $
   -}
   NonEmpty.foldBalanced (+) $
   fmap (\f -> stereoOsciSawPacked2 (base*f)) $
   0.25 !: 1.00 : 1.25 : 1.50 : []

lazyChord :: IO ()
lazyChord =
   (SVL.writeFile "speedtest.f32" . asStereo . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run (Stereo.multiValue <$> Causal.amplifyStereo 0.1 $* chord 0.005)

filterSweepComplex :: IO ()
filterSweepComplex =
   playStereo $
      (Causal.amplifyStereo 0.3 . BandPass.causal
         $< (Causal.map (\x -> BandPass.parameter 100 (0.01 * exp (2*x))) $*
             Sig.osci Wave.sine 0 (0.1/44100))
         $* chord 0.005)

lfoSineCausal ::
   Causal.T (MultiValue.T Float) a -> Exp Float -> Sig.T a
lfoSineCausal f reduct =
   f . Causal.map (\x -> 0.01 * exp (2*x)) $*
   Sig.osci Wave.sine 0 (reduct * 0.1/44100)

lfoSine ::
   (Expr.Aggregate ae a) =>
   (Exp Float -> ae) ->
   Exp Float -> Sig.T a
lfoSine f = lfoSineCausal (Causal.map f)

filterSweep :: IO ()
filterSweep =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 10000) $
   Render.run $
      (0.2 * Ctrl.processCtrlRate 128 (lfoSine (Filt2.bandpassParameter 100))
         $* Sig.osci Wave.saw 0 (frequency 0.01))

filterSweepPacked :: IO ()
filterSweepPacked =
   (SVL.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap (SVL.take (div 10000000 4)) $
   fmap ($ SVL.chunkSize 10000) $
   Render.run
      (0.2 *
       CtrlPS.processCtrlRate 128 (lfoSine (Filt2.bandpassParameter 100))
            $* SigPS.osci Wave.saw 0 (frequency 0.01))

exponentialFilter2Packed :: IO ()
exponentialFilter2Packed =
   (SVL.writeFile "speedtest.f32" . asMonoPacked16 =<<) $
   fmap (SVL.take (div 10000000 16)) $
   fmap ($ SVL.chunkSize 10000) $
   Render.run
      (Filt2.causalPacked
         $< Sig.constant (Filt2.Parameter 1 0 0   0 0.99)
         $* (
--             (Causal.delay1 $ Serial.fromFixedList (0.1 !: 0.01 !: 0.001 !: 0.0001 !: Empty.Cons))
--             (Causal.delay1 $ Serial.replicate 1)
             (Causal.delay1 $ Serial.fromFixedList (1 !: NonEmptyC.repeat 0))
               $* 0))

filterSweepPacked2 :: IO ()
filterSweepPacked2 =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 10000) $
   Render.run
      (0.2 *
       Ctrl.processCtrlRate 128 (lfoSine (Filt2P.bandpassParameter 100))
         $* Sig.osci Wave.saw 0 (frequency 0.01))

butterworthNoisePacked :: IO ()
butterworthNoisePacked =
   (SVL.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap (SVL.take (div 10000000 4)) $
   fmap ($ SVL.chunkSize 10000) $
   Render.run
      (CausalPS.amplify 0.2 .
       CtrlPS.processCtrlRate 128
         (lfoSineCausal
            (Butterworth.parameterCausal TypeNum.d3 FiltR.Lowpass $<# 0.5))
         $* SigPS.noise 0 0.3)

chebyshevNoisePacked :: IO ()
chebyshevNoisePacked =
   (SVL.writeFile "speedtest.f32" . asMonoPacked =<<) $
   fmap (SVL.take (div 10000000 4)) $
   fmap ($ SVL.chunkSize 10000) $
   Render.run
      (CausalPS.amplify 0.2 .
       CtrlPS.processCtrlRate 128
         (lfoSineCausal
            (Chebyshev.parameterCausalA TypeNum.d5 FiltR.Lowpass $<# 0.5))
         $* SigPS.noise 0 0.3)


upsample :: IO ()
upsample =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (let reduct = 128 :: Exp Float
       in Sig.interpolateConstant reduct
            (Sig.osci Wave.sine 0 (reduct*0.1/44100)))


filterSweepControlRateCausal ::
   Causal.T
      (Stereo.T (MultiValue.T Float))
      (Stereo.T (MultiValue.T Float))
filterSweepControlRateCausal =
   Causal.amplifyStereo 0.3 <<< BandPass.causal
   $< (let reduct = 128 :: Exp Float
       in Sig.interpolateConstant reduct
            (Causal.map (BandPass.parameter 100) .
             Causal.map (\x -> 0.01 * exp (2*x))
               $* Sig.osci Wave.sine 0 (reduct*0.1/44100)))

{- |
Trigonometric functions are very slow in LLVM
because they are translated to calls to C's math library.
Thus it is advantageous to compute filter parameters
at a lower rate and interpolate constantly.
-}
filterSweepControlRate :: IO ()
filterSweepControlRate =
   (SVL.writeFile "speedtest.f32" . asStereo . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (Stereo.multiValue <$> filterSweepControlRateCausal $* chord 0.005)


filterSweepMusic :: IO ()
filterSweepMusic = do
   proc <-
      Render.run $ \music ->
         Stereo.multiValue ^<< Causal.amplifyStereo 20 .
            filterSweepControlRateCausal <<^ Stereo.unMultiValue $* music
   music <- SV.readFile "lichter.f32"
   SVL.writeFile "speedtest.f32" . asStereo
      =<< proc (SVL.chunkSize 100000) music


playFilterSweepMusicLazy :: IO ()
playFilterSweepMusicLazy = do
   proc <-
      Render.run $ \vol music ->
         Stereo.multiValue ^<< Causal.amplifyStereo vol .
            filterSweepControlRateCausal <<^ Stereo.unMultiValue $* music
   IO.withFile "lichter.f32" IO.ReadMode $ \h ->
      playStereoVector . proc (SVL.chunkSize 100000) (20::Float) {-1.125-} . snd
         =<< SVL.hGetContentsAsync (SVL.chunkSize 4321) h

playFilterSweepMusicCausal :: IO ()
playFilterSweepMusicCausal = do
   proc <-
      CausalRender.run $
         Stereo.multiValue ^<< Causal.amplifyStereo 20 .
            filterSweepControlRateCausal <<^ Stereo.unMultiValue
   music <- SV.readFile "lichter.f32"
   void $ SoxPlay.simple SV.hPut SoxOption.none 44100 =<<
      pioApplyStrict proc music

playFilterSweepMusicCausalLazy :: IO ()
playFilterSweepMusicCausalLazy = do
   proc <-
      CausalRender.run $
         Stereo.multiValue ^<< Causal.amplifyStereo 20 .
            filterSweepControlRateCausal <<^ Stereo.unMultiValue
   IO.withFile "lichter.f32" IO.ReadMode $ \h ->
      playStereoVector =<< pioApply proc . snd
       =<< SVL.hGetContentsAsync (SVL.chunkSize 43210) h


deinterleaveProc ::
   IO (Float ->
       PIO.T
         (SV.Vector (StereoInt.T D4 Float))
         (Zip.T
            (SV.Vector (StereoInt.T D4 Float))
            (SV.Vector (StereoInt.T D4 Float))))
deinterleaveProc =
   CausalRender.run deinterleaveCausal

deinterleaveCausal ::
   Exp Float ->
   Causal.T
      (StereoInt.Value D4 Float)
      (StereoInt.Value D4 Float, StereoInt.Value D4 Float)
deinterleaveCausal freq =
   Func.withArgs $ \input ->
      let env =
             Func.fromSignal $
                0.5 * (1 + SigPS.osci (Wave.triangleSquarePower 4) 0 freq)
      in  (Causal.zipWith StereoInt.envelope $& env &|& input)
          &|&
          (Causal.zipWith StereoInt.envelope $& (1-env) &|& input)

deinterleave :: IO ()
deinterleave = do
   proc <- deinterleaveProc
   runSplitProcess (proc (2/44100))


disturbProc, disturbFMProc ::
   IO (PIO.T
         (SV.Vector (StereoInt.T D4 Float))
         (Zip.T
            (SV.Vector (StereoInt.T D4 Float))
            (SV.Vector (StereoInt.T D4 Float))))
disturbProc =
   CausalRender.run $ crossMix disturbCausal

disturbCausal, disturbFMCausal ::
   Causal.T (StereoInt.Value D4 Float) (StereoInt.Value D4 Float)
disturbCausal =
   Func.withArgs $ \inputInt ->
      let tone = Func.fromSignal $ SigPS.osci Wave.triangle 0 (440/44100)
          getEnvelope x =
             Filt1.lowpassCausalPacked $&
                (Func.fromSignal $
                 Sig.constant $ Filt1Core.parameter (1/44100))
                &|&
                (Causal.map abs $& x)
          envelopedTone x = getEnvelope x * tone
      in  Causal.map StereoInt.interleave $&
          CausalPS.amplifyStereo 5 $&
          Stereo.liftApplicative envelopedTone
             (Causal.map StereoInt.deinterleave $& inputInt)

disturbFMProc =
   CausalRender.run $ crossMix disturbFMCausal

disturbFMCausal =
   Func.withArgs $ \inputInt ->
      let getEnvelope x =
             Filt1.lowpassCausalPacked $&
                (Func.fromSignal $
                 Sig.constant $ Filt1Core.parameter (1/44100))
                &|&
                (Causal.map abs $& x)
          modulatedTone x =
             getEnvelope x *
             (CausalPS.osci Wave.triangle $&
                NP.zero
                &|&
                10 * getEnvelope (CausalPS.differentiate 0 $& x))
      in  Causal.map StereoInt.interleave $&
          CausalPS.amplifyStereo 5 $&
          Stereo.liftApplicative modulatedTone
             (Causal.map StereoInt.deinterleave $& inputInt)

disturb :: IO ()
disturb =
   runSplitProcess =<< disturbFMProc


wowFlutterProc ::
   IO (PIO.T
         (SV.Vector (StereoInt.T D4 Float))
         (Zip.T
            (SV.Vector (StereoInt.T D4 Float))
            (SV.Vector (StereoInt.T D4 Float))))
wowFlutterProc =
   CausalRender.run $ crossMix wowFlutterCausal

wowFlutterCausal ::
   Causal.T (StereoInt.Value D4 Float) (StereoInt.Value D4 Float)
wowFlutterCausal =
   Func.withArgs $ \inputInt ->
      let freq =
             Func.fromSignal $ (44100*) $
                0.01 * (1 + SigPS.osci Wave.triangle 0 (1/44100 :: Exp Float)) +
                0.01 * (1 + SigPS.osci Wave.approxSine2
                                                  0 (1.23/44100 :: Exp Float))
          modulatedTone x =
             CausalPS.pack
                (Causal.delayControlledInterpolated Interpolation.linear
                    (0 :: Exp Float) (441*2*2+10))
             $&
             freq &|& x
      in  Causal.map StereoInt.interleave $&
          Stereo.liftApplicative modulatedTone
             (Causal.map StereoInt.deinterleave $& inputInt)

crossMix ::
   Causal.T (StereoInt.Value D4 Float) (StereoInt.Value D4 Float) ->
   Causal.T
      (StereoInt.Value D4 Float)
      (StereoInt.Value D4 Float, StereoInt.Value D4 Float)
crossMix proc =
   ((fst NP.+ snd)  &&&  (fst NP.- snd))
   .
   (id &&& proc)
   .
   Causal.map (StereoInt.amplify 0.5)


wowFlutter :: IO ()
wowFlutter =
   runSplitProcess =<< wowFlutterProc



scrambleProc0, scrambleProc1 ::
   IO (Float ->
       PIO.T
         (SV.Vector (StereoInt.T D4 Float))
         (Zip.T
            (SV.Vector (StereoInt.T D4 Float))
            (SV.Vector (StereoInt.T D4 Float))))
scrambleProc0 =
   CausalRender.run $ \freq ->
      deinterleaveCausal freq NP.+
      (id &&& NP.negate id) .
         Causal.map (StereoInt.amplify 0.5) . wowFlutterCausal

scrambleProc1 =
   CausalRender.run $ \freq ->
      deinterleaveCausal freq NP.+
      (id &&& NP.negate id) .
         Causal.map (StereoInt.amplify 0.3) .
         (wowFlutterCausal NP.+ disturbFMCausal)

scramble :: IO ()
scramble = do
   proc <- scrambleProc1
   runSplitProcess (proc (2/44100))


runSplitProcess ::
   (Storable a) =>
   PIO.T (SV.Vector a) (Zip.T (SV.Vector a) (SV.Vector a)) ->
   IO ()
runSplitProcess proc = do
   void $
      IO.withFile "/tmp/test.f32" IO.ReadMode $ \h ->
      IO.withFile "/tmp/even.f32" IO.WriteMode $ \h0 ->
      IO.withFile "/tmp/odd.f32"  IO.WriteMode $ \h1 ->

      case proc of
         PIO.Cons next create delete ->
            {-
            Is the use of 'bracket' correct?
            I think 'delete' must be called with the final state,
            not with the initial one.
            -}
            bracket create delete $
               let chunkSize = 543210
                   loop s0 = do
                      chunk <- SV.hGet h chunkSize
                      (Zip.Cons y0 y1, s1) <- next chunk s0
                      SV.hPut h0 y0
                      SV.hPut h1 y1
                      when
                         (SV.length y0 >= SV.length chunk &&
                          SV.length y1 >= SV.length chunk &&
                          SV.length chunk >= chunkSize)
                         (loop s1)
               in  loop


antimixProc ::
   IO (SVL.Vector (StereoInt.T D4 Float) ->
       PIO.T
         (SV.Vector (StereoInt.T D4 Float))
         (Zip.T
            (SV.Vector (StereoInt.T D4 Float))
            (SV.Vector (StereoInt.T D4 Float))))
antimixProc =
   CausalRender.run $ \xs -> crossMix $
      Causal.map (StereoInt.amplify 0.5) . Causal.fromSignal xs

antimix :: IO ()
antimix = do
   proc <- antimixProc
   void $
      IO.withFile "/tmp/test.f32" IO.ReadMode $ \h ->
      IO.withFile "/tmp/even.f32" IO.WriteMode $ \h0 ->
      IO.withFile "/tmp/odd.f32"  IO.WriteMode $ \h1 -> do
         let chunkSize = SVL.chunkSize 543210
         input <- fmap snd $ SVL.hGetContentsAsync chunkSize h
         let vectorSize = 4
             additive = SVL.drop (div 44100 vectorSize) input
{-
             additive =
                case SVL.splitAt (div 44100 vectorSize) input of
                   (prefix, suffix) ->
                      SVL.append suffix $
                      SVL.replicate chunkSize (SVL.length prefix) StereoInt.zero
-}
{-
             additive =
                case SVL.splitAt (div 44100 vectorSize) input of
                   (prefix, suffix) -> SVL.append suffix prefix
-}

         case proc additive of
            PIO.Cons next create delete ->
               {-
               Is the use of 'bracket' correct?
               I think 'delete' must be called with the final state,
               not with the initial one.
               -}
               bracket create delete $ \state ->
                  let loop cs0 s0 =
                         case cs0 of
                            [] -> return ()
                            c : cs -> do
                               (Zip.Cons y0 y1, s1) <- next c s0
                               SV.hPut h0 y0
                               SV.hPut h1 y1
                               when
                                  (SV.length y0 >= SV.length c &&
                                   SV.length y1 >= SV.length c)
                                  (loop cs s1)
                  in  loop (SVL.chunks input) state


arrangeLazy :: IO ()
arrangeLazy = do
   IO.hSetBuffering IO.stdout IO.NoBuffering
   arrange <- SigStL.makeArranger
   print $
      arrange (SVL.chunkSize 2) $
      EventList.fromPairList $
         (0, SVL.pack (SVL.chunkSize 2) [1,2::Double]) :
         (0, SVL.pack (SVL.chunkSize 2) [3,4,5,6]) :
         (2, SVL.pack (SVL.chunkSize 2) [7,8,9,10]) :
 --        repeat (2, SVL.empty)
--         (2, SVL.empty) :
--         (2, SVL.empty) :
--         (2::NonNeg.Int, error "undefined sound") :
         error "end of list"
 --        []


{- |
This is inefficient because pingSig is compiled by LLVM
for every occurence of the sound!

randomTones :: IO ()
randomTones = do
   playMonoVector $
      SigStL.arrange (SVL.chunkSize 12345) $
      EventList.fromPairList $ zip
         (cycle $ map (flip div 16 . (44100*)) [1,2,3])
         (cycle $ map (SVL.take 44100 . Sig.renderChunky (SVL.chunkSize 54321) .
                       pingSig . (0.01*))
          [1,1.25,1.5,2])
-}

{-
{- |
So far we have not managed to compile signals
that depend on parameters.
Thus in order to avoid much recompilation,
we compile and render a few sounds in advance.
-}
pingTones :: [SVL.Vector Float]
pingTones =
   map (SVL.take 44100 . Sig.renderChunky (SVL.chunkSize 4321) .
        pingSig . (0.01*))
   [1,1.25,1.5,2]
-}

pingTonesIO :: IO [SVL.Vector Float]
pingTonesIO =
   fmap
      (\pingVec ->
         map
            (SVL.take 44100 .
             pingVec (SVL.chunkSize 4321) .
             (0.01*))
            [1,1.25,1.5,2::Float])
      (Render.run pingSigP)

{-
Arrange itself does not seem to have a space leak with temporary data.
However it may leak sound data.
This is not very likely because this would result in a large memory leak.

Generate random tones in order to see whether generated sounds leak.
How does 'arrange' compare with 'concat'?
-}

{-
cycleTones :: IO ()
cycleTones = do
--   playMono $
   pings <- pingTonesIO
   SVL.writeFile "test.f32" $
--   Play.auto (0.01::Double) 44100 $
      asMono $
{-
after 13min runtime memory consumption increased from 2.5 to 3.9
and we get lot of buffer underruns with this implementation of amplification
(renderChunky . amplify . fromStorableVector)
-}
      Sig.renderChunky (SVL.chunkSize 432109) $
      Sig.amplify 0.1 $
      Sig.fromStorableVectorLazy $
{-
after 20min memory consumption increased from 2.5 to 3.4
and we get lot of buffer underruns with applyStorableChunky
-}
{-
applyStorableChunky applied to concatenated zero vectors
starts with memory consumption 1.0 and after an hour, it's still 1.1
without buffer underruns.
-}
{-
      CausalP.applyStorableChunky (CausalP.amplify $# (0.1::Float)) () $
      asMono $
-}
{-
with chunksize 12345678
after 50min runtime the memory consumption increased from 12.0 to 26.2

with chunksize 123
after 25min runtime the memory consumption is constant 7.4
however at start time there 5 buffer underruns, but no more
probably due to initial LLVM compilation

with chunksize 1234567 and SVL.replicate instead of pingTones
we get memory consumption from 1.3 to 3.2 in 15min,
while producing lots of buffer underruns.
After 45min in total, it is still 3.2 of memory consumption.
Is this a memory leak, or isn't it?

with chunksize 12345678 and SVL.replicate
we get from 5.6 to 10.2 in 3min
to 14.9 after total 13min.
-}
{-
      SigStL.arrange (SVL.chunkSize 12345678) $
      EventList.fromPairList $ zip
         (repeat (div 44100 8))
--         (cycle $ map (flip div 4 . (44100*)) [1,2,3])
-}
{-
With plain concatenation of those zero vectors
we stay constantly at 0.4 memory consumption and no buffer underruns over 30min.
-}
      SVL.concat
         (cycle pings)
--         (repeat $ SVL.replicate (SVL.chunkSize 44100) 44100 0)
   return ()
-}


tonesChunkSize :: SVL.ChunkSize
numTones :: Int

{-
For one-time-compiled fill functions,
larger chunks have no relevant effect on the processing speed.
-}
(tonesChunkSize, numTones) =
   (SVL.chunkSize 441, 200)
--   (SVL.chunkSize 44100, 200)

fst :: Arrow arrow => arrow (a,b) a
fst = arr P.fst

snd :: Arrow arrow => arrow (a,b) b
snd = arr P.snd


{-# NOINLINE makePing #-}
makePing :: IO (Float -> Float -> SVL.Vector Float)
makePing =
   fmap ($ tonesChunkSize) $
   Render.run $ \halfLife freq ->
      Causal.envelope
         $< Sig.exponential2 halfLife 1
         $* Sig.osci Wave.saw 0.5 freq

tonesDown :: IO ()
tonesDown = do
   let dist = div 44100 10
   pingp <- makePing
   arrange <- SigStL.makeArranger
   amplify <- CausalRender.run Causal.amplify
   playMonoVector =<<
      (pioApply (amplify (0.03::Float)) $
       arrange tonesChunkSize $
       EventList.fromPairList $
       zip
         (repeat (NonNeg.fromNumber dist))
         (map (SVL.take (numTones * dist) . pingp 50000) $
          iterate (0.999*) 0.01))


vibes :: (Exp Float, Exp Float) -> Sig.MV Float
vibes (modDepth, freq) =
   let halfLife = 5000
       -- sine = Wave.sine
       sine = Wave.approxSine4
   in Causal.envelope
         $< Sig.exponential2 halfLife 1
         $* (((Causal.osci sine
                $< (Causal.envelope
                       $< Sig.exponential2 halfLife modDepth
                       $* (Causal.osci sine $* Sig.constant (0, 2*freq))))
               <<<
               Causal.amplify freq
               <<<
               (Causal.osci sine * 0.01 + 1))
             $* Sig.constant (0, 0.0001))

makeVibes :: IO ((Float,Float) -> SVL.Vector Float)
makeVibes = fmap ($ tonesChunkSize) $ Render.run vibes

vibesCycleVector :: ((Float,Float) -> SVL.Vector Float) -> IO (SVL.Vector Float)
vibesCycleVector pingp =
   (\evs -> fmap (\arrange -> arrange tonesChunkSize evs) SigStL.makeArranger) $
   EventList.fromPairList $ zip
      (repeat 5000)
      (map (SVL.take 50000 . pingp) $
       zip
          (map (\k -> 0.5 * (1 - cos k)) $ iterate (0.05+) 0)
          (cycle $ map (0.01*) [1, 1.25, 1.5, 2]))

pioApply ::
   (Storable a, Storable b) =>
   PIO.T (SV.Vector a) (SV.Vector b) -> SVL.Vector a -> IO (SVL.Vector b)
pioApply proc sig = do
   act <- PIO.runStorableChunkyCont proc
   return $ act (const SVL.empty) sig

pioApplyStrict ::
   (Storable a, Storable b) =>
   PIO.T (SV.Vector a) (SV.Vector b) -> SV.Vector a -> IO (SV.Vector b)
pioApplyStrict proc sig = do
   act <- PIO.runCont proc
   return $
      case act (const []) [sig] of
         chunk : _ -> chunk
         [] -> SV.empty

vibesCycle :: IO ()
vibesCycle = do
   sig <- vibesCycleVector =<< makeVibes
   proc <- CausalRender.run Causal.amplify
   playMonoVector =<< pioApply (proc (0.2::Float)) sig

vibesEcho :: IO ()
vibesEcho = do
   sig <- vibesCycleVector =<< makeVibes
   proc <-
      CausalRender.run (\vol -> Causal.amplify vol <<< Causal.comb 0.5 7000)
   playMonoVector =<< pioApply (proc (0.2::Float)) sig

vibesReverb :: IO ()
vibesReverb = do
   sig <- vibesCycleVector =<< makeVibes
   proc <-
      CausalRender.run
         (\params -> Causal.amplify 0.3 <<< Causal.reverbExplicit params)
   playMonoVector =<<
      pioApply
         (proc (Causal.reverbParams (mkStdGen 142)
                  TypeNum.d16 (0.9,0.97) (400,1000)))
         sig

vibesReverbStereo :: IO ()
vibesReverbStereo = do
   sig <- vibesCycleVector =<< makeVibes
   proc <-
      CausalRender.run
         (\params ->
            Stereo.multiValue
            ^<<
            Causal.amplifyStereo 0.3
            <<<
            Causal.stereoFromMonoParametric Causal.reverbExplicit params
            <<^
            (\x -> Stereo.cons x x))
   playStereoVector =<<
      pioApply
         (proc
            (fmap
                (\seed ->
                   Causal.reverbParams (mkStdGen seed)
                      TypeNum.d16 (0.9,0.97) (400,1000))
                (Stereo.cons 142 857)))
         sig


stair :: IO ()
stair =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap
      (\f ->
         f tonesChunkSize $
         EventListBT.fromPairList $
         zip (iterate (/2) 1) (iterate (2*) (1::NonNeg.Integer))) $
   Render.run Const.flatten


filterBass :: IO ()
filterBass = do
   proc <-
      Render.run $ \xs ->
         (fmap Stereo.multiValue BandPass.causal
          <<<
          CausalClass.feedSnd
            (liftA2 Stereo.cons
               (Sig.osci Wave.saw 0 (frequency 0.001499))
               (Sig.osci Wave.saw 0 (frequency 0.001501)))
          <<<
          Causal.map (BandPass.parameter 100))
         $*
         Const.flatten xs

   playStereoVector $ proc tonesChunkSize $
      EventListBT.fromPairList $
      zip
         (map (((0.01::Float)*) . (2**) . (/12) . fromInteger) $
          randomRs (0,24) (mkStdGen 998))
         (repeat (6300::NonNeg.Int))


mixVectorStereo ::
   SVL.Vector (Stereo.T Float) ->
   SVL.Vector (Stereo.T Float) ->
   SVL.Vector (Stereo.T Float)
mixVectorStereo = Unsafe.performIO mixVectorStereoIO

mixVectorStereoIO ::
   IO (SVL.Vector (Stereo.T Float) ->
       SVL.Vector (Stereo.T Float) ->
       SVL.Vector (Stereo.T Float))
mixVectorStereoIO =
   (\proc xs ys -> Unsafe.performIO $ pioApply (proc xs) ys)
   <$>
   CausalRender.run (\xs -> Causal.mix $< xs)

{-
slightly slower than mixVectorParam
-}
mixVectorHaskell :: SVL.Vector Float -> SVL.Vector Float -> SVL.Vector Float
mixVectorHaskell = SVL.zipWith (+)

toneMix :: IO ()
toneMix = do
   pingp <- makePing
   mix <- CausalRender.run $ \x -> Causal.mix $< x
   amplify <- CausalRender.run (Causal.amplify 0.1)
   playMonoVector
      =<< pioApply amplify
      =<< ((\(x:xs) -> Fold.foldlM (pioApply . mix) x xs) $ take numTones $
           map (pingp 1000000) $ iterate (*(2/3)) 0.01)

fadeEnvelope :: Exp Word -> Exp Word -> Sig.MV Float
fadeEnvelope intro len =
   Sig.parabolaFadeIn intro
   <>
   (Causal.take len $* 1)
   <>
   Sig.parabolaFadeOut intro

fadeEnvelopeWrite :: IO ()
fadeEnvelopeWrite =
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ SVL.chunkSize 1234) $
   Render.run $ fadeEnvelope 100000 200000


-- | normalize a list of numbers, such that they have a specific average
-- Cf. haskore-supercollider/src/Haskore/Interface/SuperCollider/Example.hs
normalizeLevel :: (Field.C a) => a -> [a] -> [a]
normalizeLevel newAvrg xs =
   let avrg = sum xs / fromIntegral (length xs)
   in  map ((newAvrg-avrg)+) xs

stereoOsciParams ::
   (TypeNum.Integer n) =>
   Proxy n -> Float -> (Float, Stereo.T (MultiValue.Array n (Float,Float)))
stereoOsciParams np freq =
   let n = TypeNum.integralFromProxy np
       volume :: Float
       volume = recip $ sqrt $ TypeNum.integralFromProxy np
       detunes :: [Float]
       detunes =
          normalizeLevel 1 $ take (2*n) $
             randomRs (0,0.03) $ mkStdGen 912
       phases :: [Float]
       phases = randomRs (0,1) $ mkStdGen 54
   in (,) volume $
      fmap MultiValue.Array $
      uncurry Stereo.cons $ splitAt n $
      zipWith
         (\phase detune -> (phase, detune*freq))
         phases detunes

stereoOsciSawP ::
   (TypeNum.Natural n) =>
   (TypeNum.Natural arrSize, arrSize ~ (n :*: LLVM.UnknownSize)) =>
   (TypeNum.Natural stereoSize, stereoSize ~ (D2 :*: arrSize)) =>
   Exp Float -> Stereo.T (Exp (MultiValue.Array n (Float,Float))) ->
   Sig.MV (Stereo.T Float)
stereoOsciSawP volume =
   fmap Stereo.multiValue
   .
   stereoFromMonoParametricSignal
      (\params ->
         Causal.amplify volume
         $* multiMixSignal
               (\phaseFreq ->
                   Sig.osci Wave.saw
                      (Expr.fst phaseFreq)
                      (Expr.snd phaseFreq))
               params)

stereoFromMonoParametricSignal ::
   (Marshal.C x) =>
   (D2 :*: LLVM.SizeOf (Marshal.Struct x) ~ arrSize,
    TypeNum.Natural arrSize) =>
   (Exp x -> Sig.MV Float) ->
   Stereo.T (Exp x) -> Sig.T (Stereo.T (MultiValue.T Float))
stereoFromMonoParametricSignal f ps =
   Causal.toSignal $
      Causal.stereoFromMonoParametric (Causal.fromSignal . f) ps
      <<^
      (\() -> Stereo.cons () ())

multiMixSignal ::
   (TypeNum.Natural n, Marshal.C x,
    n :*: LLVM.SizeOf (Marshal.Struct x) ~ arraySize,
    TypeNum.Natural arraySize,
    Tuple.Undefined a, Tuple.Phi a, A.Additive a) =>
   (Exp x -> Sig.T a) ->
   Exp (MultiValue.Array n x) -> Sig.T a
multiMixSignal f =
   Causal.toSignal . multiMix (Causal.fromSignal . f)

multiMix ::
   (TypeNum.Natural n, Marshal.C x,
    n :*: LLVM.SizeOf (Marshal.Struct x) ~ arraySize,
    TypeNum.Natural arraySize,
    Tuple.Undefined b, Tuple.Phi b, A.Additive b) =>
   (Exp x -> Causal.T a b) ->
   Exp (MultiValue.Array n x) -> Causal.T a b
multiMix f ps =
   Causal.replicateControlledParam (\x -> Causal.mix <<< first (f x)) ps
   <<^
   (\a -> (a, A.zero))

stereoOsciSawVector :: Float -> SVL.Vector (Stereo.T Float)
stereoOsciSawVector freq =
   Unsafe.performIO $
   (\f -> uncurry (f tonesChunkSize) (stereoOsciParams TypeNum.d5 freq))
   <$>
   Render.run stereoOsciSawP

stereoOsciSawChord :: NonEmpty.T [] Float -> SVL.Vector (Stereo.T Float)
stereoOsciSawChord =
   NonEmpty.foldBalanced mixVectorStereo . fmap stereoOsciSawVector

stereoOsciSawPad :: Word -> NonEmpty.T [] Float -> SVL.Vector (Stereo.T Float)
stereoOsciSawPad dur pitches =
   let attack = 20000
   in Unsafe.performIO $
      fmap
         (\f ->
            Unsafe.performIO $
            pioApply (f attack (dur-attack)) (stereoOsciSawChord pitches)) $
      CausalRender.run
         (\intro len ->
            Stereo.multiValue <$>
               (Causal.envelopeStereo $< fadeEnvelope intro len)
                  <<^ Stereo.unMultiValue)

a0, as0, b0, c1, cs1, d1, ds1, e1, f1, fs1, g1, gs1,
 a1, as1, b1, c2, cs2, d2, ds2, e2, f2, fs2, g2, gs2,
 a2, as2, b2, c3, cs3, d3, ds3, e3, f3, fs3, g3, gs3,
 a3, as3, b3, c4, cs4, d4, ds4, e4, f4, fs4, g4, gs4 :: Float
a0 : as0 : b0 : c1 : cs1 : d1 : ds1 : e1 : f1 : fs1 : g1 : gs1 :
 a1 : as1 : b1 : c2 : cs2 : d2 : ds2 : e2 : f2 : fs2 : g2 : gs2 :
 a2 : as2 : b2 : c3 : cs3 : d3 : ds3 : e3 : f3 : fs3 : g3 : gs3 :
 a3 : as3 : b3 : c4 : cs4 : d4 : ds4 : e4 : f4 : fs4 : g4 : gs4 : _ =
  iterate ((2 ** recip 12) *) (55/44100)


chordSequence :: [(Word, NonEmpty.T [] Float)]
chordSequence =
   (2, f1  !: f2  : a2 : c3 : []) :
   (1, g1  !: g2  : b2 : d3 : []) :
   (2, c2  !: g2  : c3 : e3 : []) :
   (1, f1  !: a2  : c3 : f3 : []) :
   (2, g1  !: g2  : b2 : d3 : []) :
   (1, gs1 !: gs2 : b2 : e3 : []) :
   (2, a1  !: e2  : a2 : c3 : []) :
   (1, g1  !: g2  : b2 : d3 : []) :
   (3, c2  !: g2  : c3 : e3 : []) :

   (2, f1  !: f2  : a2 : c3 : []) :
   (1, g1  !: g2  : b2 : d3 : []) :
   (2, c2  !: g2  : c3 : e3 : []) :
   (1, f1  !: a2  : c3 : f3 : []) :
   (2, g1  !: g2  : b2 : d3 : []) :
   (1, gs1 !: gs2 : b2 : e3 : []) :
   (2, a1  !: e2  : a2 : c3 : []) :
   (1, g1  !: g2  : b2 : e3 : []) :
   (3, c2  !: e2  : g2 : c3 : []) :
   []


withDur :: (Word -> a -> v) -> Word -> a -> (v, NonNeg.Int)
withDur f d ps =
   let dur = d*30000
   in  (f dur ps, NonNeg.fromNumber $ fromIntegral dur)


padMusic :: IO ()
padMusic = do
   arrange <- SigStL.makeArranger
   amplify <-
      CausalRender.run $ \volume ->
         Stereo.multiValue ^<<
         Causal.amplifyStereo volume <<^
         Stereo.unMultiValue
   (playStereoVector =<<) $
      pioApply (amplify (0.1::Float)) $
      arrange tonesChunkSize $
      EventListTM.switchTimeR const $
      EventListMT.consTime 0 $
      EventListBT.fromPairList $
      map (\(d,ps) -> withDur stereoOsciSawPad d ps)
      chordSequence


lowpassSweepControlRateCausal ::
   Causal.T
      (Stereo.T (MultiValue.T Float))
      (Stereo.T (MultiValue.T Float))
lowpassSweepControlRateCausal =
--   Causal.stereoFromVector $
   Causal.stereoFromMono $
      UniFilter.lowpass ^<<
      Ctrl.processCtrlRate 128
         (lfoSine (UniFilter.parameter (10::Exp Float)))


moogSweepControlRateCausal ::
   Causal.T
      (Stereo.T (MultiValue.T Float))
      (Stereo.T (MultiValue.T Float))
moogSweepControlRateCausal =
--   Causal.stereoFromVector $
   Causal.stereoFromMono $
      Ctrl.processCtrlRate 128
         (lfoSine (Moog.parameter TypeNum.d8 (10::Exp Float)))


filterMusic :: IO ()
filterMusic = do
   arrange <- SigStL.makeArranger
   pad <- stereoOsciSawPadIO
   proc <-
      CausalRender.run $ \volume ->
         Stereo.multiValue ^<<
         Causal.amplifyStereo volume <<<
         moogSweepControlRateCausal <<^
         Stereo.unMultiValue
   (playStereoVector =<<) $
      pioApply (proc (0.05::Float)) $
      arrange tonesChunkSize $
      EventListTM.switchTimeR const $
      EventListMT.consTime 0 $
      EventListBT.fromPairList $
      map (\(d,ps) -> withDur pad d ps)
      chordSequence



stereoOsciSawVectorIO :: IO (Float -> SVL.Vector (Stereo.T Float))
stereoOsciSawVectorIO =
   (\f freq -> uncurry (f tonesChunkSize) (stereoOsciParams TypeNum.d5 freq))
   <$>
   Render.run stereoOsciSawP

applyFadeEnvelopeIO ::
   IO (Word -> SVL.Vector (Stereo.T Float) -> SVL.Vector (Stereo.T Float))
applyFadeEnvelopeIO =
   let attack = 20000 in
   fmap
      (\f dur sig ->
         Unsafe.performIO $ pioApply (f attack (dur-attack)) sig) $
   CausalRender.run
      (\intro len ->
         Stereo.multiValue <$>
            (Causal.envelopeStereo $< fadeEnvelope intro len)
               <<^ Stereo.unMultiValue)

stereoOsciSawChordIO :: IO (NonEmpty.T [] Float -> SVL.Vector (Stereo.T Float))
stereoOsciSawChordIO = do
   sawv <- stereoOsciSawVectorIO
   mix <- mixVectorStereoIO
   return (NonEmpty.foldBalanced mix . fmap sawv)

stereoOsciSawPadIO ::
   IO (Word -> NonEmpty.T [] Float -> SVL.Vector (Stereo.T Float))
stereoOsciSawPadIO = do
   chrd <- stereoOsciSawChordIO
   envelope <- applyFadeEnvelopeIO
   return $
      \ dur pitches -> envelope dur (chrd pitches)

padMusicIO :: IO ()
padMusicIO = do
   arrange <- SigStL.makeArranger
   pad <- stereoOsciSawPadIO
   amplify <-
      CausalRender.run $ \volume ->
         Stereo.multiValue ^<<
         Causal.amplifyStereo volume <<^
         Stereo.unMultiValue
   (playStereoVector =<<) $
      pioApply (amplify (0.08::Float)) $
      arrange tonesChunkSize $
      EventListTM.switchTimeR const $
      EventListMT.consTime 0 $
      EventListBT.fromPairList $
      map (uncurry (withDur pad)) $
      chordSequence

{-
Apply the envelope separately to each tone of the chord
and mix all tones by 'arrange'.
-}
padMusicSeparate :: IO ()
padMusicSeparate = do
   arrange <- SigStL.makeArranger
   osci <- stereoOsciSawVectorIO
   env <- applyFadeEnvelopeIO
   amplify <-
      CausalRender.run $ \volume ->
         Stereo.multiValue ^<<
         Causal.amplifyStereo volume <<^
         Stereo.unMultiValue
   (playStereoVector =<<) $
      pioApply (amplify (0.08::Float)) $
      arrange tonesChunkSize $
      EventList.flatten $
      EventListTM.switchTimeR const $
      EventListMT.consTime 0 $
      EventListBT.fromPairList $
      map (uncurry (withDur (\d ps ->
         map (\p -> env d (osci p)) $ NonEmpty.flatten ps))) $
      chordSequence


delay :: IO ()
delay =
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   fmap (\f -> f tonesChunkSize (0::Word) (10000::Word)) $
   Render.run $ \del dur ->
      Causal.delayZero del . Causal.take dur
      $*
      Sig.osci Wave.saw 0 (frequency 0.01)

delayStereo :: IO ()
delayStereo =
   (SVL.writeFile "speedtest.f32" . asStereo =<<) $
   fmap (\f -> f tonesChunkSize (7::Word) (10000::Word)) $
   Render.run $ \del dur ->
      Causal.take dur . liftA2 Stereo.consMultiValue id (Causal.delayZero del)
      $*
      Sig.osci Wave.saw 0 (frequency 0.01)

delayPhaser :: IO ()
delayPhaser =
   (SVL.writeFile "speedtest.f32" . asStereo =<<) $
   fmap (\f -> f tonesChunkSize (40000::Word)) $
   Render.run $ \dur ->
   Func.compileSignal $
      let osci = Func.fromSignal $ Sig.osci Wave.saw 0 (frequency 0.01)
          ctrl =
             Func.fromSignal $
             Sig.osci Wave.triangle 0 $ frequency (1/20000)
      in  Causal.take dur $&
          liftA2 Stereo.consMultiValue
             osci
             (Causal.delayControlledInterpolated Interpolation.cubic 0 100
              $&
              (50+50*ctrl) &|& osci)



allpassControl ::
   (TypeNum.Natural n) =>
   Proxy n -> Exp Float ->
   Sig.T (Allpass.CascadeParameter n (MultiValue.T Float))
allpassControl order reduct =
   Sig.interpolateConstant reduct $
   lfoSine (Allpass.flangerParameter order) reduct

allpassPhaserCausal, allpassPhaserPipeline ::
   Exp Float ->
   Sig.MV Float ->
   Sig.MV Float
allpassPhaserCausal reduct xs =
   let order = TypeNum.d16
   in 0.5 * Allpass.phaser $< allpassControl order reduct $* xs

allpassPhaserPipeline reduct xs =
   let order = TypeNum.d16
   in (nest (TypeNum.integralFromProxy order) Sig.tail) $
      -- Sig.drop
      --    (TypeNum.integralFromProxy order)
         (0.5 * Allpass.phaserPipeline $< allpassControl order reduct $* xs)

allpassPhaser :: IO ()
allpassPhaser =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap (\f -> f (SVL.chunkSize 100000) (128::Float)) $
   Render.run $
   \reduct ->
--      allpassPhaserCausal reduct $
      allpassPhaserPipeline reduct $
      Sig.osci Wave.saw 0 (frequency 0.01)

noise :: IO ()
noise =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run $
   Sig.noise 0 0.3

noisePacked :: IO ()
noisePacked =
   (SVL.writeFile "speedtest.f32" . asMonoPacked
      . SVL.take (div 10000000 4) =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run $
   SigPS.noise 0 0.3
--   SigPS.pack (SigP.noise 0 0.3)
--   SigPS.packSmall (SigP.noise 0 0.3)

frequencyModulationStorable :: IO ()
frequencyModulationStorable = do
   sample <- Render.run $ Sig.osci Wave.saw 0 (frequency 0.01)
   f <-
      Render.run $ \smp ->
         Causal.frequencyModulationLinear smp $* 0.3
   SVL.writeFile "speedtest.f32" . asMono $
      f (SVL.chunkSize 100000) $ SVL.take 1000000 $ sample (SVL.chunkSize 1000)


frequencyModulation :: IO ()
frequencyModulation =
   (SVL.writeFile "speedtest.f32" . asMono . SVL.take 10000000 =<<) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (Causal.frequencyModulationLinear (Sig.osci Wave.saw 0 (frequency 0.01))
       $* Sig.exponential2 500000 1)

frequencyModulationStereo :: IO ()
frequencyModulationStereo = do
   sample <- Render.run $ Sig.osci Wave.saw 0 (frequency 0.01)
   f <-
      Render.run $ \smp ->
         Stereo.multiValue ^<<
         Causal.stereoFromMono (Causal.frequencyModulationLinear smp)
            $* Sig.constant (Stereo.cons 0.2999 0.3001)
   SVL.writeFile "speedtest.f32" . asStereo $
      f (SVL.chunkSize 100000) $ SVL.take 1000000 $ sample (SVL.chunkSize 1000)

frequencyModulationProcess :: IO ()
frequencyModulationProcess = do
   proc <-
      CausalRender.run
         (Causal.frequencyModulationLinear
            (Causal.take 50000 $* Sig.osci Wave.saw 0 (frequency 0.01)))
   sample <- Render.run (1 + 0.1 * Sig.osci Wave.approxSine2 0 0.0001)
   SVL.writeFile "speedtest.f32" . asMono =<<
      pioApply proc (sample (SVL.chunkSize 512))



quantize :: IO ()
quantize =
{-
   SV.writeFile "speedtest.f32" $
   asMono $
   (\xs -> SigP.render xs 10000000 ()) $
-}
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   fmap (SVL.take 10000000) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run $
      (Causal.quantizeLift id
         $<# (5.5::Float)
         $* Sig.osci Wave.saw 0 (frequency 0.01))

quantizedFilterControl :: IO ()
quantizedFilterControl =
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   fmap (SVL.take 10000000) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (0.3 * (UniFilter.lowpass ^<< Ctrl.process)
       $< (Causal.quantizeLift
            (Causal.map (UniFilter.parameter 100) <<<
   --         (Causal.map (Moog.parameter TypeNum.d8 100) <<<
             Causal.map (\x -> 0.01 * exp (2 * x)))
            $<# (128::Float)
            $* Sig.osci Wave.approxSine2 0 (frequency (0.1/44100)))
       $* Sig.osci Wave.saw 0 (frequency 0.01))


arrowNonShared :: IO ()
arrowNonShared =
   (SVL.writeFile "speedtest.f32" . asStereo =<<) $
   fmap (SVL.take 10000000) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (let osci = Causal.osci Wave.approxSine2
       in liftA2 Stereo.consMultiValue osci osci $* Sig.constant (0, 0.01))

arrowShared :: IO ()
arrowShared =
   (SVL.writeFile "speedtest.f32" . asStereo =<<) $
   fmap (SVL.take 10000000) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (let osci = Func.lift $ Causal.osci Wave.approxSine2
       in Func.compile (liftA2 Stereo.consMultiValue osci osci) $*
          Sig.constant (0, 0.01))

arrowIndependent :: IO ()
arrowIndependent =
   (SVL.writeFile "speedtest.f32" . asStereo =<<) $
   fmap (SVL.take 10000000) $
   fmap ($ SVL.chunkSize 100000) $
   Render.run
      (let osci = Causal.osci Wave.approxSine2
       in Func.compile
               (uncurry Stereo.consMultiValue  <$>
                  (osci *** osci  $&  Func.lift id)) $*
            Sig.constant ((0, 0.01), (0.25, 0.01001)))


rampDown :: Int -> SV.Vector Float
rampDown n =
   SigS.toStrictStorableSignal n $
   CtrlS.line n (1, 0)

impulses :: Int -> Float -> SVL.Vector Float
impulses n x =
   SVL.fromChunks $
   concatMap (\k -> [SV.singleton x, SV.replicate k 0]) $
   take n $ iterate (2*) 1

convolution :: IO ()
convolution =
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   ((\f ->
      pioApply (f $ Render.buffer $ rampDown 1000) (impulses 18 0.1)) =<<) $
   CausalRender.run FiltNR.convolve

convolutionPacked :: IO ()
convolutionPacked = do
   pack <- Render.run SigPS.pack
   impulsesPacked <- pack SVL.defaultChunkSize $ impulses 18 0.1
   (SVL.writeFile "speedtest.f32" . asMonoPacked =<<) $
      ((\f ->
         pioApply (f $ Render.buffer $ rampDown 1000) impulsesPacked) =<<) $
      CausalRender.run FiltNR.convolvePacked


helixSaw :: IO ()
helixSaw = do
   let srcFreq = 0.01
       srcLength :: Word
       srcLength = 40000
   osci <- Render.run $ \dur -> Sig.osci Wave.saw 0 srcFreq * (1-Sig.ramp dur)
   let perc =
         asMono $ osci (fromIntegral srcLength) srcLength
   SV.writeFile "osci-saw.f32" perc
   stretched <-
      Render.run $ \dur sig ->
      Func.compileSignal $
      (Helix.static Interpolation.cubic Interpolation.cubic
            100 (recip srcFreq) sig
         $&
         (Func.fromSignal $ Sig.amplify (fromIntegral srcLength) $ Sig.ramp dur)
         &|&
         (Causal.osciCore $& 0 &|& 0.01))
   SVL.writeFile "osci-stretched.f32" . asMono =<<
      stretched SVL.defaultChunkSize (80000::Word) (Render.buffer perc)


loadTomato :: IO (Float, SVL.Vector Float)
loadTomato = do
   let Sample.Info name _sampleRate positions = Sample.tomatensalat
   word <- Sample.load (Default.sampleDirectory </> name)
   return (Sample.period $ head positions, word)

helixOsci :: Exp Float -> Func.T a (MultiValue.T Float)
helixOsci period =
   Causal.osciCore  $&  0 &|& Func.fromSignal (Sig.constant (recip period))

helixSpeechStaticSig ::
   Func.T () (MultiValue.T Float) ->
   Exp (Source.StorableVector Float) ->
   Exp Float ->
   Sig.MV Float
helixSpeechStaticSig shape word period =
   Func.compileSignal
      (Helix.static Interpolation.linear Interpolation.linear
          (Expr.roundToIntFast period) period word
       $&
       shape
       &|&
       helixOsci period)

helixSpeechStaticSpeed ::
   Exp Float ->
   Exp (Source.StorableVector Float) ->
   Exp Float ->
   Sig.MV Float
helixSpeechStaticSpeed speed word =
   helixSpeechStaticSig
      (Func.fromSignal
         (Causal.takeWhile
            (Expr.fromIntegral (Source.storableVectorLength word) >*)
          $*
          Sig.rampSlope speed))
      word

helixSpeechStatic :: IO ()
helixSpeechStatic = do
   smp <- loadTomato
   stretched <-
      Render.run $ \speed (period, word) ->
         helixSpeechStaticSpeed speed word period
   (SVL.writeFile "speech-stretched.f32" . asMono =<<) $
      stretched SVL.defaultChunkSize (0.5::Float) $
      mapSnd (Render.buffer . SV.concat . SVL.chunks) smp

helixSpeechDynamicSig ::
   Func.T () (MultiValue.T Float) ->
   Sig.MV Float ->
   Exp Float ->
   Sig.MV Float
helixSpeechDynamicSig shape word period =
   Func.compileSignal
      (Helix.dynamicLimited Interpolation.linear Interpolation.linear
          (Expr.roundToIntFast period) period word
       $&
       shape
       &|&
       helixOsci period)

helixSpeechDynamicSpeed ::
   Exp Float ->
   Sig.MV Float ->
   Exp Float ->
   Sig.MV Float
helixSpeechDynamicSpeed speed =
   helixSpeechDynamicSig (Func.fromSignal $ Sig.constant speed)

helixSpeechDynamic :: IO ()
helixSpeechDynamic = do
   smp <- loadTomato
   stretched <-
      Render.run $ \speed (period, word) ->
      helixSpeechDynamicSpeed speed word period
   SVL.writeFile "speech-stretched.f32" $ asMono $
      stretched SVL.defaultChunkSize (0.5::Float) smp

helixSpeechCompare :: IO ()
helixSpeechCompare = do
   (per,smp) <- loadTomato
   stretched <-
      Render.run $ \speed period word wordBuffer ->
      fmap Stereo.multiValue $
      sequenceA $
      Stereo.cons
         (helixSpeechStaticSpeed speed wordBuffer period)
         (helixSpeechDynamicSpeed speed word period)
   SVL.writeFile "speech-stretched.f32" $ asStereo $
      stretched SVL.defaultChunkSize (0.5::Float)
         per smp (Render.buffer . SV.concat . SVL.chunks $ smp)

helixSpeechVariCompare :: IO ()
helixSpeechVariCompare = do
   (per,smp) <- loadTomato
   stretched <-
      Render.run $ \period word wordBuffer ->
      fmap Stereo.multiValue $
      sequenceA $
      let speed =
             Func.fromSignal $ Sig.cycle $
             Sig.fromArray $ Expr.cons $
             (MultiValue.Array [0.2, 0.5, 1, 1.5, 1.8]
                :: MultiValue.Array TypeNum.D5 Float)
      in  Stereo.cons
             (helixSpeechStaticSig
                 (Causal.integrateZero $& speed)
                 wordBuffer period)
             (helixSpeechDynamicSig speed word period)
   SVL.writeFile "speech-stretched.f32" $ asStereo $
      stretched SVL.defaultChunkSize
         per smp (Render.buffer . SV.concat . SVL.chunks $ smp)

helixLimited :: IO ()
helixLimited = do
   let period = 100
       srcLength :: Int
       srcLength = 500
       dstLength = 5000
       speed :: Exp Float
       speed = 0.5
       osci =
          0.5
          *
          Sig.ramp (fromIntegral srcLength)
          *
          Sig.osci Wave.approxSine2 0 (recip period)
   renderOsci <- Render.run osci
   let osciVec = renderOsci srcLength
   SV.writeFile "helix-orig.f32" $ asMono osciVec

   let stretchedStatic osciBuffer =
          Helix.static Interpolation.linear Interpolation.linear
             (Expr.roundToIntFast period) period osciBuffer
          $&
          Func.fromSignal (Sig.rampSlope speed)
          &|&
          helixOsci period
       stretchedDynamic =
          Helix.dynamic Interpolation.linear Interpolation.linear
             (Expr.roundToIntFast period) period osci
          $&
          Func.fromSignal (Sig.constant speed)
          &|&
          helixOsci period
       stretched osciBuffer =
          liftA2 Stereo.consMultiValue
             (stretchedStatic osciBuffer) stretchedDynamic
   renderHelix <- Render.run $ Func.compileSignal . stretched
   SV.writeFile "helix-stretched.f32" $ asStereo $
      renderHelix dstLength (Render.buffer osciVec)

cycleRamp :: IO ()
cycleRamp =
   SVL.writeFile "speedtest.f32" . asMono .
         (\f -> f SVL.defaultChunkSize (10000::Word)) =<<
      Render.run
         (\dur ->
            Causal.take 100000 $*
            Sig.cycle (Sig.append (Sig.ramp dur) (1 - Sig.ramp dur)))

zigZag :: IO ()
zigZag =
   SVL.writeFile "speedtest.f32" . asMono .
         (\f -> f SVL.defaultChunkSize (-3::Float)) =<<
      Render.run
         (\start -> Causal.take 100000 $* (Helix.zigZag start $* 0.0001))

zigZagPacked :: IO ()
zigZagPacked =
   SVL.writeFile "speedtest.f32" . asMonoPacked .
         (\f -> f SVL.defaultChunkSize (-3::Float)) =<<
      Render.run
         (\start ->
            let vectorSize = 4
            in Causal.take (fromInteger $ div 100000 vectorSize) $*
                  (Helix.zigZagPacked start $* 0.0001))


trigger :: IO ()
trigger =
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ SVL.defaultChunkSize) $
      Render.run
         (let pause len =
                 CausalClass.applyConst (Causal.take len) Maybe.nothing
              pulse :: Float -> Exp Word -> Sig.T (Maybe.T (MultiValue.T Float))
              pulse freq len =
                 Causal.take len .
                 arr (flip Maybe.fromBool (MultiValue.cons freq) . unbool) .
                 Causal.delay1 Expr.true $*# False
          in  (Causal.zipWith ExprMaybe.select
                  $> Sig.noise 0 (0.01 :: Exp Float)) $*
              (Causal.trigger (\freq -> Causal.take 150000 $* pingSigP freq) $*
               pause 50000 <>
               pulse 0.004 100000 <>
               pulse 0.005 200000 <>
               pulse 0.006 400000))

-- FixMe: duplicate of CausalExp.ProcessPrivate
unbool :: MultiValue.T Bool -> LLVM.Value Bool
unbool (MultiValue.Cons b) = b


triggerLFO :: Sig.MV Float
triggerLFO =
   Sig.osci Wave.approxSine2 0 0.00015
   +
   Sig.osci Wave.approxSine2 0 0.000037

trackZeros :: Causal.MV Float Bool
trackZeros =
   Causal.zipWith (\x y -> x &&* Expr.not y) .
   (id &&& Causal.delay1 Expr.false) .
   Causal.map (>* 0)

fmPingSig :: Exp Float -> Exp Float -> Sig.MV Float
fmPingSig freq depth =
   Sig.exponential2 5000 1
   *
   ((Causal.osci Wave.approxSine2 $> Sig.constant freq)
    $*
    (Sig.constant depth * Sig.osci Wave.approxSine2 0 (2*freq)))

sweepTrigger :: IO ()
sweepTrigger =
   (SVL.writeFile "speedtest.f32" . asMono =<<) $
   fmap ($ SVL.defaultChunkSize) $
      Render.run
         ((Causal.zipWith ExprMaybe.select $> Sig.noise 0 0.01) $*
            (Causal.trigger (fmPingSig 0.005) $*
               liftA2 (Maybe.fromBool . unbool)
                  (Causal.take 10000000 . trackZeros $* triggerLFO)
                  (5 * Sig.osci Wave.approxSine2 0 0.00001)))


main :: IO ()
main = do
   LLVM.initializeNativeTarget
   filterSweepComplex
