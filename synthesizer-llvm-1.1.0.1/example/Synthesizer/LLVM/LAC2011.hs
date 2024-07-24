{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Synthesizer.LLVM.LAC2011 where

import Synthesizer.LLVM.ExampleUtility

import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked as BandPass
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.Butterworth as Butterworth
import qualified Synthesizer.LLVM.Filter.Chebyshev as Chebyshev
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Filter.SecondOrderPacked as Filt2P
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter
import qualified Synthesizer.LLVM.Causal.Controlled as Ctrl
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.SignalPacked as GenP
import qualified Synthesizer.LLVM.Generator.Signal as Gen
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Frame as Frame
import qualified Synthesizer.LLVM.Wave as Wave

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (D4, D8, D16, d0, d1, d2, d3, d4, d5, d6, d7, d8)

import Synthesizer.LLVM.Causal.Process (($<), ($*), ($*#))

import qualified Synthesizer.Plain.Filter.Recursive as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1Core
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2Core

import Control.Arrow (Arrow, arr, (&&&), (^<<))
import Control.Category ((<<<), (.), id)
import Control.Monad ((<=<))
import Control.Applicative (liftA2, pure)
import Control.Functor.HT (void)
import Data.Traversable (traverse)

import Foreign.Storable (Storable)
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified Sound.Sox.Option.Format as SoxOption
import qualified Sound.Sox.Frame as SoxFrame
import qualified Sound.Sox.Play as SoxPlay

-- import qualified Sound.ALSA.PCM as ALSA
-- import qualified Synthesizer.ALSA.Storable.Play as Play

import Data.List (genericLength)
import System.Random (randomRs, mkStdGen)

import qualified System.IO as IO

import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (fst, snd, id, (.))
import qualified NumericPrelude.Base as P


playStereo :: Gen.T (Stereo.T (MultiValue.T Float)) -> IO ()
playStereo sig =
   playStereoStream . ($ SVL.chunkSize 100000) =<<
   Render.run (fmap Stereo.multiValue sig)

playStereoStream :: SVL.Vector (Stereo.T Float) -> IO ()
playStereoStream = playStreamSox

playMono :: Gen.MV Float -> IO ()
playMono sig  =  playMonoStream . ($ SVL.chunkSize 100000) =<< Render.run sig

playMonoPacked :: Gen.T (MultiValue.T (Serial.T D4 Float)) -> IO ()
playMonoPacked =
   playMonoStream .
   SigStL.unpack .
   ($ SVL.chunkSize 100000) <=<
   Render.run

playMonoStream :: SVL.Vector Float -> IO ()
playMonoStream = playStreamSox


{-
play ::
   (C.MakeValueTuple y, Tuple.ValueOf y ~ a, Memory.C a struct) =>
   Gen.MV a -> IO ()
play =
   playStreamSox .
   Gen.renderChunky (SVL.chunkSize 100000)
-}

{-
playStreamALSA ::
   (Additive.C y, ALSA.SampleFmt y) =>
   SVL.Vector y -> IO ()
playStreamALSA =
   Play.auto (Play.makeSink Play.defaultDevice (0.05::Double) sampleRate)
-}

-- reacts faster to CTRL-C
playStreamSox ::
   (Storable y, SoxFrame.C y) =>
   SVL.Vector y -> IO ()
playStreamSox =
   void . SoxPlay.simple SVL.hPut SoxOption.none sampleRate


sampleRate :: Ring.C a => a
sampleRate = 44100

intSecond :: Ring.C a => Float -> a
intSecond t = fromInteger $ round $ t * sampleRate

second :: Field.C a => a -> a
second t = t * sampleRate

hertz :: Field.C a => a -> a
hertz f = f / sampleRate

sine :: IO ()
sine =
   playMono (0.99 * Gen.osci Wave.sine 0 (hertz 440))

ping :: IO ()
ping =
   playMono (Gen.exponential2 (second 1) 1 * Gen.osci Wave.triangle 0 (hertz 440))

tremolo :: IO ()
tremolo =
   playMono (Gen.osci Wave.sine 0 (hertz 0.3) * Gen.osci Wave.triangle 0 (hertz 440))


stereo :: IO ()
stereo =
   playStereo (liftA2 Stereo.cons (Gen.osci Wave.triangle 0 (hertz 439)) (Gen.osci Wave.triangle 0 (hertz 441)))

stereoFancy :: IO ()
stereoFancy =
   playStereo (traverse (Gen.osci Wave.triangle 0 . hertz) (Stereo.cons 439 441))


pingParam :: IO (Float -> SVL.Vector Float)
pingParam =
   fmap ($ SVL.chunkSize 1024) $
   Render.run $ \freq ->
   Gen.exponential2 (second 0.3) 1 * Gen.osci Wave.triangle 0 freq

playPingParam :: IO ()
playPingParam = do
   png <- pingParam
   playMonoStream (SVL.take (intSecond 1) $ png (hertz 880))

melody :: IO (SVL.Vector Float)
melody = do
   png <- pingParam
   return $ SVL.concat $ map (SVL.take (intSecond 0.2) . png . hertz) $ cycle [440, 550, 660, 880]

playMelody :: IO ()
playMelody = do
   mel <- melody
   playMonoStream mel

pingParam2 :: IO ((Float, Float) -> SVL.Vector Float)
pingParam2 =
   fmap ($ SVL.chunkSize 1024) $
   Render.run $ \(amp,freq) ->
   Gen.exponential2 (second 0.3) amp * Gen.osci Wave.triangle 0 freq

playMelody2 :: IO ()
playMelody2 = do
   png <- pingParam2
   playMonoStream $ SVL.concat $ map (SVL.take (intSecond 0.2) . png) $ zip (map sin $ [0,0.1..]) (cycle $ map hertz [440, 550, 660, 880])


retard :: Gen.MV Float -> Gen.MV Float
retard xs =
   Causal.frequencyModulationLinear xs .
   Causal.map Field.recip $*
   (1 + Gen.rampInf (second 10))

playRetarded :: IO ()
playRetarded = do
   mel <- melody
   ret <- Render.run retard
   playMonoStream $ ret (SVL.chunkSize 10000) mel



pingGen :: Gen.MV Float
pingGen =
   Gen.exponential2 (second 0.5) 0.7 *
   Gen.osci Wave.triangle 0 (hertz 440)

delay :: IO ()
delay =
   playMono $
      pingGen + 0.7 * (Causal.delay 0 (intSecond 0.5) $* pingGen)

delayArrow :: IO ()
delayArrow =
   playMono
      ((id + 0.7 * Causal.delay 0 (intSecond 0.5)) $* pingGen)

comb :: IO ()
comb =
   playMono $
      (Causal.loopZero
          (id  &&&  0.7 * Causal.delay 0 (intSecond 0.5)
             <<< Causal.mix) $*
       pingGen)


lfoSine :: Exp Float -> Gen.T (Moog.Parameter D8 (MultiValue.T Float))
lfoSine reduct =
   Causal.map (Moog.parameter d8 30 . (hertz 700 *) . (2**))
   $*
   Gen.osci Wave.sine 0 (reduct * hertz 0.1)

filterSweep :: IO ()
filterSweep =
   playMono $
      (Ctrl.processCtrlRate 128 lfoSine $* Gen.noise 0 (recip $ hertz 3.5e6))


pingPacked :: IO ()
pingPacked =
   playMonoPacked $
      GenP.exponential2 (second 1) 1 * GenP.osci Wave.triangle 0 (hertz 440)
