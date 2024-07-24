{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Synthesizer.LLVM.LNdW2011 where

import Synthesizer.LLVM.ExampleUtility

import qualified Synthesizer.LLVM.Causal.Render as Render
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalP
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as SigRender
import qualified Synthesizer.LLVM.Generator.SignalPacked as GenP
import qualified Synthesizer.LLVM.Generator.Signal as Gen

import qualified Synthesizer.LLVM.Plug.Input as PIn
import qualified Synthesizer.LLVM.Plug.Output as POut
import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.MIDI.EventList as Ev
import qualified Synthesizer.MIDI.CausalIO.ControllerSelection as MCS
import qualified Synthesizer.MIDI.CausalIO.Process as PMIDI
import qualified Synthesizer.MIDI.Value as MV
import qualified Synthesizer.ALSA.CausalIO.Process as PALSA
import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Zip as Zip
import Synthesizer.ALSA.EventList (ClientName(ClientName))

import qualified Sound.MIDI.Controller as Ctrl
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Class.Check as MidiCheck

import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked as BandPass
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.Butterworth as Butterworth
import qualified Synthesizer.LLVM.Filter.Chebyshev as Chebyshev
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Filter.SecondOrderPacked as Filt2P
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Frame as Frame
import qualified Synthesizer.LLVM.Wave as Wave

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Core as LLVM
import LLVM.Core (Value, value, valueOf, constVector, constOf)
import LLVM.Util.Arithmetic () -- Floating instance for TValue

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (D4, D8, D16, d0, d1, d2, d3, d4, d5, d6, d7, d8)

import qualified Synthesizer.Causal.Class as CausalClass
import Synthesizer.Causal.Class (($<), ($*))

import qualified Synthesizer.Plain.Filter.Recursive as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1Core
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2Core

import qualified Synthesizer.Causal.Spatial as Spatial

import qualified Control.Monad.Trans.State as State
import qualified Control.Arrow as Arr
import Control.Arrow (Arrow, arr, (&&&), (***), (^<<), (^>>), (>>^))
import Control.Category ((<<<), (.), id, (>>>))
import Control.Monad (liftM2, (<=<))
import Control.Applicative (liftA2, pure, (<$>))
import Control.Functor.HT (void)
import Data.Tuple.HT (mapPair)
import Data.Traversable (traverse)

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Class as NonNeg

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt

import qualified Sound.Sox.Option.Format as SoxOption
import qualified Sound.Sox.Frame as SoxFrame
import qualified Sound.Sox.Play as SoxPlay

import qualified Sound.ALSA.PCM as ALSA
import qualified Synthesizer.ALSA.Storable.Play as Play

import Data.List (genericLength)
import System.Random (randomRs, mkStdGen)

import qualified System.IO as IO

import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive
import qualified Algebra.IntegralDomain as Integral

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (fst, snd, id, (.))
import qualified NumericPrelude.Base as P


playStereo :: Gen.T (Stereo.T (MultiValue.T Float)) -> IO ()
playStereo sig =
   playStereoStream . ($ SVL.chunkSize 100000) =<<
   SigRender.run (fmap Stereo.multiValue sig)

playStereoStream :: SVL.Vector (Stereo.T Float) -> IO ()
playStereoStream = playStreamSox

playMono :: Gen.MV Float -> IO ()
playMono sig =
   playMonoStream . ($ SVL.chunkSize 100000) =<< SigRender.run sig

playMonoPacked :: Gen.T VectorValue -> IO ()
playMonoPacked =
   playMonoStream .
   SigStL.unpack .
   ($ SVL.chunkSize 100000) <=<
   SigRender.run

playMonoStream :: SVL.Vector Float -> IO ()
playMonoStream = playStreamSox


playStreamALSA ::
   (Additive.C y, ALSA.SampleFmt y) =>
   SVL.Vector y -> IO ()
playStreamALSA =
   Play.auto (Play.makeSink Play.defaultDevice (0.05::Double) sampleRate)

-- reacts faster to CTRL-C
playStreamSox ::
   (Storable y, SoxFrame.C y) =>
   SVL.Vector y -> IO ()
playStreamSox =
   void . SoxPlay.simple SVL.hPut SoxOption.none sampleRate


sampleRate :: Ring.C a => a
sampleRate = 44100

type Vector = Serial.T VectorSize Float
type VectorSize = TypeNum.D4
type VectorValue = MultiValue.T Vector

vectorSize :: Int
vectorSize =
   TypeNum.integralFromSingleton
      (TypeNum.singleton :: TypeNum.Singleton VectorSize)

vectorRate :: Field.C a => a
vectorRate = sampleRate / fromIntegral vectorSize



intSecond :: Ring.C a => Float -> a
intSecond t = fromInteger $ round $ t * sampleRate

second :: Field.C a => a -> a
second t = t * sampleRate

hertz :: Field.C a => a -> a
hertz f = f / sampleRate



fst :: Arrow arrow => arrow (a,b) a
fst = arr P.fst

snd :: Arrow arrow => arrow (a,b) b
snd = arr P.snd


playFromEvents ::
   (ALSA.SampleFmt a, Additive.C a) =>
   Double ->
   Double ->
   PIO.T PALSA.Events (SV.Vector a) ->
   IO ()
playFromEvents latency period =
   PALSA.playFromEvents
      Play.defaultDevice (ClientName "Haskell-LLVM-demo")
      latency period sampleRate


modulation :: IO ()
modulation = do
   proc <- Render.run (0.95 * (Causal.osci Wave.approxSine4 $< 0))
   playFromEvents 0.01 (0.015::Double)
      ((proc :: PIO.T (EventListBT.T NonNegW.Int Float) (SV.Vector Float))
       .
       PMIDI.controllerExponential
         (ChannelMsg.toChannel 0)
         Ctrl.modulation
         (hertz 500, hertz 2000) (hertz 1000))


vectorBlockSize :: Double
vectorBlockSize = fromIntegral $ 150*vectorSize

subsample, _subsample :: (Integral.C t) => t -> t -> State.State t t
subsample step t  =  State.state $ \r -> divMod (r+t) step
_subsample step t = do
   State.modify (t+)
   (q,r) <- State.gets (flip divMod step)
   State.put r
   return q

subsampleBT :: EventListBT.T NonNegW.Int a -> EventListBT.T NonNegW.Int a
subsampleBT =
   flip State.evalState NonNeg.zero .
   EventListBT.mapTimeM
      (subsample (NonNegW.fromNumberMsg "vectorSize" vectorSize))

modulationPacked :: IO ()
modulationPacked = do
   proc <-
      Render.run
         (0.95 * (CausalP.osci Wave.approxSine4 $< 0)
          .
          Causal.map Serial.upsample)
   playFromEvents 0.01 (vectorBlockSize/sampleRate)
      (arr SigStL.unpackStrict
       .
       (proc :: PIO.T (EventListBT.T NonNegW.Int Float) (SV.Vector Vector))
       .
       arr subsampleBT
       .
       PMIDI.controllerExponential
         (ChannelMsg.toChannel 0)
         Ctrl.modulation
         (hertz 500, hertz 2000) (hertz 1000))


bubbles :: IO ()
bubbles = do
   proc <-
      Render.run
         (0.95 * (Causal.osci Wave.sine $< 0)
          .
          (fst.fst * (1 + snd.fst * snd))
          .
          Arr.second (Causal.osci Wave.saw $< 0))
   playFromEvents 0.01 (0.015::Double)
      ((proc ::
           PIO.T
              (Zip.T
                 (Zip.T
                    (EventListBT.T NonNegW.Int Float)
                    (EventListBT.T NonNegW.Int Float))
                 (EventListBT.T NonNegW.Int Float))
              (SV.Vector Float))
       .
       PIO.zip
          (PIO.zip
             (PMIDI.controllerExponential
                 (ChannelMsg.toChannel 0)
                 Ctrl.modulation
                 (hertz 500, hertz 2000) (hertz 1000))
             (PMIDI.controllerLinear
                 (ChannelMsg.toChannel 0)
                 Ctrl.timbre
                 (-1, 1) (-0.1)))
          (PMIDI.controllerExponential
             (ChannelMsg.toChannel 0)
             Ctrl.soundVariation
             (hertz 1, hertz 10) (hertz 1)))

bubbleControl ::
   (MidiCheck.C event) =>
   PIO.T (EventListTT.T Ev.StrictTime [event]) (PCS.T Int Float)
bubbleControl =
   MCS.filter [
      MCS.controllerExponential Ctrl.volume (0.001, 0.99) 0.5,
      MCS.controllerExponential Ctrl.modulation (hertz 500, hertz 2000) (hertz 1000),
      MCS.controllerLinear Ctrl.soundVariation (-1, 1) 0.7,
      MCS.controllerExponential Ctrl.timbre (hertz 0.2, hertz 5) (hertz 1),
      MCS.controllerLinear Ctrl.soundController5 (-1, 1) 0.5,
      MCS.controllerExponential Ctrl.soundController7 (hertz 2, hertz 20) (hertz 10)]
   .
   MCS.fromChannel (ChannelMsg.toChannel 0)

bubblesSet :: IO ()
bubblesSet = do
   proc <-
      Render.runPlugged
         (PIn.controllerSet d6)
         (Causal.arrayElement d0 *
          (Causal.osci Wave.sine $< 0)
          .
          (Causal.arrayElement d1
           *
           (1 - Causal.arrayElement d2 *
              (Causal.osci Wave.saw $< 0) .
              Causal.arrayElement d3)
           *
           (1 - Causal.arrayElement d4 *
              (Causal.osci Wave.saw $< 0) .
              Causal.arrayElement d5)))
         POut.storableVector
   playFromEvents 0.01 (0.015::Double)
      ((proc :: PIO.T (PCS.T Int Float) (SV.Vector Float))
       .
       bubbleControl)


subsamplePCS :: PCS.T key a -> PCS.T key a
subsamplePCS =
   PCS.mapStream $
   flip State.evalState NonNeg.zero .
   EventListTT.mapTimeM (subsample (NonNegW.fromNumberMsg "vectorSize" $ fromIntegral vectorSize))

bubblesPacked :: IO ()
bubblesPacked = do
   proc <-
      Render.runPlugged
         (PIn.controllerSet d6)
         (CausalP.arrayElement d0 *
          (CausalP.osci Wave.approxSine4 $< 0)
          .
          (CausalP.arrayElement d1
           *
           (1 - CausalP.arrayElement d2 *
              (CausalP.osci Wave.saw $< 0) .
              CausalP.arrayElement d3)
           *
           (1 - CausalP.arrayElement d4 *
              (CausalP.osci Wave.saw $< 0) .
              CausalP.arrayElement d5)))
         POut.storableVector
   playFromEvents 0.01 (vectorBlockSize/sampleRate)
      (arr SigStL.unpackStrict
       .
       (proc :: PIO.T (PCS.T Int Float) (SV.Vector Vector))
       .
       arr subsamplePCS
       .
       bubbleControl)


{-
Implementation of 'moveAround' that just lifts the corresponding plain function
in the @Spatial@ module from @synthesizer-core@.
Unfortunately, this way we get a @PseudoModule v v@ constraint
that cannot be satisfied with @LLVM.Vector@s.
-}
moveAround2dLifted ::
   (Expr.Aggregate ve vl, Algebraic.C ve, NormedEuc.Sqr ve ve) =>
   ve -> ve -> (ve, ve) -> Causal.T (vl, vl) (vl, vl)
moveAround2dLifted att sonicDelay ear =
   Causal.map (Spatial.moveAround att sonicDelay ear)

moveAround2d ::
   (ve ~ Exp v, vl ~ MultiValue.T v,
    MultiValue.Algebraic v, MultiValue.RationalConstant v) =>
   ve -> ve -> (ve, ve) -> Causal.T (vl, vl) (vl, vl)
moveAround2d att sonicDelay ear =
   Causal.map $
      (\dist -> (sonicDelay*dist, 1/(att+dist)^2)) .
      euclideanNorm2d . subtract ear

euclideanNorm2d ::
   (MultiValue.Algebraic a) =>
   (Exp a, Exp a) -> Exp a
euclideanNorm2d (x,y) = Expr.sqrt $ Expr.sqr x + Expr.sqr y

flyChannel ::
   (ae ~ Exp Float, al ~ MultiValue.T Float) =>
   (ae, ae) -> Causal.T (al, (al, al)) al
flyChannel ear =
   ((snd ^>> moveAround2d 1 0.1 ear >>> Arr.first (negate id))
    &&&
    (Arr.second
         (2 * (Causal.differentiate (0,0) >>> Causal.map euclideanNorm2d))
     >>>
     Causal.mix))
   >>>
   arr (\((phase,volume), speed) -> (volume, (phase,speed)))
   >>>
   Arr.second (Causal.osci Wave.saw)
   >>>
   (Causal.envelope * 10)

fly :: IO ()
fly = do
   let slow, fast :: Causal.T (MultiValue.T Float) (MultiValue.T Float)
       slow =
          Filt1.lowpassCausal $<
          Gen.constant (Filt1Core.parameter (1/sampleRate :: Exp Float))
       fast =
          Filt1.lowpassCausal $<
          Gen.constant (Filt1Core.parameter (30/sampleRate :: Exp Float))
   proc <-
      Render.runPlugged
         (PIn.controllerSet d5)
         ((Causal.arrayElement d0 &&&
           (liftA2 (,)
               (Causal.arrayElement d2)
               (liftA2 (,)
                   ((Causal.arrayElement d3 >>> slow)
                    +
                    Causal.arrayElement d1 *
                    (CausalClass.fromSignal (Gen.noise 366210 0.3)
                        >>> fast >>> fast))
                   ((Causal.arrayElement d4 >>> slow)
                    +
                    Causal.arrayElement d1 *
                    (CausalClass.fromSignal (Gen.noise 234298 0.3)
                        >>> fast >>> fast)))
            >>>
            liftA2 Stereo.cons
               (flyChannel (-1,0))
               (flyChannel ( 1,0))))
          >>>
          Causal.envelopeStereo
          >>^
          Stereo.multiValue)
         POut.storableVector
   playFromEvents 0.01 (0.015::Double)
      ((proc :: PIO.T (PCS.T Int Float) (SV.Vector (Stereo.T Float)))
       .
       MCS.filter [
          MCS.controllerExponential Ctrl.volume (0.001, 0.99) 0.2,
          MCS.controllerLinear Ctrl.modulation (0, 5) 2,
          MCS.pitchBend 2 (hertz 250),
          MCS.controllerLinear Ctrl.vectorX (-10, 10) 0,
          MCS.controllerLinear Ctrl.vectorY (-10, 10) 0]
       .
       MCS.fromChannel (ChannelMsg.toChannel 0))


flyChannelPacked ::
   (ae ~ Exp Vector, al ~ VectorValue) =>
   (ae, ae) -> Causal.T (al, (al, al)) al
flyChannelPacked ear =
   ((snd ^>> moveAround2d 1 0.1 ear >>> Arr.first (negate id))
    &&&
    (Arr.second
         (2 * (CausalP.differentiate 0 *** CausalP.differentiate 0
               >>>
               Causal.map euclideanNorm2d))
     >>>
     Causal.mix))
   >>>
   arr (\((phase,volume), speed) -> (volume, (phase,speed)))
   >>>
   Arr.second (CausalP.osci Wave.saw)
   >>>
   Causal.envelope
   >>>
   CausalP.amplify 10


flyPacked :: IO ()
flyPacked = do
   let slow, fast :: Causal.T VectorValue VectorValue
       slow =
          Filt1.lowpassCausalPacked $<
          Gen.constant (Filt1Core.parameter (1/sampleRate :: Exp Float))
       fast =
          Filt1.lowpassCausalPacked $<
          Gen.constant (Filt1Core.parameter (30/sampleRate :: Exp Float))
   proc <-
      Render.runPlugged
         (PIn.controllerSet d5)
         ((CausalP.arrayElement d0 &&&
           (liftA2 (,)
               (CausalP.arrayElement d2)
               (liftA2 (,)
                  ((CausalP.arrayElement d3 >>> slow)
                   +
                   CausalP.arrayElement d1 *
                   (CausalClass.fromSignal (GenP.noise 366210 0.3)
                        >>> fast >>> fast))
                  ((CausalP.arrayElement d4 >>> slow)
                   +
                   CausalP.arrayElement d1 *
                   (CausalClass.fromSignal (GenP.noise 234298 0.3)
                        >>> fast >>> fast)))
            >>>
            liftA2 Stereo.cons
               (flyChannelPacked (-1,0))
               (flyChannelPacked ( 1,0))))
          >>>
          Causal.envelopeStereo
          >>^
          Stereo.multiValueSerial)
         POut.storableVector
   playFromEvents 0.01 (vectorBlockSize/sampleRate)
      (arr SigStL.unpackStrict
       .
       (proc :: PIO.T (PCS.T Int Float) (SV.Vector (Serial.T VectorSize (Stereo.T Float))))
       .
       arr subsamplePCS
       .
       MCS.filter [
          MCS.controllerExponential Ctrl.volume (0.001, 0.99) 0.2,
          MCS.controllerLinear Ctrl.modulation (0, 5) 2,
          MCS.pitchBend 2 (hertz 250),
          MCS.controllerLinear Ctrl.vectorX (-10, 10) 0,
          MCS.controllerLinear Ctrl.vectorY (-10, 10) 0]
       .
       MCS.fromChannel (ChannelMsg.toChannel 0))
