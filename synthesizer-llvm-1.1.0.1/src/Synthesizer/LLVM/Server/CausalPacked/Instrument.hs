{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{- |
This module contains some instruments with Causal arrow interface.
The interface is a bit low-level
since you have to write the transformations of the Haskell-side
separately from the computations on the LLVM side.
A nicer integration is used in
"Synthesizer.LLVM.Server.CausalPacked.InstrumentPlug".
However, we preserve this module in order to show
how things work internally.
-}
module Synthesizer.LLVM.Server.CausalPacked.Instrument (
   ping,
   pingRelease,
   helixSound,
   pingStereoReleaseFM,
   filterSawStereoFM,
   tineStereoFM,
   bellNoiseStereoFM,
   wind,
   windPhaser,
   softStringShapeFM, cosineStringStereoFM,
   arcSawStringStereoFM, arcSineStringStereoFM,
   arcSquareStringStereoFM, arcTriangleStringStereoFM,
   fmStringStereoFM,
   sampledSound, sampledSoundMono,
   Control, DetuneBendModControl, WithEnvelopeControl, StereoChunk,
   Frequency, Time,
   pingControlledEnvelope, stringControlledEnvelope,
   reorderEnvelopeControl,
   frequencyControl, zipEnvelope,
   ) where

import Synthesizer.LLVM.Server.Packed.Instrument (stereoNoise)
import Synthesizer.LLVM.Server.CausalPacked.Common (transposeModulation)
import Synthesizer.LLVM.Server.CommonPacked
import Synthesizer.LLVM.Server.Common hiding
         (Instrument, Frequency, Time, Control, transposeModulation)
import Synthesizer.LLVM.Server.Common (Arg(Frequency, Time))

import qualified Synthesizer.LLVM.Server.SampledSound as Sample
import qualified Synthesizer.LLVM.Storable.Process as PSt
import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.CausalIO.Gate as Gate
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.LLVM.Filter.Universal as UniFilter
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Causal.Exponential2 as Exp
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame as Frame
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Causal.Helix as Helix
import qualified Synthesizer.LLVM.Causal.Functional as F
import qualified Synthesizer.LLVM.Causal.ControlledPacked as CtrlPS
import qualified Synthesizer.LLVM.Causal.Render as Render
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import qualified Synthesizer.LLVM.Wave as WaveL
import Synthesizer.LLVM.Causal.Functional (($&), (&|&))
import Synthesizer.LLVM.Causal.Process (($<), ($>), ($<#))

import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.MIDI as MIDIL
import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.Causal.Class as CausalClass
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Zip as Zip
import qualified Data.EventList.Relative.BodyTime as EventListBT

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp, (<=*), (>*))

import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Control.Applicative.HT as App
import qualified Control.Monad.HT as M
import Control.Arrow (Arrow, arr, first, second, (&&&), (<<^), (^<<))
import Control.Category (id, (.))
import Control.Applicative (liftA2, liftA3, (<$>))
import Control.Functor.HT (unzip)

import qualified Data.Traversable as Trav
import Data.Semigroup ((<>))
import Data.Monoid (mappend)
import Data.Tuple.HT (mapPair)

import qualified Number.DimensionTerm as DN

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (id, unzip, (.))


type Instrument a sig = SampleRate a -> MIO.Instrument a sig

type Control = EventListBT.T PC.ShortStrictTime

type Time = DN.Time Real
type Frequency = DN.Frequency Real

type Chunk = SV.Vector Vector
type StereoChunk = SV.Vector (Stereo.T Vector)
type BendModControl = Control (BM.T Real)
type DetuneBendModControl = Zip.T (Control Real) (Control (BM.T Real))

type PIOId a = PIO.T a a



frequencyFromBendModulationPacked ::
   Exp Real ->
   F.T inp (MultiValue.T (BM.T Real)) ->
   F.T inp VectorValue
frequencyFromBendModulationPacked speed fm =
   MIDIL.frequencyFromBendModulationPacked speed $& (BM.unMultiValue <$> fm)

stereoFrequenciesFromDetuneBendModulation ::
   Exp Real ->
   (F.T inp (MultiValue.T Real),
    F.T inp (MultiValue.T (BM.T Real))) ->
   F.T inp (Stereo.T VectorValue)
stereoFrequenciesFromDetuneBendModulation speed (detune, freq) =
   Causal.envelopeStereo $&
      frequencyFromBendModulationPacked speed freq
      &|&
      (Causal.map (fmap Serial.upsample) $&
       liftA2 Stereo.cons (one + detune) (one - detune))


frequencyFromSampleRate :: SampleRate a -> DN.Frequency a
frequencyFromSampleRate (SampleRate sr) = DN.frequency sr

halfLifeControl ::
   (Functor f) =>
   SampleRate Real ->
   f Time ->
   f (Exp.ParameterPacked Vector)
halfLifeControl sr =
   fmap (Exp.parameterPackedPlain .
         flip DN.mulToScalar (frequencyFromSampleRate sr))

frequencyControl ::
   (Functor f) =>
   SampleRate Real ->
   f Frequency ->
   f Real
frequencyControl sr =
   fmap (flip DN.divToScalar $ frequencyFromSampleRate sr)

takeThreshold :: Exp Real -> Causal.T VectorValue VectorValue
takeThreshold threshold =
   Causal.takeWhile (\y -> threshold <=* Serial.subsample y)


type EnvelopeControl =
        Zip.T MIO.GateChunk
           (Zip.T (Control Time) (Control Time))

type WithEnvelopeControl remainder =
        Zip.T MIO.GateChunk
           (Zip.T
              (Zip.T (Control Time) (Control Time))
              remainder)

reorderEnvelopeControl ::
   (Arrow arrow, CutG.Read remainder) =>
   arrow
      (WithEnvelopeControl remainder)
      (Zip.T EnvelopeControl remainder)
reorderEnvelopeControl =
   arr $ \(Zip.Cons gate (Zip.Cons times ctrl)) ->
      Zip.consChecked "ping gate ctrl"
         (Zip.consChecked "ping gate times" gate times) ctrl


zipEnvelope ::
   (Arrow arrow, CutG.Transform a, CutG.Transform b) =>
   arrow EnvelopeControl a ->
   arrow (WithEnvelopeControl b) (Zip.T a b)
zipEnvelope env =
   Zip.arrowFirstShorten env
   .
   reorderEnvelopeControl


ping :: IO (Instrument Real Chunk)
ping =
   fmap (\proc sampleRate vel freq ->
      proc sampleRate vel freq
      .
      Gate.toStorableVector) $
   Render.run $
   wrapped $ \(Number vel) (Frequency freq) ->
   constant time 0.2 $ \halfLife _sr ->
      Causal.fromSignal $
         SigPS.exponential2 halfLife (amplitudeFromVelocity vel)
         *
         SigPS.osci WaveL.saw zero freq


pingReleaseEnvelope ::
   IO (Real -> Real ->
       SampleRate Real -> Real ->
       PIO.T MIO.GateChunk Chunk)
pingReleaseEnvelope =
   liftA2
      (\sustain release dec rel sr vel ->
         PSt.continuePacked
            (sustain sr dec vel
             .
             Gate.toChunkySize)
            (\y ->
               release sr rel y
               .
               Gate.allToChunkySize))
      (Render.run $
       wrapped $ \(Time decay) (Number vel) (SampleRate _sr) ->
         Causal.fromSignal $
         SigPS.exponential2
            -- FixMe: is division vectorSize correct?
            (decay / fromIntegral vectorSize) (amplitudeFromVelocity vel))
      (Render.run $
       wrapped $ \(Time releaseHL) (Number level) ->
       constant time 1 $ \releaseTime _sr ->
         Causal.take
            (Expr.roundToIntFast $ releaseTime / fromIntegral vectorSize)
         .
         Causal.fromSignal (SigPS.exponential2 releaseHL level))

pingRelease :: IO (Real -> Real -> Instrument Real Chunk)
pingRelease =
   liftA2
      (\osci envelope dec rel sr vel freq ->
         osci sr freq
         .
         envelope dec rel sr vel)
      (Render.run $
       wrapped $ \(Frequency freq) (SampleRate _sr) ->
         Causal.envelope $> SigPS.osci WaveL.saw zero freq)
      pingReleaseEnvelope


pingControlledEnvelope ::
   Maybe Real ->
   IO (SampleRate Real -> Real ->
       PIO.T EnvelopeControl Chunk)
pingControlledEnvelope threshold =
   liftA2
      (\sustain release sr vel ->
         PSt.continuePacked
            (sustain sr vel
             .
             Gate.shorten
             .
             Zip.arrowSecond (arr (halfLifeControl sr . Zip.first)))
            (\y ->
             release sr y
             <<^
             halfLifeControl sr . Zip.second . Zip.second))
      (Render.run $
       wrapped $ \(Number vel) (SampleRate _sr) ->
         Exp.causalPacked (amplitudeFromVelocity vel)
            <<^ Exp.unMultiValueParameterPacked)
      (Render.run $
       wrapped $ \(Number level) (SampleRate _sr) ->
         let expo = Exp.causalPacked level <<^ Exp.unMultiValueParameterPacked
         in  case threshold of
                Just y -> takeThreshold (Expr.cons y) . expo
                Nothing -> expo)


pingStereoReleaseFM ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T
                (Zip.T (Control Real) (Control Time))
                (Zip.T
                   (Zip.T (Control Real) (Control Time))
                   DetuneBendModControl)))
          StereoChunk)
pingStereoReleaseFM =
   liftA2
      (\osc env sr vel freq ->
         osc sr
         .
         Zip.arrowSecond
            (Zip.arrowSplit
               (Zip.arrowSecond $ arr $ halfLifeControl sr)
               ((Zip.arrowSecond $ Zip.arrowSecond $
                   arr $ transposeModulation sr freq)
                .
                (Zip.arrowFirst $ Zip.arrowSecond $
                   arr $ halfLifeControl sr)))
         .
         zipEnvelope (env sr vel))
      (Render.run $
       constant frequency 10 $ \speed _sr ->
         (arr Stereo.multiValue
          .
          Causal.envelopeStereo
          .
          second
             (F.withArgs $ \((shape0,shapeDecay),((phase,phaseDecay),fm)) ->
              let shape = Causal.map Serial.upsample $& shape0
                  shapeCtrl =
                     1/pi + (shape-1/pi) *
                        (Exp.causalPacked 1
                              <<^ Exp.unMultiValueParameterPacked
                           $& shapeDecay)
                  freqs = stereoFrequenciesFromDetuneBendModulation speed fm
                  expo =
                     (Causal.map Serial.upsample $& phase) *
                     (Exp.causalPacked 1 <<^ Exp.unMultiValueParameterPacked
                        $& phaseDecay)
                  osci ::
                     Causal.T
                        (VectorValue, (VectorValue, VectorValue)) VectorValue
                  osci = CausalPS.shapeModOsci WaveL.rationalApproxSine1
              in  liftA2 Stereo.cons
                     (osci $&  shapeCtrl &|& (expo &|& fmap Stereo.left freqs))
                     (osci $&  shapeCtrl &|&
                                 (negate expo &|& fmap Stereo.right freqs)))))
      (pingControlledEnvelope (Just 0.01))



filterSawStereoFM ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T
                (Zip.T (Control Frequency) (Control Time))
                DetuneBendModControl))
          StereoChunk)
filterSawStereoFM =
   liftA2
      (\osc env sr vel freq ->
         osc sr
         .
         Zip.arrowSecond
            (Zip.arrowSplit
               (Zip.arrowSplit
                  (arr $ frequencyControl sr)
                  (arr $ halfLifeControl sr))
               (Zip.arrowSecond $
                  arr $ transposeModulation sr freq))
         .
         zipEnvelope (env sr vel))
      (Render.run $
       constant frequency 10 $ \speed ->
       constant frequency 100 $ \lowerFreq _sr ->
         (arr Stereo.multiValue
          .
          Causal.envelopeStereo
          .
          second
             (F.withArgs $ \((cutoff,cutoffDecay),fm) ->
              let freqs = stereoFrequenciesFromDetuneBendModulation speed fm
                  {- bound control in order to avoid too low resonant frequency,
                     which makes the filter instable -}
                  expo =
                     takeThreshold lowerFreq $&
                     (Causal.map Serial.upsample $& cutoff) *
                     (Exp.causalPacked 1 <<^ Exp.unMultiValueParameterPacked
                        $& cutoffDecay)
              in  Causal.stereoFromMonoControlled
                     (UniFilter.lowpass ^<< CtrlPS.process)
                  $&
                  ((Causal.quantizeLift
                     (Causal.map
                          (UniFilter.parameter 10
                           .
                           Serial.subsample))
                     $<# (100 / fromIntegral vectorSize :: Real))
                   $&
                   expo)
                  &|&
                  (Causal.stereoFromMono
                     (CausalPS.osci WaveL.saw $< zero) $&
                     freqs))))
      (pingControlledEnvelope (Just 0.01))

tineStereoFM ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T
                (Zip.T (Control Real) (Control Real))
                DetuneBendModControl))
          StereoChunk)
tineStereoFM =
   liftA2
      (\osc env sr vel freq ->
         osc sr vel
         .
         (Zip.arrowSecond $ Zip.arrowSecond $
          Zip.arrowSecond $
            arr $ transposeModulation sr freq)
         .
         zipEnvelope (env sr vel))
      (Render.run $
       wrapped $ \(Number vel) ->
       constant frequency 5 $ \speed ->
       constant time 1 $ \halfLife _sr ->
         (arr Stereo.multiValue
          .
          Causal.envelopeStereo
          .
          second
             (F.withArgs $ \((index0,depth0), fm) ->
              let freqs = stereoFrequenciesFromDetuneBendModulation speed fm
                  index = Causal.map Serial.upsample $& index0
                  depth = Causal.map Serial.upsample $& depth0
                  expo = F.fromSignal $ SigPS.exponential2 halfLife (1 + vel)
                  osci indexDepth freq =
                     case unzip indexDepth of
                        (index1,depth1) ->
                           CausalPS.osci WaveL.approxSine2 $&
                              expo * depth1 *
                                 (CausalPS.osci WaveL.approxSine2
                                  $& zero &|& index1*freq)
                              &|&
                              freq
              in  stereoFromMonoControlled osci (index&|&depth) freqs)))
      (pingControlledEnvelope (Just 0.01))

{- |
'Stereo.liftApplicative' specialised to 'T'.

Should be moved to Functional utility module.
(Functional module itself would cause cyclic dependency.)
-}
stereoFromMonoControlled,
      _stereoFromMonoControlledArgs,
      _stereoFromMonoControlledGrounded,
      _stereoFromMonoControlledGuided,
      _stereoFromMonoControlledPrepared,
      _stereoFromMonoControlledPrepared2 ::
   (Tuple.Phi a, Tuple.Phi b, Tuple.Phi c) =>
   (Tuple.Undefined a, Tuple.Undefined b, Tuple.Undefined c) =>
   (forall inp0. F.T inp0 c -> F.T inp0 a -> F.T inp0 b) ->
   F.T inp c -> F.T inp (Stereo.T a) -> F.T inp (Stereo.T b)
stereoFromMonoControlled proc ctrl stereo =
   Causal.stereoFromMonoControlled
      (F.compile $ uncurry proc $ unzip $ F.lift id)
   $&
   ctrl &|& stereo

_stereoFromMonoControlledArgs proc ctrl stereo =
   Causal.stereoFromMonoControlled
      (F.withArgs (uncurry proc) <<^ mapPair (F.AnyArg, F.AnyArg))
   $&
   ctrl &|& stereo

_stereoFromMonoControlledGrounded proc ctrl stereo =
   Causal.stereoFromMonoControlled
      (F.withGroundArgs $ \(F.Ground c, F.Ground s) -> proc c s)
   $&
   ctrl &|& stereo

_stereoFromMonoControlledGuided proc ctrl stereo =
   Causal.stereoFromMonoControlled
      (F.withGuidedArgs (F.atom, F.atom) (uncurry proc))
   $&
   ctrl &|& stereo

_stereoFromMonoControlledPrepared proc ctrl stereo =
   Causal.stereoFromMonoControlled
      (F.withPreparedArgs (F.pairArgs F.atomArg F.atomArg) (uncurry proc))
   $&
   ctrl &|& stereo

_stereoFromMonoControlledPrepared2 proc ctrl stereo =
   Causal.stereoFromMonoControlled
      (F.withPreparedArgs2 F.atomArg F.atomArg proc)
   $&
   ctrl &|& stereo


type RealValue = MultiValue.T Real

bellNoiseStereoFM ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T
                (Zip.T (Control Real) (Control Real))
                DetuneBendModControl))
          StereoChunk)
bellNoiseStereoFM =
   liftA3
      (\osc env envInf sr vel freq ->
         osc sr
         .
         (Zip.arrowSecond $ Zip.arrowSecond $
          Zip.arrowSecond $
            arr $ transposeModulation sr freq)
         .
         zipEnvelope
            (Zip.arrowFanoutShorten
               (env sr (vel*0.5))
               (let shortenTimes ::
                       Real ->
                       PIOId (Zip.T (Control Time) (Control Time))
                    shortenTimes n =
                       let rn = recip n
                       in  (Zip.arrowFirst $ arr $ fmap $ DN.scale rn)
                           .
                           (Zip.arrowSecond $ arr $ fmap $ DN.scale rn)
                in  PIO.zip
                      (envInf sr (vel*2)
                       .
                       Zip.arrowSecond (shortenTimes 4))
                      (envInf sr (vel*4)
                       .
                       Zip.arrowSecond (shortenTimes 7)))))
      (Render.run $
       constant noiseReference 20000 $ \noiseRef ->
       constant frequency 5 $ \speed _sr ->
         (F.withArgs $ \((env1,(env4,env7)),((noiseAmp0,noiseReson),fm)) ->
          let noiseAmp = Causal.map Serial.upsample $& noiseAmp0
              noiseParam ::
                  Causal.T
                     (RealValue, RealValue)
                     (Moog.Parameter TypeNum.D8 RealValue)
              noiseParam =
                 Causal.quantizeLift
                       (Causal.zipWith (Moog.parameter TypeNum.d8))
                    $<# (100 / fromIntegral vectorSize :: Real)
              noise = F.fromSignal (SigPS.noise 12 noiseRef)
              freqs = stereoFrequenciesFromDetuneBendModulation speed fm
              osci amp env n =
                 CausalPS.amplifyStereo amp $&
                 Causal.envelopeStereo $&
                 env &|&
                 (Causal.stereoFromMono
                    (CausalPS.osci WaveL.approxSine4 $< zero)
                  $&
                  CausalPS.amplifyStereo n
                  $&
                  freqs)
          in Stereo.multiValue <$>
              (Causal.envelopeStereo $&
                 (noiseAmp * env1)
                 &|&
                 Stereo.liftApplicative
                    (\freq ->
                       CtrlPS.process $&
                          (noiseParam $& noiseReson &|&
                           (Causal.map Serial.subsample $& freq))
                          &|&
                          noise)
                    freqs)
              + osci 1.00 env1 1
              + osci 0.10 env4 4
              + osci 0.01 env7 7))
      (pingControlledEnvelope (Just 0.01))
      (pingControlledEnvelope Nothing)



stringControlledEnvelope ::
   IO (SampleRate Real -> Real ->
       PIO.T EnvelopeControl Chunk)
stringControlledEnvelope =
   liftA3
      (\attack sustain release sr vel ->
         let amp = amplitudeFromVelocity vel
         in  PSt.continuePacked
                ((attack sr amp <>
                  {- we could also feed the sustain process
                     with a signal with sample type () -}
                  sustain sr amp)
                 .
                 Gate.shorten
                 .
                 Zip.arrowSecond (arr (halfLifeControl sr . Zip.first)))
                (\y ->
                 release sr y
                 <<^
                 halfLifeControl sr . Zip.second . Zip.second))
      (Render.run $
       wrapped $ \(Number amp) (SampleRate _sr) ->
             Causal.fromSignal (SigPS.constant amp)
             -
             takeThreshold 1e-4
             .
             Exp.causalPacked amp <<^ Exp.unMultiValueParameterPacked)
      (Render.run $
       wrapped $ \(Number amp) (SampleRate _sr) ->
             Causal.fromSignal (SigPS.constant amp))
      (Render.run $
       wrapped $ \(Number level) (SampleRate _sr) ->
             takeThreshold 0.01
             .
             Exp.causalPacked level <<^ Exp.unMultiValueParameterPacked)


windCore ::
   F.T a (MultiValue.T Real) ->
   F.T a (MultiValue.T (BM.T Real)) ->
   SampleRate (Exp Real) ->
   F.T a (Stereo.T VectorValue)
windCore reson fm =
   constant frequency 0.2 $ \speed sr ->
   let modu =
          Causal.map Serial.subsample $&
          (fmap (`asTypeOf` (undefined :: VectorValue)) $
           frequencyFromBendModulationPacked speed fm)
   in  Causal.stereoFromMonoControlled CtrlPS.process $&
          (Causal.zipWith (Moog.parameter TypeNum.d8) $&  reson &|& modu)
          &|&
          F.fromSignal (stereoNoise sr)

wind ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl DetuneBendModControl)
          StereoChunk)
wind =
   liftA2
      (\osc env sr vel freq ->
         osc sr
         .
         (Zip.arrowSecond $ Zip.arrowSecond $
            arr $ transposeModulation sr freq)
         .
         zipEnvelope (env sr vel))
      (Render.run $ \sr ->
         F.withArgs $ \(env,(reson,fm)) ->
            Stereo.multiValue <$>
            Causal.envelopeStereo $& env &|& windCore reson fm sr)
      stringControlledEnvelope


windPhaser ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T (Control Real)
                (Zip.T (Control Frequency) DetuneBendModControl)))
          StereoChunk)
windPhaser =
   liftA2
      (\osc env sr vel freq ->
         osc sr
         .
         (Zip.arrowSecond $ Zip.arrowSecond $
          Zip.arrowSplit
             (arr $ fmap (Allpass.flangerParameter TypeNum.d8) .
                    frequencyControl sr)
             (Zip.arrowSecond $
              arr $ transposeModulation sr freq))
         .
         zipEnvelope (env sr vel))
      (Render.run $ \sr ->
         (F.withArgs $ \(env,(phaserMix0,(phaserFreq,(reson,fm)))) ->
          let phaserMix = Causal.map Serial.upsample $& phaserMix0
              noise = windCore reson fm sr

          in Stereo.multiValue <$>
              Causal.envelopeStereo $&
                 env &|&
                 ((Causal.envelopeStereo $& (1 - phaserMix) &|& noise)
                  +
                  (Causal.envelopeStereo $&
                     phaserMix &|&
                     (Stereo.arrowFromMonoControlled CtrlPS.process $&
                        (Allpass.cascadeParameterUnMultiValue <$> phaserFreq)
                        &|& noise)))))
      stringControlledEnvelope


phaserOsci ::
   (Exp Real -> Exp Real -> Causal.T a VectorValue) ->
   Causal.T a (Stereo.T VectorValue)
phaserOsci osci =
   CausalPS.amplifyStereo 0.25
   .
   Trav.traverse sumNested
      (Stereo.cons
         (zipWith osci [0.1, 0.7, 0.2, 0.3] [1.0, -0.4, 0.5, -0.7])
         (zipWith osci [0.4, 0.9, 0.6, 0.5] [0.4, -1.0, 0.7, -0.5]))


type
   StringInstrument =
      SampleRate Real -> Real -> Real ->
      PIO.T
         (WithEnvelopeControl
            (Zip.T (Control Real) DetuneBendModControl))
         StereoChunk

softStringShapeCore ::
   (forall r.
    VectorValue ->
    VectorValue ->
    LLVM.CodeGenFunction r VectorValue) ->
   IO StringInstrument
softStringShapeCore wave =
   liftA2
      (\osc env sr vel freq ->
         osc sr
         .
         (Zip.arrowSecond $ Zip.arrowSecond $
          Zip.arrowSecond $
            arr $ transposeModulation sr freq)
         .
         zipEnvelope (env sr vel))
      (Render.run $
       constant frequency 5 $ \speed _sr ->
         (arr Stereo.multiValue
          .
          Causal.envelopeStereo
          .
          second
             (F.withArgs $ \(shape0,(det0,fm)) ->
              let det = Causal.map Serial.upsample $& det0
                  shape = Causal.map Serial.upsample $& shape0
                  modu = frequencyFromBendModulationPacked speed fm
                  osci ::
                     Exp Real ->
                     Exp Real ->
                     Causal.T
                        (VectorValue,
                              {- wave shape parameter -}
                         (VectorValue, VectorValue)
                              {- detune, frequency modulation -})
                        VectorValue
                  osci p d =
                     CausalPS.shapeModOsci wave
                     .
                     second
                        (CausalClass.feedFst (SigPS.constant p)
                         .
                         Causal.envelope
                         .
                         first (one + CausalPS.amplify d))

              in  phaserOsci osci $&  shape &|& det &|& modu)))
      stringControlledEnvelope

arcStringStereoFM ::
   (forall r.
    VectorValue ->
    LLVM.CodeGenFunction r VectorValue) ->
   IO StringInstrument
arcStringStereoFM wave =
   softStringShapeCore
      (\k p ->
         M.liftJoin2 Frame.amplifyMono
            (WaveL.approxSine4 =<< WaveL.halfEnvelope p)
            (wave =<< WaveL.replicate k p))

softStringShapeFM, cosineStringStereoFM,
   arcSawStringStereoFM, arcSineStringStereoFM,
   arcSquareStringStereoFM, arcTriangleStringStereoFM ::
      IO StringInstrument
softStringShapeFM =
   softStringShapeCore WaveL.rationalApproxSine1
cosineStringStereoFM =
   softStringShapeCore
      (\k p -> WaveL.approxSine2 =<< WaveL.replicate k p)
arcSawStringStereoFM = arcStringStereoFM WaveL.saw
arcSineStringStereoFM = arcStringStereoFM WaveL.approxSine2
arcSquareStringStereoFM = arcStringStereoFM WaveL.square
arcTriangleStringStereoFM = arcStringStereoFM WaveL.triangle


fmStringStereoFM ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T
                (Zip.T (Control Real) (Control Real))
                DetuneBendModControl))
          StereoChunk)
fmStringStereoFM =
   liftA2
      (\osc env sr vel freq ->
         osc sr
         .
         (Zip.arrowSecond $ Zip.arrowSecond $
          Zip.arrowSecond $
            arr $ transposeModulation sr freq)
         .
         zipEnvelope (env sr vel))
      (Render.run $
       constant frequency 5 $ \speed _sr ->
         (F.withArgs $ \(env,((depth0,shape0),(det0,fm))) ->
          let det = Causal.map Serial.upsample $& det0
              shape = Causal.map Serial.upsample $& shape0
              depth =
                 Causal.envelope $&
                    env &|&
                    (Causal.map Serial.upsample $& depth0)
              modu = frequencyFromBendModulationPacked speed fm

              osci ::
                 Exp Real ->
                 Exp Real ->
                 Causal.T
                    ((VectorValue, VectorValue)
                          {- phase modulation depth, modulator distortion -},
                     (VectorValue, VectorValue)
                          {- detune, frequency modulation -})
                    VectorValue
              osci p d =
                 CausalPS.osci WaveL.approxSine2
                 .
                 ((Causal.envelope
                  .
                  second
                     (CausalPS.shapeModOsci WaveL.rationalApproxSine1
                        . second (CausalClass.feedFst (SigPS.constant p)))
                  <<^
                  (\((dp, ds), f) -> (dp, (ds, f))))
                  &&& arr snd)
                 .
                 second
                    (Causal.envelope .
                     first (one + CausalPS.amplify d))

          in  Stereo.multiValue <$>
              Causal.envelopeStereo $&
                 env &|&
                 (phaserOsci osci $&  (depth &|& shape) &|& (det &|& modu))))
      stringControlledEnvelope



sampledSound ::
   IO (Sample.T ->
       SampleRate Real -> Real -> Real ->
       PIO.T
          (Zip.T MIO.GateChunk DetuneBendModControl)
          StereoChunk)
sampledSound =
   liftA2
      (\osc freqMod smp sr vel freq ->
         let pos = Sample.positions smp
         in  assembleParts osc smp sr vel
             .
             Zip.arrowSecond
                ((id :: PIOId StereoChunk)
                 .
                 freqMod sr
                 .
                 (Zip.arrowSecond $ arr $
                    transposeModulation sr (freq * Sample.period pos))))
      (Render.run $ \sr (amp, smp) ->
         Stereo.multiValue
         ^<<
         Causal.stereoFromMono (resamplingProc sr (amp, smp))
         <<^
         Stereo.unMultiValue)
      (Render.run $
       constant frequency 3 $ \speed _sr ->
         fmap Stereo.multiValue $
         F.withArgs $ stereoFrequenciesFromDetuneBendModulation speed)


{- |
mainly for testing purposes
-}
sampledSoundMono ::
   IO (Sample.T ->
       SampleRate Real -> Real -> Real ->
       PIO.T (Zip.T MIO.GateChunk BendModControl) Chunk)
sampledSoundMono =
   liftA2
      (\osc freqMod smp sr vel freq ->
         let pos = Sample.positions smp
         in  assembleParts osc smp sr vel
             .
             Zip.arrowSecond
                ((id :: PIOId Chunk)
                 .
                 freqMod sr
                 .
                 (arr $ transposeModulation sr (freq * Sample.period pos))))
      (Render.run resamplingProc)
      (Render.run $
       constant frequency 3 $ \speed _sr ->
         F.withArgs $ frequencyFromBendModulationPacked speed)

{-
We split the frequency modulation signal
in order to get a smooth frequency modulation curve.
Without (periodic) frequency modulation
we could just split the piecewise constant control curve @fm@.
-}
assembleParts ::
   (CutG.Transform a, CutG.Transform b) =>
   (SampleRate Real -> (Real, SVL.Vector Real) -> PIO.T a b) ->
   Sample.T -> SampleRate Real -> Real ->
   PIO.T (Zip.T (Gate.Chunk gate) a) b
assembleParts osc smp sr vel =
   let pos = Sample.positions smp
       amp = 2 * amplitudeFromVelocity vel
       (attack, sustain, release) = Sample.parts smp
       osci smpBody = osc sr (amp, smpBody)
   in  mappend
          (osci
             (attack `SigSt.append`
              SVL.cycle (SigSt.take (Sample.loopLength pos) sustain))
           .
           Gate.shorten)
          (osci release <<^ Zip.second)

resamplingProc ::
   SampleRate (Exp Real) ->
   (Exp Real, Sig.T (MultiValue.T Real)) ->
   Causal.T VectorValue VectorValue
resamplingProc _sr (amp, smp) =
       CausalPS.amplify amp
       .
       CausalPS.pack
          (Causal.frequencyModulationLinear
             {-
             (Sig.fromStorableVector $
                fmap (SV.concat . SVL.chunks . SVL.take 1000000) smp)
             -}
             smp
             {- (Sig.osci WaveL.saw 0 (1 / 324 {- samplePeriod smp -})) -})

helixSound ::
   IO (Sample.T ->
       SampleRate Real -> Real -> Real ->
       PIO.T
          (Zip.T MIO.GateChunk
              (Zip.T (Control Real) DetuneBendModControl))
          StereoChunk)
helixSound =
   App.lift4
      (\helix zigZag integrate freqMod smp sr vel freq ->
         let pos = Sample.positions smp
             amp = 2 * amplitudeFromVelocity vel
             rateFactor =
                DN.divToScalar
                   (Sample.sampleRate smp)
                   (frequencyFromSampleRate sr)
             releaseStart =
                fromIntegral $
                Sample.loopStart pos + Sample.loopLength pos
             releaseStop =
                fromIntegral $
                Sample.start pos + Sample.length pos
             poss =
                (fromIntegral $ Sample.start pos,
                 fromIntegral $ Sample.loopStart pos,
                 fromIntegral $ Sample.loopLength pos)
         in  helix sr amp (Sample.period pos)
                (Render.buffer $ SV.concat $ SVL.chunks $ Sample.body smp)
             .
             Zip.arrowFirstShorten
                (mappend
                    (zigZag sr poss . Gate.shorten)
                    (integrate sr (releaseStart, releaseStop)
                        <<^ Zip.second))
             .
             Zip.arrowSecond
                (freqMod sr
                 .
                 (Zip.arrowSecond $ arr $ transposeModulation sr freq))
             .
             arr (\(Zip.Cons gate (Zip.Cons speed fm)) ->
                       Zip.Cons (Zip.Cons gate (fmap (rateFactor*) speed)) fm))
      makeHelix
      makeZigZag
      makeIntegrate
      (Render.run $
       constant frequency 3 $ \speed _sr ->
         fmap Stereo.multiValue $
         F.withArgs $ stereoFrequenciesFromDetuneBendModulation speed)

makeHelix ::
   IO (SampleRate Real -> Real -> Real -> Render.Buffer Real ->
       PIO.T (Zip.T Chunk StereoChunk) StereoChunk)
makeHelix =
   Render.run $
   wrapped $
      \(Number amp) (Number per) (SampleRate _sr) smp ->
           arr Stereo.multiValue
           .
           CausalPS.amplifyStereo amp
           .
           Causal.stereoFromMono
              (Helix.staticPacked
                  Interpolation.linear
                  Interpolation.linear
                  (Expr.roundToIntFast per) per
                  smp
               .
               second (CausalPS.osciCore $< 0))
           .
           arr (\(shape, freq) -> (,) shape <$> Stereo.unMultiValue freq)

makeZigZag ::
   IO (SampleRate Real -> (Real, Real, Real) ->
       PIO.T (Control Real) Chunk)
makeZigZag =
   Render.run $
   wrapped $
      \(Number start, Number loopStart, Number loopLength) (SampleRate _sr) ->
         CausalPS.raise start
         .
         -- CausalPS.pack (Helix.zigZagLong (loopStart-start) loopLength)
         Helix.zigZagLongPacked (loopStart-start) loopLength
         .
         Causal.map Serial.upsample

makeIntegrate ::
   IO (SampleRate Real -> (Real, Real) ->
       PIO.T (Control Real) Chunk)
makeIntegrate =
   Render.run $
   wrapped $
      \(Number start, Number stop) (SampleRate _sr) ->
         Causal.takeWhile (\v -> stop >* Serial.subsample v)
         .
         CausalPS.integrate start
         .
         Causal.map Serial.upsample
