{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{- |
The Instruments in this module have the same causal arrow interface
as the ones in "Synthesizer.LLVM.Server.CausalPacked.Instrument",
but here we use the higher level interface
of the "Synthesizer.LLVM.Causal.FunctionalPlug" module.
-}
module Synthesizer.LLVM.Server.CausalPacked.InstrumentPlug (
   tineStereoFM,
   helixNoise,
   ) where

import Synthesizer.LLVM.Server.CausalPacked.Instrument (
          Control, DetuneBendModControl,
          WithEnvelopeControl, StereoChunk,
          pingControlledEnvelope,
          stringControlledEnvelope,
          reorderEnvelopeControl)
import Synthesizer.LLVM.Server.CausalPacked.Common (transposeModulation)
import Synthesizer.LLVM.Server.CommonPacked (VectorValue)
import Synthesizer.LLVM.Server.Common (
          SampleRate, expSampleRate, Real,
          Arg(Number), wrapped,
          constant, frequency, time)

import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Causal.Helix as Helix
import qualified Synthesizer.LLVM.Causal.FunctionalPlug as FP
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import qualified Synthesizer.LLVM.Wave as WaveL
import Synthesizer.LLVM.Causal.FunctionalPlug (($&), (&|&))

import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.MIDI as MIDIL
import qualified Synthesizer.Zip as Zip

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value as MultiValue

import Control.Category ((.))
import Control.Applicative (liftA2, (<$>))

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (id, (.))


stereoFrequenciesFromDetuneBendModulation ::
   Exp Real ->
   (FP.T p inp (MultiValue.T Real),
    FP.T p inp (MultiValue.T (BM.T Real))) ->
   FP.T p inp (Stereo.T VectorValue)
stereoFrequenciesFromDetuneBendModulation speed (detune, freq) =
   Causal.envelopeStereo $&
      (MIDIL.frequencyFromBendModulationPacked speed $&
         (BM.unMultiValue <$> freq))
      &|&
      (Causal.map (fmap Serial.upsample) $&
       liftA2 Stereo.cons (one + detune) (one - detune))

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
         osc (sr, freq) (sr, vel)
         .
         Zip.arrowFirstShorten (env sr vel)
         .
         reorderEnvelopeControl)
      (FP.withArgs $ \(env, ((index0,depth0), (detune,fm))) pl ->
       (\f -> case Expr.unzip pl of (sr,vel) -> f (expSampleRate sr) vel) $
       wrapped $ \(Number vel) ->
       constant time 1 $ \halfLife ->
       constant frequency 5 $ \speed _sr ->
         let freqs =
                stereoFrequenciesFromDetuneBendModulation
                   speed
                   (FP.plug detune,
                    FP.plug $
                      liftA2 (uncurry transposeModulation) FP.askParameter fm)
             index = Causal.map Serial.upsample $& FP.plug index0
             depth = Causal.map Serial.upsample $& FP.plug depth0
             expo = FP.fromSignal $ SigPS.exponential2 halfLife (1 + vel)
             osci freq =
                CausalPS.osci WaveL.approxSine2 $&
                   expo * depth *
                      (CausalPS.osci WaveL.approxSine2
                       $& zero &|& index*freq)
                   &|&
                   freq
         in fmap Stereo.multiValue $
            Causal.envelopeStereo $&
               FP.plug env &|& Stereo.liftApplicative osci freqs)
      (pingControlledEnvelope (Just 0.01))


helixNoise ::
   IO (SampleRate Real -> Real -> Real ->
       PIO.T
          (WithEnvelopeControl
             (Zip.T (Control Real) DetuneBendModControl))
          StereoChunk)
helixNoise =
   liftA2
      (\osc env sr vel freq ->
         osc (sr, freq) sr
         .
         Zip.arrowFirstShorten (env sr vel)
         .
         reorderEnvelopeControl)
      (FP.withArgs $ \(env, (speed0, (detune,fm))) sr ->
       (\f -> f (expSampleRate sr)) $
       constant frequency 5 $ \modSpeed _sr ->
         let freqs =
                stereoFrequenciesFromDetuneBendModulation
                   modSpeed
                   (FP.plug detune,
                    FP.plug $
                      liftA2 (uncurry transposeModulation) FP.askParameter fm)
             speed = Causal.map Serial.upsample $& FP.plug speed0
         in fmap Stereo.multiValue $
            Causal.envelopeStereo $&
               FP.plug env &|& Stereo.liftApplicative (helixOsci speed) freqs)
      stringControlledEnvelope

helixOsci ::
   FP.T pp inp VectorValue ->
   FP.T pp inp VectorValue ->
   FP.T pp inp VectorValue
helixOsci speed freq =
   CausalPS.pack
      (Helix.dynamicLimited Interpolation.cubic Interpolation.cubic
          64 (64 :: Exp Real) (Sig.noise 66 0.2))
   $&
   speed &|&
   (CausalPS.osciCore $& 0 &|& freq)
