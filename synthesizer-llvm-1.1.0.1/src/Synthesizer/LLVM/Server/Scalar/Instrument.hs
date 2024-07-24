module Synthesizer.LLVM.Server.Scalar.Instrument (
   ping,
   pingDur,
   pingDurTake,
   pingRelease,
   pingStereoRelease,
   tine,
   tineStereo,
   softString,

   -- * for testing
   dummy,
   ) where

import Synthesizer.LLVM.Server.Common
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.Wave as WaveL
import Synthesizer.Causal.Class (($<), ($>), ($*))

import qualified LLVM.DSL.Expression as Expr
import qualified LLVM.Extra.Multi.Value as MultiValue
import LLVM.DSL.Expression (Exp)

import qualified Synthesizer.MIDI.EventList as Ev
import Synthesizer.MIDI.Storable (chunkSizesFromLazyTime)

import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SigStV

import Control.Applicative (liftA, liftA2)
import Data.Semigroup ((<>))

import NumericPrelude.Numeric (zero, round, (+))
import Prelude hiding (Real, round, break, (+))


pingSig ::
   SampleRate (Exp Real) -> Exp Real -> Exp Real -> Sig.T (MultiValue.T Real)
pingSig =
   wrapped $ \(Number vel) (Frequency freq) ->
   constant time 0.2 $ \halfLife _sr ->
      Causal.envelope
         $< Sig.exponential2 halfLife (amplitudeFromVelocity vel)
         $* Sig.osci WaveL.saw zero freq

ping :: IO (SigSt.ChunkSize -> SampleRate Real -> Real -> Real -> SigSt.T Real)
ping = Render.run pingSig

pingDur :: IO (Instrument Real Real)
pingDur =
   fmap (\sound sr vel freq -> pioApplyToLazyTime $ sound sr vel freq) $
   CausalRender.run (\sr vel freq -> Causal.fromSignal $ pingSig sr vel freq)

pingDurTake :: IO (SigSt.ChunkSize -> Instrument Real Real)
pingDurTake =
   fmap (\sound chunkSize sr vel freq dur ->
      SigStV.take (chunkSizesFromLazyTime dur) $
      sound chunkSize sr vel freq) ping

dummy :: SigSt.ChunkSize -> Instrument Real Real
dummy chunkSize =
   \ _sr vel freq dur ->
      SigStV.take (chunkSizesFromLazyTime dur) $
      SigSt.repeat chunkSize (vel + 1e-3*freq)



pingReleaseEnvelope ::
   IO (Real -> Real -> SigSt.ChunkSize ->
       SampleRate Real -> Real -> Ev.LazyTime -> SigSt.T Real)
pingReleaseEnvelope =
   liftA2
      (\pressed release decay rel chunkSize sr vel dur ->
         SigStL.continue
            (pioApplyToLazyTime (pressed sr decay vel) dur)
            (\x -> release chunkSize sr rel x))
      (CausalRender.run $
       wrapped $ \(Time halfLife) (Number velocity) (SampleRate _sr) ->
       Causal.fromSignal
         (Sig.exponential2 halfLife (amplitudeFromVelocity velocity)))
      (Render.run $
       wrapped $ \(Time release) (Number amplitude) (SampleRate _sr) ->
         Causal.take (Expr.roundToIntFast (release*3)) $*
         Sig.exponential2 release amplitude)

pingRelease :: IO (Real -> Real -> SigSt.ChunkSize -> Instrument Real Real)
pingRelease =
   liftA2
      (\osc env dec rel chunkSize sr vel freq dur ->
         pioApply (osc sr freq) (env dec rel chunkSize sr vel dur))
      (CausalRender.run $ frequency $+ \freq _sr ->
         Causal.envelope $> Sig.osci WaveL.saw zero freq)
      pingReleaseEnvelope

pingStereoRelease ::
   IO (Real -> Real -> SigSt.ChunkSize -> Instrument Real (Stereo.T Real))
pingStereoRelease =
   liftA2
      (\osc env dec rel chunkSize sr vel freq dur ->
         pioApply (osc sr freq) (env dec rel chunkSize sr vel dur))
      (CausalRender.run $ frequency $+ \freq _sr ->
         Stereo.multiValue <$>
         Causal.envelopeStereo $>
            liftA2 Stereo.cons
               (Sig.osci WaveL.saw zero (0.999*freq))
               (Sig.osci WaveL.saw zero (1.001*freq)))
      pingReleaseEnvelope



tine :: IO (Real -> Real -> SigSt.ChunkSize -> Instrument Real Real)
tine =
   liftA2
      (\osc env dec rel chunkSize sr vel freq dur ->
         pioApply (osc sr vel freq) (env dec rel chunkSize sr 0 dur))
      (CausalRender.run $
       wrapped $ \(Number vel) (Frequency freq) ->
       constant time 1 $ \halfLife _sr ->
         Causal.envelope $>
         (Causal.osci WaveL.approxSine2
            $> Sig.constant freq
            $* (Causal.envelope
                  $< Sig.exponential2 halfLife (vel+1)
                  $* Sig.osci WaveL.approxSine2 zero (2*freq))))
      pingReleaseEnvelope

tineStereo ::
   IO (Real -> Real -> SigSt.ChunkSize -> Instrument Real (Stereo.T Real))
tineStereo =
   liftA2
      (\osc env dec rel chunkSize sr vel freq dur ->
         pioApply (osc sr vel freq) (env dec rel chunkSize sr 0 dur))
      (CausalRender.run $
       wrapped $ \(Number vel) (Frequency freq) ->
       constant time 1 $ \halfLife _sr ->
         let chanOsci d =
               Causal.osci WaveL.approxSine2 $> Sig.constant (freq*d)
         in Stereo.multiValue <$>
            Causal.envelopeStereo $>
               (liftA2 Stereo.cons (chanOsci 0.995) (chanOsci 1.005) $*
                  (Causal.envelope
                     $< Sig.exponential2 halfLife (vel+1)
                     $* Sig.osci WaveL.approxSine2 zero (2*freq))))
      pingReleaseEnvelope



softStringReleaseEnvelope ::
   IO (Real -> SampleRate Real -> Real -> Ev.LazyTime -> SigSt.T Real)
softStringReleaseEnvelope =
   liftA
      (\env attackTime (SampleRate sampleRate) vel dur ->
         let attackTimeInt = round (attackTime * sampleRate)
             {-
             release <- take attackTime beginning
             would yield a space leak, thus we first split 'beginning'
             and then concatenate it again
             -}
             {-
             We can not easily generate attack and sustain separately,
             because we want to use the chunk structure implied by 'dur'.
             -}
             (attack, sustain) =
                SigSt.splitAt attackTimeInt $
                pioApplyToLazyTime
                   (env
                      (fromIntegral attackTimeInt :: Word)
                      (amplitudeFromVelocity vel))
                   dur
             release = SigSt.reverse attack
         in attack <> sustain <> release)
      (CausalRender.run $ \attackTime amp -> Causal.fromSignal $
       Sig.amplify amp (Sig.parabolaFadeIn attackTime) <> Sig.constant amp)

softString :: IO (Instrument Real (Stereo.T Real))
softString =
   liftA2
      (\osc env sr vel freq dur -> pioApply (osc sr freq) (env 1 sr vel dur))
      (CausalRender.run $ frequency $+ \freq _sr ->
       let osci d = Sig.osci WaveL.saw zero (d * freq)
       in Stereo.multiValue <$>
          Causal.envelopeStereo $>
            liftA2 Stereo.cons
               (osci 1.005 + osci 0.998)
               (osci 1.002 + osci 0.995))
      softStringReleaseEnvelope
