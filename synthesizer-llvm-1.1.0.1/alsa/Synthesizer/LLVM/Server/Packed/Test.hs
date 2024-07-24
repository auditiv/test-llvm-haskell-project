module Synthesizer.LLVM.Server.Packed.Test where

import qualified Synthesizer.LLVM.Server.Packed.Instrument as Instr
import qualified Synthesizer.LLVM.Server.Default as Default
import qualified Synthesizer.LLVM.Server.SampledSound as Sample
import Synthesizer.LLVM.Server.Packed.Instrument (InputArg(Modulation))
import Synthesizer.LLVM.Server.ALSA (makeNote)
import Synthesizer.LLVM.Server.CommonPacked (Vector, vectorSize)
import Synthesizer.LLVM.Server.Common hiding (Instrument)

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Synthesizer.MIDI.PiecewiseConstant as PC
import qualified Synthesizer.MIDI.Generic as Gen

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector.Plain as Serial

import qualified Synthesizer.ALSA.Storable.Play as Play
import Synthesizer.MIDI.Storable (Instrument, chunkSizesFromLazyTime)

import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import Synthesizer.LLVM.Causal.Process (($*))

import qualified Synthesizer.Storable.Cut         as CutSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SVP
import qualified Data.StorableVector.Lazy         as SVL

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT

import Control.Arrow ((<<<), arr)
import Control.Applicative (pure, liftA, liftA2, (<$>))
import Control.Monad.Trans.State (evalState)

import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import Algebra.IntegralDomain (divUp)

import qualified Number.DimensionTerm as DN

import Prelude hiding (Real, round, break)


chunkSize :: SVL.ChunkSize
chunkSize = Play.defaultChunkSize

vectorChunkSize :: SVL.ChunkSize
vectorChunkSize =
   case chunkSize of
      SVL.ChunkSize size ->
         SVL.ChunkSize (divUp size vectorSize)

sampleRatePlain :: Num a => a
sampleRatePlain = case Default.sampleRate of SampleRate r -> r

sampleRate :: SampleRate Real
sampleRate = Default.sampleRate


emptyEvents :: time -> EventList.T time [Event.T]
emptyEvents t =
   let evs = EventList.cons t [] evs
   in  evs


{- |
try to reproduce a space leak
-}
sequencePlain :: IO ()
sequencePlain =
   SVL.writeFile "test.f32" $
--   print $ last $ SVL.chunks $
      CutSt.arrange chunkSize $
      evalState (Gen.sequence Default.channel (error "no sound" :: Instrument Real Real)) $
      emptyEvents 10

sequenceLLVM :: IO ()
sequenceLLVM = do
   arrange <- SigStL.makeArranger
   SVL.writeFile "test.f32" $
--   print $ last $ SVL.chunks $
      arrange vectorChunkSize $
      evalState (Gen.sequence Default.channel (error "no sound" :: Instrument Real Vector)) $
      emptyEvents 10

sequencePitchBendCycle :: IO ()
sequencePitchBendCycle = do
   arrange <- SigStL.makeArranger
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (let -- fm = error "undefined pitch bend"
              fm = EventListBT.cons 1 10 fm
          in  Gen.sequenceModulated fm Default.channel
                 (error "no sound" ::
                     PC.T Real -> Instrument Real Vector)) $
      emptyEvents 10

sequencePitchBendSimple :: IO ()
sequencePitchBendSimple = do
   arrange <- SigStL.makeArranger
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (let fm y = EventListBT.cons y 10 (fm (2-y))
          in  Gen.sequenceModulated (fm 1) Default.channel
                 (error "no sound" ::
                     PC.T Real -> Instrument Real Vector)) $
      emptyEvents 10

sequencePitchBend :: IO ()
sequencePitchBend = do
   arrange <- SigStL.makeArranger
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (do fm <- PC.pitchBend Default.channel 2 0.01
             Gen.sequenceModulated fm Default.channel
                (error "no sound" ::
                    PC.T Real -> Instrument Real Vector)) $
      emptyEvents 10

sequenceModulated :: IO ()
sequenceModulated = do
   arrange <- SigStL.makeArranger
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (do fm <- PC.bendWheelPressure Default.channel 2 0.04 0.03
             Gen.sequenceModulated fm Default.channel
                (error "no sound" ::
                    PC.T (BM.T Real) ->
                    Instrument Real Vector)) $
      emptyEvents 10

sequenceModulatedLong :: IO ()
sequenceModulatedLong = do
   arrange <- SigStL.makeArranger
--   sound <- Instr.softStringReleaseEnvelope $/ sampleRate
   sound <- Instr.softString $/ sampleRate  -- space leak
--   sound <- Instr.pingReleaseEnvelope $/ 1 $/ sampleRate  -- no space leak
--   sound <- Instr.pingRelease $/ 1 $/ 1 $/ sampleRate  -- no space leak
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState (Gen.sequence Default.channel sound) $
      let evs t = EventList.cons t [] (evs (20-t))
      in  EventList.cons 10 [makeNote Event.NoteOn 60] $
          EventList.cons 10 [makeNote Event.NoteOn 64] $
          evs 10

sequenceModulatedLongFM :: IO ()
sequenceModulatedLongFM = do
   arrange <- SigStL.makeArranger
   sound <- Instr.softStringFM
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (do fm <- PC.bendWheelPressure Default.channel 2 0.04 0.03
             Gen.sequenceModulated fm Default.channel
                (\fmlocal -> sound fmlocal $ sampleRate)) $
      let evs t = EventList.cons t [] (evs (20-t))
      in  EventList.cons 10 [makeNote Event.NoteOn 60] $
          EventList.cons 10 [makeNote Event.NoteOn 64] $
          evs 10

sequenceModulatedRepeat :: IO ()
sequenceModulatedRepeat = do
   arrange <- SigStL.makeArranger
   sound <- Instr.softStringFM
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (do fm <- PC.bendWheelPressure Default.channel 2 0.04 0.03
             Gen.sequenceModulated fm Default.channel
                (\fmlocal -> sound fmlocal $ sampleRate)) $
      let evs t =
             EventList.cons t [makeNote Event.NoteOn  60] $
             EventList.cons t [makeNote Event.NoteOff 60] $
             evs (20-t)
      in  evs 10

sequencePress :: IO ()
sequencePress = do
   arrange <- SigStL.makeArranger
--   sound <- Instr.softString $/ sampleRate
--   sound <- Instr.softStringReleaseEnvelope $/ sampleRate
   sound <- Instr.pingReleaseEnvelope $/ 1 $/ 1 $/ vectorChunkSize $/ sampleRate
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (Gen.sequence Default.channel (\ _freq -> sound)) $
      let evs t =
             EventList.cons t [makeNote Event.NoteOn  60] $
             EventList.cons t [makeNote Event.NoteOff 60] $
             evs (20-t)
      in  evs 10


sampledSoundTest0 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundTest0 =
   liftA
      (\osc smp _fm _vel _freq _dur ->
         osc chunkSize (Sample.body smp))
      (Render.run $ \smp ->
         fmap (\x -> Stereo.consMultiValue x x) $ SigPS.pack smp)

sampledSoundTest1 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundTest1 =
   liftA
      (\osc smp _fm _vel _freq _dur ->
         osc chunkSize (Sample.body smp))
      (Render.run $ \smp ->
         Stereo.multiValue <$>
         Causal.stereoFromMono
                  (CausalPS.pack (Causal.frequencyModulationLinear smp))
               $* liftA2 Stereo.cons
                     (SigPS.constant 0.999)
                     (SigPS.constant 1.001))
--               $* (SigPS.constant $# Stereo.cons 0.999 1.001)))

sampledSoundTest2 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundTest2 =
   liftA
      (\osc smp fm _vel freq dur ->
         let pos = Sample.positions smp
             body =
                SigSt.take (Sample.length pos) $
                SigSt.drop (Sample.start pos) $
                Sample.body smp
         in  SVP.take (chunkSizesFromLazyTime dur) $
             osc chunkSize sampleRate body (fm, freq * Sample.period pos))
      (Render.run $
       wrapped $ \(Signal smp) (Modulation fm) ->
       constant frequency 3 $ \speed _sr ->
         Stereo.multiValue <$>
         ((Causal.stereoFromMono
                  (CausalPS.pack (Causal.frequencyModulationLinear smp))
               <<<
               liftA2 Stereo.cons
                  (CausalPS.amplify 0.999)
                  (CausalPS.amplify 1.001))
                 $* Instr.frequencyFromBendModulation speed fm))

sampledSoundTest3SpaceLeak ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundTest3SpaceLeak =
   liftA
      (\osc smp _fm vel freq dur ->
         {-
         We split the frequency modulation signal
         in order to get a smooth frequency modulation curve.
         Without (periodic) frequency modulation
         we could just split the piecewise constant control curve @fm@.
         -}
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period pos/sampleRatePlain))
             pos = Sample.positions smp
             amp = 2 * amplitudeFromVelocity vel
             (attack, sustain, release) = Sample.parts smp
         in pioApply
              (osc amp
                (attack `SigSt.append`
                 SVL.cycle (SigSt.take (Sample.loopLength pos) sustain)))
              sustainFM
            `SigSt.append`
            pioApply (osc amp release) releaseFM)
      (CausalRender.run $ \amp smp ->
         Stereo.multiValue <$>
         (CausalPS.amplifyStereo amp
              <<<
              Causal.stereoFromMono
                 (CausalPS.pack
                    (Causal.frequencyModulationLinear smp))
              <<<
              liftA2 Stereo.cons
                 (CausalPS.amplify 0.999)
                 (CausalPS.amplify 1.001)))

sampledSoundTest4NoSpaceLeak ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundTest4NoSpaceLeak =
   liftA
      (\freqMod smp fm _vel freq dur ->
         {-
         We split the frequency modulation signal
         in order to get a smooth frequency modulation curve.
         Without (periodic) frequency modulation
         we could just split the piecewise constant control curve @fm@.
         -}
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                pioApplyToLazyTime
                   (freqMod sampleRate (fm, freq*Sample.period pos))
                   (PC.duration fm)
             pos = Sample.positions smp
         in  SigSt.map
                (\x -> Stereo.cons x x)
                (sustainFM `SigSt.append` releaseFM))
      (CausalRender.run $
       wrapped $ \(Modulation fm) ->
       constant frequency 3 $ \speed _sr ->
       Causal.fromSignal $ Instr.frequencyFromBendModulation speed fm)

sampledSoundTest5LargeSpaceLeak ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundTest5LargeSpaceLeak =
   liftA2
      (\osc freqMod smp fm vel freq dur ->
         {-
         We split the frequency modulation signal
         in order to get a smooth frequency modulation curve.
         Without (periodic) frequency modulation
         we could just split the piecewise constant control curve @fm@.
         -}
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                pioApplyToLazyTime
                   (freqMod sampleRate (fm, freq*Sample.period pos))
                   (PC.duration fm)
             pos = Sample.positions smp
             amp = 2 * amplitudeFromVelocity vel
             (attack, sustain, release) = Sample.parts smp
         in pioApply
              (osc amp
                 (attack `SigSt.append`
                  SVL.cycle (SigSt.take (Sample.loopLength pos) sustain)))
              sustainFM
            `SigSt.append`
            pioApply (osc amp release) releaseFM)
      (CausalRender.run $ \ _amp _smp -> arr (\x -> Stereo.consMultiValue x x))
      (CausalRender.run $
       wrapped $ \(Modulation fm) ->
       constant frequency 3 $ \speed _sr ->
       Causal.fromSignal $ Instr.frequencyFromBendModulation speed fm)


sampledSoundSmallSpaceLeak4 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundSmallSpaceLeak4 =
   liftA
      (\osc smp _fm _vel freq dur ->
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period pos/sampleRatePlain))
             pos = Sample.positions smp
         in  pioApply osc sustainFM
             `SigSt.append`
             SigSt.map (\x -> Stereo.cons x x) releaseFM)
      (CausalRender.run $ arr (\x -> Stereo.consMultiValue x x))

sampledSoundSmallSpaceLeak4a ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundSmallSpaceLeak4a =
   liftA
      (\osc smp _fm _vel freq dur ->
         case SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period (Sample.positions smp) / sampleRatePlain)) of
            (sustainFM, releaseFM) ->
               pioApply osc (sustainFM :: SigSt.T Vector)
               `SigSt.append`
               SigSt.map (\x -> Stereo.cons x x) releaseFM)
      (CausalRender.run $ arr (\x -> Stereo.consMultiValue x x))

sampledSoundNoSmallSpaceLeak3 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundNoSmallSpaceLeak3 =
   pure
      (\smp _fm _vel freq dur ->
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period pos/sampleRatePlain))
             pos = Sample.positions smp
         in  SigSt.map (\x -> Stereo.cons x x) sustainFM
             `SigSt.append`
             SigSt.map (\x -> Stereo.cons x x) releaseFM)

{-# NOINLINE amplifySVL #-}
amplifySVL :: SVL.Vector Vector -> SVL.Vector Vector
amplifySVL = SigSt.map (2*)

sampledSoundNoSmallSpaceLeak2 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundNoSmallSpaceLeak2 =
   liftA
      (\osc smp _fm _vel freq dur ->
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period pos/sampleRatePlain))
             pos = Sample.positions smp
         in  pioApply osc
                (amplifySVL sustainFM
                 `SigSt.append`
                 amplifySVL releaseFM))
      (CausalRender.run $ arr (\x -> Stereo.consMultiValue x x))

sampledSoundSmallSpaceLeak1 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundSmallSpaceLeak1 =
   liftA
      (\osc smp _fm _vel freq dur ->
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period pos/sampleRatePlain))
             pos = Sample.positions smp
         in  pioApply osc sustainFM
             `SigSt.append`
             pioApply osc releaseFM)
      (CausalRender.run $ arr (\x -> Stereo.consMultiValue x x))

sampledSoundSmallSpaceLeak0 ::
   IO (Sample.T ->
       PC.T (BM.T Real) ->
       Instrument Real (Stereo.T Vector))
sampledSoundSmallSpaceLeak0 =
   liftA
      (\osc smp _fm vel freq dur ->
         {-
         We split the frequency modulation signal
         in order to get a smooth frequency modulation curve.
         Without (periodic) frequency modulation
         we could just split the piecewise constant control curve @fm@.
         -}
         let sustainFM, releaseFM :: SigSt.T Vector
             (sustainFM, releaseFM) =
                SVP.splitAt (chunkSizesFromLazyTime dur) $
                SigSt.repeat chunkSize
                   (Serial.replicate (freq*Sample.period pos/sampleRatePlain))
             pos = Sample.positions smp
             amp = 2 * amplitudeFromVelocity vel
             (attack, sustain, release) = Sample.parts smp
         in  pioApply
                (osc amp
                   (attack `SigSt.append`
                    SVL.cycle (SigSt.take (Sample.loopLength pos) sustain)))
                sustainFM
             `SigSt.append`
             pioApply (osc amp release) releaseFM)
      (CausalRender.run $ \ _amp _smp -> arr (\x -> Stereo.consMultiValue x x))

makeSample :: Int -> Sample.T
makeSample size =
   Sample.Cons
      (SigSt.replicate chunkSize size 0)
      (DN.frequency 44100)
      (Sample.Positions 0 100000 50000 50000 100)

sequenceSample :: IO ()
sequenceSample = do
   arrange <- SigStL.makeArranger
   sampler <- sampledSoundTest2
   let sound = sampler $ makeSample 100000
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (do fm <- PC.bendWheelPressure Default.channel 2 0.04 0.03
             Gen.sequenceModulated fm Default.channel sound) $
      let evs t = EventList.cons t [] (evs (20-t))
      in  EventList.cons 10 [makeNote Event.NoteOn 60] $
          evs 10

{-
sequenceSample1 :: IO ()
sequenceSample1 = do
   sampler <- Instr.sampledSound
   let sound =
          sampler (SampledSound (SigSt.replicate chunkSize 100000 0)
                      (SamplePositions 0 100000 50000 50000)
                      100)
   SVL.writeFile "test.f32" $
      sound
{-
         (let evs f =
                 EventListBT.cons (BM.Cons 0.001 f) 10 (evs (0.02-f))
          in  evs 0.01)
-}
         (let evs t =
                 EventListBT.cons (BM.Cons 0.01 0.001) t (evs (20-t))
          in  evs 10)
{-
         (PCS.Cons
            (Map.singleton
               (PC.Controller VoiceMsg.modulation) 1)
            (let evs t = EventList.cons t [] (evs (20-t))
             in  EventListMT.consTime 10 $ evs 10))
-}
         0.01 1
--         (NonNegChunky.fromChunks $ repeat $ NonNegW.fromNumber 10)
         (NonNegChunky.fromChunks $ map NonNegW.fromNumber $ iterate (20-) 10)
-}

sequenceSample1 :: IO ()
sequenceSample1 = do
   sampler <- sampledSoundSmallSpaceLeak4a
   let sound = sampler $ makeSample 100000
   SVL.writeFile "test.f32" $
      sound
         (let evs = EventListBT.cons (BM.Cons 0.01 0.001) 1 evs
          in  evs)
         0.01 1
         (NonNegChunky.fromChunks $ repeat $ NonNegW.fromNumber 10)

{-
sequenceSample1a :: IO ()
sequenceSample1a = do
{-
   makeStereoLLVM <-
      CausalP.runStorableChunky2 -- NoSpaceLeak
         (arr (\x -> Stereo.cons x x))
   let stereoLLVM = makeStereoLLVM ()
-}
   stereoLLVM <- CausalP.runStorableChunky3
   let stereoPlain = SigSt.map (\x -> Stereo.cons x x)
   SVL.writeFile "test.f32" $
      let dur = NonNegChunky.fromChunks $ repeat $ SVL.chunkSize 10
          sustainFM, releaseFM :: SigSt.T Vector
          !(sustainFM, releaseFM) =
             SVP.splitAt dur $
             SigSt.repeat chunkSize (Serial.replicate 1)
      in  case 3::Int of
             -- no leak
             0 -> stereoLLVM  $ sustainFM `SigSt.append` releaseFM
             -- no leak
             1 -> stereoPlain $ sustainFM `SigSt.append` releaseFM
             -- no leak
             2 -> stereoPlain sustainFM `SigSt.append` stereoPlain releaseFM
             -- leak
             3 -> stereoLLVM  sustainFM `SigSt.append` stereoPlain releaseFM
             -- no leak
             4 -> stereoPlain sustainFM `SigSt.append` stereoLLVM  releaseFM
             -- leak
             5 -> stereoLLVM  sustainFM `SigSt.append` stereoLLVM  releaseFM
-}

sequenceSample2 :: IO ()
sequenceSample2 = do
   arrange <- SigStL.makeArranger
   sampler <- sampledSoundTest2
   let sound = sampler $ makeSample 100000
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (do bend <- PC.pitchBend Default.channel 2 0.01
             let fm = fmap (\t -> BM.Cons t t) bend
             Gen.sequenceModulated fm Default.channel sound) $
      let evs t = EventList.cons t [] (evs (20-t))
      in  EventList.cons 10 [makeNote Event.NoteOn 60] $
          evs 10

{-
Interestingly, when the program aborts because of heap exhaustion,
then the generated file has size 137MB independent of the heap size
(I tried sizes from 1MB to 64MB).
-}
sequenceSample3 :: IO ()
sequenceSample3 = do
   arrange <- SigStL.makeArranger
   sampler <- sampledSoundTest2
   let sound = sampler $ makeSample 100000
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (let evs =
                 EventListBT.cons (BM.Cons 0.01 0.001) 10 evs
          in  Gen.sequence Default.channel (sound evs)) $
      let evs = EventList.cons 10 [] evs
      in  EventList.cons 10 [makeNote Event.NoteOn 60] evs

sequenceSample4 :: IO ()
sequenceSample4 = do
   arrange <- SigStL.makeArranger
   sampler <- Instr.sampledSound
--   sampler <- sampledSoundTest2
   let sound = sampler $ makeSample 100000
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (let evs = EventListBT.cons (BM.Cons 0.01 0.001) 10 evs
          in  Gen.sequenceCore
                 Default.channel Gen.errorNoProgram
                 (Gen.Modulator () return
                     (return . Gen.renderInstrumentIgnoreProgram (sound evs sampleRate)))) $
      let evs = EventList.cons 10 [] evs
      in  EventList.cons 10 [makeNote Event.NoteOn 60] evs

sequenceFM1 :: IO ()
sequenceFM1 = do
   arrange <- SigStL.makeArranger
   sound <- Instr.softStringFM $/
      let evs = EventListBT.cons (BM.Cons 0.01 0.001) 10 evs
      in  evs
--   sound <- Instr.softStringReleaseEnvelope
   SVL.writeFile "test.f32" $
      arrange vectorChunkSize $
      evalState
         (Gen.sequenceCore
             Default.channel Gen.errorNoProgram
             (Gen.Modulator () return
                 (return . Gen.renderInstrumentIgnoreProgram (sound sampleRate)))) $
      let evs = EventList.cons 10 [] evs
      in  EventList.cons 10 [makeNote Event.NoteOn 60] evs
{-
      sound
         0.01 1
         (NonNegChunky.fromChunks $ map NonNegW.fromNumber $ iterate (20-) 10)
-}


adsr :: IO ()
adsr = do
   env <- Instr.adsr
   SVL.writeFile "adsr.f32" $
      env 0.2 2 0.15 0.3 0.5 vectorChunkSize sampleRate (-0.5) 88200


constCtrl :: a -> PC.T a
constCtrl x =
   let xs = EventListBT.cons x 10000 xs
   in  xs

bellNoiseStereoTest :: IO ()
bellNoiseStereoTest = do
   str <- Instr.bellNoiseStereoFM
   SVL.writeFile "bellnoise.f32" $
      str 0.3 0.1 (constCtrl 0.3) (constCtrl 100)
         vectorChunkSize
         (constCtrl (BM.Cons 1 0.01)) sampleRate 0 440
         100000
