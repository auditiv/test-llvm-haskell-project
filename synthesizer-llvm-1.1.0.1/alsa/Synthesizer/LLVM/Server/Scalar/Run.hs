module Synthesizer.LLVM.Server.Scalar.Run where

import qualified Synthesizer.LLVM.Server.Scalar.Instrument as Instr
import qualified Synthesizer.LLVM.Server.Option as Option
import Synthesizer.LLVM.Server.ALSA (Output, play, startMessage)
import Synthesizer.LLVM.Server.CausalPacked.Common (transposeModulation)
import Synthesizer.LLVM.Server.Common hiding (transposeModulation)

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Data.EventList.Relative.TimeBody  as EventList

import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.MIDI as MIDIL
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.Wave as WaveL
import Synthesizer.LLVM.Causal.Process (($<), ($*))

import qualified Synthesizer.Storable.Signal as SigSt

import qualified Synthesizer.ALSA.EventList as Ev

import qualified Synthesizer.MIDI.PiecewiseConstant as PC
import qualified Synthesizer.MIDI.Generic as Gen

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Control.Arrow ((^<<), (<<^))
import Control.Monad.Trans.State (evalState)
import Control.Applicative ((<$>))

import Control.Exception (bracket)

import NumericPrelude.Numeric (fromIntegral, zero, (*), (*>), (/))
import NumericPrelude.Base
import Prelude (Double)


{-# INLINE withMIDIEvents #-}
withMIDIEvents ::
   Option.T ->
   Output handle signal a ->
   (SigSt.ChunkSize -> SampleRate Real ->
    EventList.T Ev.StrictTime [Event.T] -> signal) -> IO a
withMIDIEvents opt output process = do
   putStrLn startMessage
   case output opt of
      (open,close,write) ->
         bracket open (close . snd) $ \((chunkSize,rate),h) ->
            let rrate = fromIntegral rate :: Double
            in  Ev.withMIDIEvents
                   (Option.clientName opt)
                   (fromIntegral chunkSize / rrate)
                   rrate
                   (write h .
                    process (SigSt.chunkSize chunkSize)
                       (Option.SampleRate $ fromIntegral rate))



pitchBend :: IO ()
pitchBend = do
   opt <- Option.get
   osc <-
      Render.run $ \fm ->
         Causal.osci WaveL.triangle $< zero $* piecewiseConstant fm
   withMIDIEvents opt play $ \chunkSize sampleRate ->
      (id :: SigSt.T Real -> SigSt.T Real) .
      osc chunkSize .
      evalState (PC.pitchBend (Option.channel opt) 2 (frequency sampleRate 880))


frequencyModulation :: IO ()
frequencyModulation = do
   opt <- Option.get
   osc <-
      Render.run $
      constant frequency 10 $ \speed _sr fm ->
         Causal.osci WaveL.triangle
            $< zero
            $* (MIDIL.frequencyFromBendModulation speed
                  $* piecewiseConstant (fmap BM.unMultiValue <$> fm))
   withMIDIEvents opt play $ \chunkSize sampleRate ->
      (id :: SigSt.T Real -> SigSt.T Real) .
      osc chunkSize sampleRate . transposeModulation sampleRate 880 .
      evalState (PC.bendWheelPressure (Option.channel opt) 2 0.04 (0.03::Real))



keyboard :: IO ()
keyboard = do
   opt <- Option.get
--   sound <- Instr.pingDur
{-
   sound <-
      fmap (\s vel _freq dur -> s vel dur) $
      (Instr.pingReleaseEnvelope $/ 0.4 $/ 0.1)
-}
   sound <- Instr.pingRelease $/ 0.4 $/ 0.1
   amp <- CausalRender.run Causal.amplify
   arrange <- SigStL.makeArranger
   withMIDIEvents opt play $ \chunkSize sampleRate ->
      pioApply (amp (0.2 :: Real)) .
      arrange chunkSize .
      evalState
         (Gen.sequence
            (Option.channel opt)
            (sound chunkSize sampleRate))

keyboardStereo :: IO ()
keyboardStereo = do
   opt <- Option.get
   sound <- Instr.pingStereoRelease $/ 0.4 $/ 0.1
   amp <-
      CausalRender.run $ \vol ->
         Stereo.multiValue ^<< Causal.amplifyStereo vol <<^ Stereo.unMultiValue
   arrange <- SigStL.makeArranger
   withMIDIEvents opt play $ \chunkSize sampleRate ->
      pioApply (amp (0.2 :: Real)) .
      arrange chunkSize .
      evalState
         (Gen.sequence
            (Option.channel opt)
            (sound chunkSize sampleRate))

keyboardMulti :: IO ()
keyboardMulti = do
   opt <- Option.get
   png <- Instr.pingDur
   pngRel <- Instr.pingRelease $/ 0.4 $/ 0.1 $/ Option.chunkSize opt
   tin <- Instr.tine $/ 0.4 $/ 0.1 $/ Option.chunkSize opt
   arrange <- SigStL.makeArranger
   withMIDIEvents opt play $ \chunkSize sampleRate ->
--      playALSA (Bld.put :: Int16 -> Bld.Builder Int16) (sampleRate opt::Real) .
      SigSt.map (0.2*) .
      arrange chunkSize .
      evalState (Gen.sequenceMultiProgram (Option.channel opt)
         (VoiceMsg.toProgram 2)
         (map ($ sampleRate) [png, pngRel, tin]))

keyboardStereoMulti :: IO ()
keyboardStereoMulti = do
   opt <- Option.get
   png <- Instr.pingStereoRelease $/ 0.4 $/ 0.1
   tin <- Instr.tineStereo $/ 0.4 $/ 0.1
   str <- Instr.softString
   arrange <- SigStL.makeArranger
   withMIDIEvents opt play $ \chunkSize sampleRate ->
--      playALSA (Bld.put :: Int16 -> Bld.Builder Int16) (sampleRate opt::Real) .
      SigSt.map ((0.2::Real)*>) .
      arrange chunkSize .
      evalState (Gen.sequenceMultiProgram (Option.channel opt)
         (VoiceMsg.toProgram 1)
         (map (\sound -> sound chunkSize sampleRate) [png, tin, const str]))
