module Synthesizer.LLVM.Server.Scalar.Test where

import qualified Synthesizer.LLVM.Server.Scalar.Instrument as Instr
import qualified Synthesizer.LLVM.Server.Option as Option
import qualified Synthesizer.LLVM.Server.Default as Default
import Synthesizer.LLVM.Server.Scalar.Run (withMIDIEvents)
import Synthesizer.LLVM.Server.ALSA (record, put, makeNote)
import Synthesizer.LLVM.Server.Common

import qualified Synthesizer.ALSA.Storable.Play as Play
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Synthesizer.MIDI.PiecewiseConstant as PC
import qualified Synthesizer.MIDI.Generic as Gen

import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Wave as WaveL
import Synthesizer.LLVM.Causal.Process (($<), ($*))

import qualified Synthesizer.Storable.Cut         as CutSt
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified Data.EventList.Relative.TimeBody  as EventList

import Control.Monad.Trans.State (evalState)

import NumericPrelude.Numeric (zero)
import Prelude hiding (Real)


chunkSize :: SVL.ChunkSize
chunkSize = Play.defaultChunkSize

sampleRate :: SampleRate Real
sampleRate = Default.sampleRate


pitchBend0 :: IO ()
pitchBend0 = do
   osc <-
      Render.run $ \fm ->
         Causal.osci WaveL.triangle $< zero $* piecewiseConstant fm
   SVL.writeFile "test.f32" $
      (id :: SigSt.T Real -> SigSt.T Real) .
      osc chunkSize .
      evalState (PC.pitchBend Default.channel 2 (frequency sampleRate 880)) $
      let evs = EventList.cons 100 [] evs
      in  EventList.cons 0 ([]::[Event.T]) evs

pitchBend1 :: IO ()
pitchBend1 = do
   opt <- Option.get
   osc <-
      Render.run $ \fm ->
         Causal.osci WaveL.triangle $< zero $* piecewiseConstant fm
   withMIDIEvents opt (record "test.f32") $ \ _size _rate ->
      (id :: SigSt.T Real -> SigSt.T Real) .
      osc chunkSize .
      evalState (PC.pitchBend Default.channel 2 (frequency sampleRate 880))

pitchBend2 :: IO ()
pitchBend2 = do
   opt <- Option.get
   withMIDIEvents opt put $ \ _size _rate -> id



sequencePress :: IO ()
sequencePress = do
--   arrange <- SigStL.makeArranger
--   sound <- Instr.softString
--   sound <- Instr.softStringReleaseEnvelope
--   sound <- Instr.pingReleaseEnvelope $/ 1 $/ chunkSize
--   sound <- Instr.pingDur
--   sound <- Instr.pingDurTake
   let sound = Instr.dummy chunkSize Default.sampleRate
   SVL.writeFile "test.f32" $
      CutSt.arrange chunkSize $
      evalState
         (do Gen.sequence Default.channel sound) $
      let evs t =
             EventList.cons t [makeNote Event.NoteOn  60] $
             EventList.cons t [makeNote Event.NoteOff 60] $
             evs (20-t)
      in  evs 10

