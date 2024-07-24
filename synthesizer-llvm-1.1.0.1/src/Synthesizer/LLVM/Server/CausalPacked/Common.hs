module Synthesizer.LLVM.Server.CausalPacked.Common where

import Synthesizer.LLVM.Server.Common (SampleRate(SampleRate), Real)

import qualified Synthesizer.LLVM.MIDI.BendModulation as BM

import qualified Data.EventList.Relative.TimeTime as EventListTT

import qualified Numeric.NonNegative.Class as NonNeg

import Prelude hiding (Real)


-- ToDo: might be moved to event-list package
chopEvents ::
   (NonNeg.C time, Num time) =>
   time ->
   EventListTT.T time body ->
   [EventListTT.T time body]
chopEvents chunkSize =
   let go evs =
          -- splitBeforeTime?
          let (chunk,rest) = EventListTT.splitAtTime chunkSize evs
          in  if EventListTT.duration chunk == 0
                then []
                else chunk : go rest
   in  go


transposeModulation ::
   (Functor stream) =>
   SampleRate Real ->
   Real ->
   stream (BM.T Real) ->
   stream (BM.T Real)
transposeModulation (SampleRate sampleRate) freq =
   fmap (BM.shift (freq/sampleRate))
