module Main (main) where
-- module Synthesizer.LLVM.Server.Render where

import qualified Synthesizer.LLVM.Server.CausalPacked.Arrange as Arrange
import Synthesizer.LLVM.Server.CommonPacked
          (vectorSize, vectorRate)

import qualified Synthesizer.LLVM.Server.Option as Option
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import Synthesizer.LLVM.Server.CausalPacked.Common (chopEvents)
import Synthesizer.LLVM.Server.Common

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.PiecewiseConstant.Signal as PC

import Shell.Utility.Exit (exitFailureMsg)

import qualified Data.StorableVector.Lazy as SVL

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.File.Load as Load

import qualified Sound.Sox.Write as SoxWrite
import qualified Sound.Sox.Play  as SoxPlay

import Control.Applicative ((<*>))

import Data.Monoid (mempty)

import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified System.Exit as Exit

import Prelude hiding (Real, id)



strictTimeFromChunkSize :: SVL.ChunkSize -> PC.StrictTime
strictTimeFromChunkSize (SVL.ChunkSize n) =
   NonNegW.fromNumberMsg "strictTimeFromNFrames" $ fromIntegral n


{-
This is the duration of rendering after the last MIDI event.

Optimally we would stop rendering after the last sound ends.
Unfortunately with causal processes we have no way
to make the output audio stream longer than the input MIDI stream.
We might make the stream infinitely long
and add an End-Of-Stream marker in the MIDI input
that tells the 'arrange' process to stop after the last sound.
-}
padTime :: Integer
padTime = 2

render :: Option.T -> IO (MidiFile.T -> SVL.Vector (Stereo.T Real))
render opt = do
   proc <-
      case fromInteger 0 :: Int of
         0 -> Arrange.keyboardMultiChannel $ Option.sampleDirectory opt
         _ -> Arrange.voderMaskMulti $ Option.sampleDirectory opt
   run <- PIO.runCont $ proc $ fmap fromIntegral $ Option.sampleRate opt
   return $
      SVL.fromChunks .
      run (const []) .
      chopEvents (strictTimeFromChunkSize $ Option.chunkSize opt) .
      flip EventListTM.snocTime
         (NonNegW.fromNumberMsg "render end pad" $
          case Option.sampleRate opt of
             SampleRate rate -> padTime * (fromIntegral $ div rate vectorSize)) .
--      flip EventListTM.snocTime (NonNegW.fromNumber 1) .
--      flip EventListTM.snocTime mempty .
      EventList.collectCoincident .
      EventList.mapMaybe (\ev ->
         case ev of
            FileEvent.MIDIEvent mev -> Just mev
            _ -> Nothing) .
      EventList.resample
         (vectorRate $ fmap fromIntegral $ Option.sampleRate opt) .
      (\(MidiFile.Cons typ division tracks) ->
         MidiFile.mergeTracks typ $
         map (MidiFile.secondsFromTicks division) tracks)

handleSoxExit :: IO Exit.ExitCode -> IO ()
handleSoxExit sox = do
   soxResult <- sox
   case soxResult of
      Exit.ExitSuccess -> return ()
      Exit.ExitFailure n ->
         exitFailureMsg $ "'sox' aborted with exit code " ++ show n

main :: IO ()
main = do
   (opt, midiPath, mWavePath) <- Option.get
   case Option.sampleRate opt of
      SampleRate rate -> do
         audio <- render opt <*> Load.fromFile midiPath
         case mWavePath of
            Nothing ->
               handleSoxExit $ SoxPlay.simple SVL.hPut mempty rate audio
            Just wavePath ->
               if True
                  then
                     -- Rendering to SoX ends with an error code 13, but why?
                     handleSoxExit $
                        SoxWrite.simple SVL.hPut mempty wavePath rate audio
                  else SVL.writeFile wavePath audio
