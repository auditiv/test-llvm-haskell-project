module Synthesizer.LLVM.Server.ALSA (
   Output,
   play, playChunk,
   record,
   put,
   startMessage,
   makeNote,
   ) where

import qualified Synthesizer.LLVM.Server.Option as Option

import qualified Synthesizer.ALSA.CausalIO.Process as PIO

import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer.RealTime as RealTime

import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam

import qualified Synthesizer.Storable.Signal as SigSt

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB

import qualified Algebra.Additive  as Additive

import Control.Functor.HT (void)

import qualified System.IO as IO

import Prelude hiding (Real, round)


getOptParams :: Option.T -> h -> ((PCM.Size, PCM.SampleFreq), h)
getOptParams opt h =
   ((case Option.chunkSize opt of SVL.ChunkSize size -> size,
     case Option.sampleRate opt of
        Nothing -> 44100
        Just (Option.SampleRate rate) -> rate),
    h)


type Output handle signal a = Option.T -> PIO.Output handle signal a

record ::
   (PCM.SampleFmt y) =>
   FilePath -> Output IO.Handle (SigSt.T y) ()
record name opt =
   (fmap (getOptParams opt) $ IO.openFile name IO.WriteMode,
    IO.hClose,
    SVL.hPut)

put :: (Show signal) => Output () signal ()
put opt =
   (return $ getOptParams opt (),
    return,
    \() -> print)


playChunk ::
   (Additive.C y, PCM.SampleFmt y) =>
   Output (PCM.Handle HwParam.Interleaved y) (SVB.Vector y) ()
playChunk opt =
   (openPCM opt, closePCM, write)


-- ToDo: do not record the empty chunk that is inserted for latency
{-# INLINE play #-}
play ::
   (Additive.C y, PCM.SampleFmt y) =>
   Output (PCM.Handle HwParam.Interleaved y) (SigSt.T y) ()
play opt =
   (openPCM opt, closePCM, \h -> mapM_ (write h) . SVL.chunks)
{-
   Play.auto (Play.makeSink
      (Option.device opts) (Option.periodTime opts) (round rate)) .
   SigSt.append (SigSt.replicate (Option.chunkSize opts) (Option.latency opts) zero)
--   FiltG.delayPosLazySize (Option.chunkSize opts) (Option.latency opts)
--   FiltG.delayPos (Option.latency opts)
-}


putLog :: String -> IO ()
putLog = putStrLn

openPCM ::
   (PCM.Access i, PCM.SampleFmt y) =>
   Option.T ->
   IO ((PCM.Size, PCM.SampleFreq), PCM.Handle i y)
openPCM opt = do
   putLog "alsaOpen"
   (((bufferSize,periodSize),(bufferTime,periodTime),sampleRate), h) <-
      PCM.open (PCM.modes []) PCM.StreamPlayback
         (setHwParams (Option.sampleRate opt) (Option.chunkSize opt))
         (\q@(sizes,_,_) -> do
             uncurry SwParam.setBufferSize sizes
             return q)
         (Option.device opt)
   PCM.prepare h
   putLog $ "bufferTime = " ++ show bufferTime
   putLog $ "bufferSize = " ++ show bufferSize
   putLog $ "periodTime = " ++ show periodTime
   putLog $ "periodSize = " ++ show periodSize
   return ((periodSize, sampleRate), h)

closePCM :: PCM.Handle i y -> IO ()
closePCM pcm = do
   putLog "alsaClose"
   PCM.drain pcm
   PCM.close pcm

setHwParams ::
   Maybe (Option.SampleRate Int) ->
   SVL.ChunkSize ->
   HwParam.T i y ((PCM.Size,PCM.Size),(PCM.Time,PCM.Time),PCM.SampleFreq)
   -- ^ ((bufferSize,periodSize),(bufferTime,periodTime),sampleRate)
setHwParams mrate (SVL.ChunkSize periodSize) = do
   (actualRate,_) <-
      case mrate of
         Nothing -> do
            HwParam.setRateResample False
            HwParam.setRateNear 44100 EQ
         Just (Option.SampleRate rate) -> do
            HwParam.setRateResample True
            HwParam.setRateNear rate EQ
   (actualPeriodSize,_) <-
      HwParam.setPeriodSizeNear periodSize EQ
   actualBufferSize <-
      HwParam.setBufferSizeNear
         (max periodSize (actualPeriodSize*4))

   (actualBufferTime,_) <- HwParam.getBufferTime
   (actualPeriodTime,_) <- HwParam.getPeriodTime
   return ((actualBufferSize, actualPeriodSize),
           (actualBufferTime, actualPeriodTime),
           actualRate)

write ::
   (PCM.SampleFmt y) =>
   PCM.Handle PCM.Interleaved y -> SVB.Vector y -> IO ()
write h xs =
   SVB.withStartPtr xs $ \buf ->
      void . PCM.writeiRetry h buf . fromIntegral



startMessage :: String
startMessage =
   "run 'aconnect' to connect to the MIDI controller"


-- cf. synthesizer-alsa:Synthesizer.ALSA.Storable.Server.Test
makeNote :: Event.NoteEv -> Int -> Event.T
makeNote typ pit =
   (Event.simple Addr.subscribers $ Event.NoteEv typ $
    Event.simpleNote (Event.Channel 0)
        (Event.Pitch $ fromIntegral pit) Event.normalVelocity)
      { Event.time =
           Time.consAbs $ Time.Real $ RealTime.fromInteger 0
      }
