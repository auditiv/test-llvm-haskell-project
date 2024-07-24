{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
-- module Synthesizer.LLVM.Server.JACK where

import qualified Synthesizer.LLVM.Server.CausalPacked.Arrange as Arrange

import Synthesizer.LLVM.Server.CommonPacked (Vector, VectorSize, vectorSize)

import qualified Synthesizer.LLVM.Server.Option as Option
import Synthesizer.LLVM.Server.Common

import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Storable.Signal as SigStL

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import Foreign.Marshal.Array (copyArray)

import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Absolute.TimeTime  as EventListAbsTT
import qualified Data.EventList.Absolute.TimeMixed as EventListAbsTM

import qualified Synthesizer.Zip as Zip

import qualified Sound.JACK.Audio as JackAudio
import qualified Sound.JACK.MIDI as JackMIDI
import qualified Sound.JACK.Exception as JackExc
import qualified Sound.JACK as JACK

import Data.IORef (newIORef, readIORef, writeIORef)

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message as Msg

import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Monad.Trans.Class as MT

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path

import Control.Arrow (arr, (<<<), (^<<))
import Control.Category (id)

import qualified System.Random as Random
import qualified Numeric.NonNegative.Wrapper as NonNegW

import Prelude hiding (Real, id)


type StereoVector = StereoInt.T VectorSize Real

type StrictTime = NonNegW.Integer


strictTimeFromNFrames :: JACK.NFrames -> StrictTime
strictTimeFromNFrames (JACK.NFrames n) =
   NonNegW.fromNumberMsg "strictTimeFromNFrames" $ fromIntegral n

writeBlock :: JackAudio.Port JACK.Output -> SV.Vector Real -> IO ()
writeBlock output block = do
   outArr <-
      JackAudio.getBufferPtr output $
         JACK.NFrames $ fromIntegral $ SV.length block
   SVB.withStartPtr (SV.map realToFrac block) $
      copyArray outArr

{-# INLINE playFromEvents #-}
playFromEvents ::
   (JACK.Client ->
    (ports -> Exc.ExceptionalT JackExc.All IO ()) ->
    Exc.ExceptionalT JackExc.All IO ()) ->
   (ports -> block -> IO ()) ->
   Option.T ->
   (SampleRate Real ->
    PIO.T Events block) ->
   IO ()
playFromEvents withOutPorts writeBlocks opt process = do
   let Option.ClientName name = Option.clientName opt
   JACK.handleExceptions $
      JACK.withClientDefault name $ \client ->
      JACK.withPort client "input" $ \input ->
      withOutPorts client $ \output -> do
         sampleRate <- MT.lift $ JACK.getSampleRate client
         case process (SampleRate $ fromIntegral sampleRate) of
            PIO.Cons next create delete ->
               {-
               Is the use of 'bracket' correct?
               I think 'delete' must be called with the final state,
               not with the initial one.
               -}
               Exc.bracketT (MT.lift create) (MT.lift . delete) $ \start -> do
                  stateRef <- MT.lift $ newIORef start
                  let jackProcess nframes = do
                         evs <- JackMIDI.readEventsFromPort input nframes
                         MT.lift $ do
                            let deconsNFrames (JACK.NFrames n) = fromIntegral n
                            (block, state) <-
                               next
                                  (EventListTT.collectCoincident $
                                   EventListTT.mapTime
                                      (NonNegW.fromNumberMsg "JACK.playFromEvents") $
                                   EventListTT.fromAbsoluteEventList $
                                   EventListAbsTT.mapTime
                                      (flip div (fromIntegral vectorSize) .
                                       deconsNFrames) $
                                   EventListAbsTM.snocTime evs nframes)
{-
                                  (EventListTT.collectCoincident $
                                   EventListTT.mapTime strictTimeFromNFrames $
                                   EventListTT.fromAbsoluteEventList $
                                   EventListAbsTM.snocTime evs nframes)
-}
                                  =<< readIORef stateRef
                            writeIORef stateRef state
                            writeBlocks output block
                  JACK.withProcess client jackProcess $
                     JACK.withActivation client $ MT.lift $ do
                        putStrLn $ "started " ++ name ++ "..."
                        JACK.waitForBreak
--                  MT.lift $ readIORef stateRef

playMonoFromEvents ::
   Option.T ->
   (SampleRate Real ->
    PIO.T Events (SV.Vector Vector)) ->
   IO ()
playMonoFromEvents opt proc =
   playFromEvents
      (flip JACK.withPort "mono")
      writeBlock
      opt
      (\sampleRate -> SigStL.unpackStrict ^<< proc sampleRate)

playStereoFromEvents ::
   Option.T ->
   (SampleRate Real ->
    PIO.T Events (Zip.T (SV.Vector Vector) (SV.Vector Vector))) ->
   IO ()
playStereoFromEvents opt proc =
   playFromEvents
      (\client f ->
         JACK.withPort client "left" $ \left ->
         JACK.withPort client "right" $ \right ->
         f (left, right))
      (\(leftPort,rightPort) (Zip.Cons leftBlock rightBlock) ->
         writeBlock leftPort leftBlock >>
         writeBlock rightPort rightBlock)
      opt
      (\sampleRate ->
         Zip.arrowSplit SigStL.unpackStrict SigStL.unpackStrict
         ^<<
         proc sampleRate)


keyboard :: IO ()
keyboard = do
   opt <- Option.get
   proc <- Arrange.keyboard

   playMonoFromEvents opt $ proc (Option.channel opt)


type Events = MIO.Events Msg.T

unconsStereo :: Stereo.T t -> (t, t)
unconsStereo x =
   (Stereo.left x, Stereo.right x)

keyboardFM :: IO ()
keyboardFM = do
   opt <- Option.get
   playStereoFromEvents opt =<<
      Arrange.keyboardFM (arr unconsStereo) (Option.channel opt)


keyboardDetuneFMCore ::
   (PathClass.AbsRel ar) =>
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real ->
       PIO.T
          Events
          (Zip.T (SV.Vector Vector) (SV.Vector Vector)))
keyboardDetuneFMCore =
   Arrange.keyboardDetuneFMCore (arr unconsStereo)

keyboardDetuneFM :: IO ()
keyboardDetuneFM = do
   opt <- Option.get
   proc <- keyboardDetuneFMCore (Option.sampleDirectory opt)
   playStereoFromEvents opt $
      proc (Option.channel opt) (VoiceMsg.toProgram 0)

keyboardMultiChannel :: IO ()
keyboardMultiChannel = do
   opt <- Option.get
   proc <- keyboardDetuneFMCore (Option.sampleDirectory opt)
   mix <- CausalRender.run Causal.mix

   playStereoFromEvents opt $ \ sampleRate ->
      foldl1
         (\x y -> mix <<< Zip.arrowFanout x y)
         (map
             (\chan ->
                proc (ChannelMsg.toChannel chan) (VoiceMsg.toProgram 0)
                     sampleRate)
             [0 .. 3])


voderBand :: IO ()
voderBand = do
   opt <- Option.get
   proc <-
      Arrange.voderBand
         (arr unconsStereo)
         (Option.sampleDirectory opt)

   playStereoFromEvents opt $ \ sampleRate ->
      proc (Option.channel opt) (VoiceMsg.toProgram 5) sampleRate


voderMaskSeparated :: IO ()
voderMaskSeparated = do
   opt <- Option.get
   let postProcessing params =
          if True
            then
               CausalPS.pack
                  (Stereo.arrowFromChannels
                     (Causal.reverbExplicit $ Stereo.left params)
                     (Causal.reverbExplicit $ Stereo.right params))
            else id
   proc <-
      Arrange.voderMaskSeparated
         (\reverbParams -> unconsStereo ^<< postProcessing reverbParams)
         (Option.sampleDirectory opt)

   playStereoFromEvents opt $ \ sampleRate@(SampleRate rate) ->
      proc
         (Option.channel opt) (Option.extraChannel opt)
         (VoiceMsg.toProgram 4) sampleRate
         (fmap
            (\seed ->
               Causal.reverbParams
                  (Random.mkStdGen seed) TypeNum.d16 (0.92,0.98)
                  (round (rate/200), round (rate/40)))
            (Stereo.cons 42 23))


main :: IO ()
main = keyboardMultiChannel
