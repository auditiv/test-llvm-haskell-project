module Synthesizer.LLVM.Server.CausalPacked.Run where

import qualified Synthesizer.LLVM.Server.CausalPacked.Arrange as Arrange
import Synthesizer.LLVM.Server.CausalPacked.Arrange
          (StereoVector, controllerExponentialDim, (&+&))

import qualified Sound.MIDI.Controller as Ctrl

import qualified Synthesizer.LLVM.Server.CausalPacked.Speech as Speech
import qualified Synthesizer.LLVM.Server.Option as Option
import Synthesizer.LLVM.Server.ALSA (playChunk, startMessage)
import Synthesizer.LLVM.Server.Common

import qualified Sound.ALSA.Sequencer.Event as Event
import Sound.MIDI.ALSA.Query ()
import Sound.MIDI.ALSA.Construct ()

import qualified Synthesizer.MIDI.CausalIO.ControllerSet as MCS
import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.ALSA.CausalIO.Process as PAlsa
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Storable.Signal as SigStL

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified Data.StorableVector as SV

import qualified Synthesizer.Zip as Zip

import qualified Sound.ALSA.PCM as PCM

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel as ChannelMsg

import Control.Arrow (arr, (<<<), (^<<), (<<^))

import qualified Number.DimensionTerm as DN

import qualified Algebra.Additive as Additive

import Prelude hiding (Real, id)



playFromEvents ::
   (PCM.SampleFmt a, Additive.C a) =>
   Option.T ->
   (SampleRate Real ->
    PIO.T (MIO.Events Event.T) (SV.Vector a)) ->
   IO ()
playFromEvents opt process = do
   putStrLn startMessage
   PAlsa.playFromEventsWithParams (playChunk opt)
      (Option.clientName opt)
      (\(_size,rate) ->
         process (SampleRate $ fromIntegral rate))


keyboard :: IO ()
keyboard = do
   opt <- Option.get
   proc <- Arrange.keyboard

   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStrict
      <<<
      proc (Option.channel opt) sampleRate


keyboardFM :: IO ()
keyboardFM = do
   opt <- Option.get
   proc <-
      Arrange.keyboardFM
         (Causal.map StereoInt.interleave)
         (Option.channel opt)
   playFromEvents opt $ \ sampleRate ->
      SigStL.unpackStereoStrict ^<< proc sampleRate

keyboardDetuneFMCore ::
   Option.T ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real ->
       PIO.T (MIO.Events Event.T) (SV.Vector StereoVector))
keyboardDetuneFMCore opt =
   Arrange.keyboardDetuneFMCore
      (Causal.map StereoInt.interleave)
      (Option.sampleDirectory opt)

keyboardDetuneFM :: IO ()
keyboardDetuneFM = do
   opt <- Option.get
   proc <- keyboardDetuneFMCore opt
   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      proc (Option.channel opt) (VoiceMsg.toProgram 0) sampleRate

keyboardMultiChannel :: IO ()
keyboardMultiChannel = do
   opt <- Option.get
   proc <- Arrange.keyboardMultiChannel (Option.sampleDirectory opt)
   playFromEvents opt proc


voderBand :: IO ()
voderBand = do
   opt <- Option.get
   proc <-
      Arrange.voderBand
         (Causal.map StereoInt.interleave)
         (Option.sampleDirectory opt)

   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      proc (Option.channel opt) (VoiceMsg.toProgram 4) sampleRate

voderMask :: IO ()
voderMask = do
   opt <- Option.get
   proc <-
      Arrange.voderMask
         (Causal.map StereoInt.interleave)
         (Option.sampleDirectory opt)

   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      proc (Option.channel opt) (VoiceMsg.toProgram 4) sampleRate

voderMaskEnv :: IO ()
voderMaskEnv = do
   opt <- Option.get
   proc <-
      Arrange.voderMaskEnv
         (Causal.map StereoInt.interleave)
         (Option.sampleDirectory opt)

   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      proc (Option.channel opt) (VoiceMsg.toProgram 4) sampleRate

voderMaskSeparated :: IO ()
voderMaskSeparated = do
   opt <- Option.get
   proc <-
      Arrange.voderMaskSeparated
         (const $ Causal.map StereoInt.interleave)
         (Option.sampleDirectory opt)

   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      proc
         (Option.channel opt) (Option.extraChannel opt)
         (VoiceMsg.toProgram 4) sampleRate ()

voderMaskMulti :: IO ()
voderMaskMulti = do
   opt <- Option.get
   proc <- Arrange.voderMaskMulti $ Option.sampleDirectory opt
   playFromEvents opt proc


formant :: IO ()
formant = do
   opt <- Option.get
   proc <-
      Arrange.keyboardDetuneFMCore (arr Stereo.multiValue)
         (Option.sampleDirectory opt)
   form <- Speech.filterFormant
   mix <- CausalRender.run Causal.mix
   interleave <-
      CausalRender.run
         (Causal.map StereoInt.interleave <<^ Stereo.unMultiValue)

   playFromEvents opt $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      interleave
      <<<
      foldl1
         (\x y -> mix <<< Zip.arrowFanout x y)
         (zipWith
             (\n (freq, amp, reson) ->
                form sampleRate
                <<<
                Zip.arrowFirst
                   (MCS.controllerExponential (Ctrl.fromInt $ 16+n) (0.01,1) amp
                    &+&
                    (MCS.controllerExponential (Ctrl.fromInt $ 26+n) (1,100) reson
                     &+&
                     controllerExponentialDim (Ctrl.fromInt $ 21+n)
                        (DN.frequency 100, DN.frequency 10000)
                        (DN.frequency freq))))
             [0..]
             [( 650, 1.00, 30),
              (1080, 0.25, 30),
              (2650, 0.20, 30),
              (2900, 0.16, 30),
              (3250, 0.01, 30)
              ])
      <<<
      MCS.fromChannel (Option.channel opt)
      &+&
      proc (Option.channel opt) (VoiceMsg.toProgram 4) sampleRate
