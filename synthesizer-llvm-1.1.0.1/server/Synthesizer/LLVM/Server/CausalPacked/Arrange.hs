module Synthesizer.LLVM.Server.CausalPacked.Arrange where

import Synthesizer.LLVM.Server.CommonPacked
         (VectorSize, Vector, VectorValue, stair)

import qualified Sound.MIDI.Controller as Ctrl

import qualified Synthesizer.LLVM.Server.CausalPacked.Speech as Speech
import qualified Synthesizer.LLVM.Server.CausalPacked.Instrument as Instr
import qualified Synthesizer.LLVM.Server.CausalPacked.InstrumentPlug as InstrPlug
import qualified Synthesizer.LLVM.Server.SampledSound as Sample
import Synthesizer.LLVM.Server.Common

import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.MIDI.CausalIO.ControllerSet as MCS
import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.MIDI.Value as MV
import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.PiecewiseConstant.Signal as PC

import qualified Synthesizer.LLVM.Plug.Output as POut
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Storable.Process as CausalSt
import qualified Synthesizer.LLVM.Storable.Signal as SigStL

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial

import qualified Data.EventList.Relative.TimeTime  as EventListTT

import qualified Data.StorableVector as SV

import qualified Synthesizer.Zip as Zip

import qualified Synthesizer.MIDI.Dimensional.ValuePlain as DMV
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Class.Construct as Construct
import qualified Sound.MIDI.Message.Class.Check as Check

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path

import Control.Arrow (Arrow, arr, (***), (<<<), (^<<), (<<^))
import Control.Category (id)
import Control.Applicative ((<*>))

import qualified Data.List.HT as ListHT
import qualified Data.Map as Map
import Data.Maybe.HT (toMaybe)

import qualified Number.DimensionTerm as DN
import qualified Algebra.DimensionTerm as Dim

import qualified Algebra.Transcendental as Trans

import qualified Numeric.NonNegative.Wrapper as NonNegW

import Prelude hiding (Real, id)



type StereoVector = StereoInt.T VectorSize Real


keyboard ::
   (Check.C msg) =>
   IO (ChannelMsg.Channel ->
       SampleRate Real ->
       PIO.T (MIO.Events msg) (SV.Vector Vector))
keyboard = do
   arrange <- CausalSt.makeArranger
   amp <- CausalRender.run (CausalPS.amplify 0.2)

   ping <- Instr.pingRelease

   return $ \ chan sampleRate ->
      amp
      <<<
      arrange
      <<<
      arr shortTime
      <<<
      MIO.sequenceCore chan
         (\ _pgm -> ping 0.8 0.1 sampleRate)


infixr 3 &+&

(&+&) ::
   (Arrow arrow) =>
   arrow a b -> arrow a c -> arrow a (Zip.T b c)
(&+&) = Zip.arrowFanout


controllerExponentialDirect ::
   (Check.C msg, Trans.C y, Dim.C v) =>
   ChannelMsg.Channel ->
   VoiceMsg.Controller ->
   (DN.T v y, DN.T v y) ->
   DN.T v y ->
   PIO.T (MIO.Events msg) (Instr.Control (DN.T v y))
controllerExponentialDirect chan ctrl bnds initial =
   MIO.slice
      (Check.controller chan ctrl)
      (DMV.controllerExponential bnds)
      initial

shortTime ::
   EventListTT.T PC.StrictTime body ->
   EventListTT.T PC.ShortStrictTime body
shortTime =
   EventListTT.mapTime
      (NonNegW.fromNumberUnsafe . fromInteger . NonNegW.toNumber)

keyboardFM ::
   (Check.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   ChannelMsg.Channel ->
   IO (SampleRate Real -> PIO.T (MIO.Events msg) b)
keyboardFM emitStereo chan = do
   arrange <- CausalSt.makeArranger
   amp <-
      CausalRender.run
         (emitStereo <<< CausalPS.amplifyStereo 0.2 <<^ Stereo.unMultiValue)

   ping <- Instr.pingStereoReleaseFM

   return $ \ sampleRate ->
      amp
      <<<
      arrange
      <<<
      arr shortTime
      <<<
      -- ToDo: fetch parameters from controllers
      MIO.sequenceModulated chan
         (\ _pgm -> ping sampleRate)
      <<<
      id &+&
         ((controllerExponentialDirect chan
             Ctrl.attackTime (DN.time 0.25, DN.time 2.5) (DN.time 0.8)
           &+&
           controllerExponentialDirect chan
             Ctrl.releaseTime (DN.time 0.03, DN.time 0.3) (DN.time 0.1))
          &+&
          ((MIO.controllerExponential chan controllerTimbre0 (1/pi,0.01) 0.05
            &+&
            controllerExponentialDirect chan controllerTimbre1
               (DN.time 0.01, DN.time 10)
               (DN.time 5))
           &+&
           ((MIO.controllerLinear chan Ctrl.soundController5 (0,2) 1
             &+&
             controllerExponentialDirect chan Ctrl.soundController7
                (DN.time 0.25, DN.time 2.5)
                (DN.time 0.8))
            &+&
            (MIO.controllerLinear chan controllerDetune (0,0.005) 0.001
             &+&
             MIO.bendWheelPressure chan 2 0.04 0.03))))


controllerExponentialDim ::
   (Arrow arrow,
    Trans.C y, Dim.C v) =>
   VoiceMsg.Controller ->
   (DN.T v y, DN.T v y) ->
   DN.T v y ->
   MCS.T arrow (DN.T v y)
controllerExponentialDim ctrl bnds initial =
   MCS.slice
      (MCS.Controller ctrl)
      (DMV.controllerExponential bnds)
      initial


timeControlPercussive, timeControlString ::
   PIO.T
      (PCS.T MCS.Controller Int)
      (Zip.T
         (Instr.Control Instr.Time)
         (Instr.Control Instr.Time))

timeControlPercussive =
   controllerExponentialDim Ctrl.attackTime
      (DN.time 0.1, DN.time 2.5) (DN.time 0.8)
   &+&
   controllerExponentialDim Ctrl.releaseTime
      (DN.time 0.03, DN.time 0.3) (DN.time 0.1)

timeControlString =
   controllerExponentialDim Ctrl.attackTime
      (DN.time 0.005, DN.time 0.1) (DN.time 0.1)
   &+&
   controllerExponentialDim Ctrl.releaseTime
      (DN.time 0.03, DN.time 0.3) (DN.time 0.2)


keyboardDetuneFMCore ::
   (PathClass.AbsRel ar, Check.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> PIO.T (MIO.Events msg) b)
keyboardDetuneFMCore emitStereo smpDir = do
   arrange <- keyboardDetuneFMConstVolume smpDir
   amp <-
      CausalRender.run
         (emitStereo <<<
          Causal.envelopeStereo <<<
          Causal.map Serial.upsample *** arr Stereo.unMultiValue)
   return $ \chan initPgm rate ->
      amp
      <<<
      MIO.controllerExponential chan controllerVolume (0.001, 1) (0.2::Float)
      &+&
      arrange chan initPgm rate

keyboardDetuneFMConstVolume ::
   (PathClass.AbsRel ar, Check.C msg) =>
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program -> SampleRate Real ->
       PIO.T (MIO.Events msg) (SV.Vector (Stereo.T Vector)))
keyboardDetuneFMConstVolume smpDir = do
   arrange <- CausalSt.makeArranger

   tine <- Instr.tineStereoFM
   ping <- Instr.pingStereoReleaseFM
   filterSaw <- Instr.filterSawStereoFM
   bellNoise <- Instr.bellNoiseStereoFM

   wind <- Instr.wind
   windPhaser <- Instr.windPhaser
   string <- Instr.softStringShapeFM
   fmString <- Instr.fmStringStereoFM
   helixNoise <- InstrPlug.helixNoise
   arcs <- sequence $
      Instr.cosineStringStereoFM :
      Instr.arcSawStringStereoFM :
      Instr.arcSineStringStereoFM :
      Instr.arcSquareStringStereoFM :
      Instr.arcTriangleStringStereoFM :
      []

   helixSound <- Instr.helixSound
   sampledSound <- Instr.sampledSound

   syllables <-
      fmap concat $
      mapM (Sample.loadRanges smpDir) $
      Sample.tomatensalat :
      Sample.hal :
      Sample.graphentheorie :
      []

   let frequencyControlPercussive =
          MCS.controllerLinear controllerDetune (0,0.005) 0.001
          &+&
          MCS.bendWheelPressure 2 0.04 0.03

       frequencyControlString =
          MCS.controllerLinear controllerDetune (0,0.01) 0.005
          &+&
          MCS.bendWheelPressure 2 0.04 0.03

   let tineProc rate vel freq =
          tine rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlPercussive
              &+&
              (((fmap stair ^<<
                 MCS.controllerLinear controllerTimbre0 (0.5,6.5) 2)
                &+&
                MCS.controllerLinear controllerTimbre1 (0,1.5) 1)
               &+&
               frequencyControlPercussive))

       pingProc rate vel freq =
          ping rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlPercussive
              &+&
              ((MCS.controllerExponential controllerTimbre0 (1/pi,10) 0.05
                &+&
                controllerExponentialDim controllerTimbre1
                    (DN.time 0.01, DN.time 10) (DN.time 5))
               &+&
               ((MCS.controllerLinear Ctrl.soundController5 (0,10) 2
                 &+&
                 controllerExponentialDim Ctrl.soundController7
                    (DN.time 0.03, DN.time 1) (DN.time 0.5))
                &+&
                frequencyControlPercussive)))

       filterSawProc rate vel freq =
          filterSaw rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlPercussive
              &+&
              ((controllerExponentialDim controllerTimbre0
                   (DN.frequency 100, DN.frequency 10000)
                   (DN.frequency 1000)
                &+&
                controllerExponentialDim controllerTimbre1
                   (DN.time 0.1, DN.time 1)
                   (DN.time 0.6))
               &+&
               frequencyControlPercussive))

       bellNoiseProc rate vel freq =
          bellNoise rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlPercussive
              &+&
              ((MCS.controllerLinear controllerTimbre0 (0,1) 0.3
                &+&
                MCS.controllerExponential controllerTimbre1 (1,1000) 100)
               &+&
               frequencyControlPercussive))

       windProc rate vel freq =
          wind rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlString
              &+&
              (MCS.controllerExponential controllerTimbre1 (1,1000) 100
               &+&
               MCS.bendWheelPressure 12 0.8 0))

       windPhaserProc rate vel freq =
          windPhaser rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlString
              &+&
              (MCS.controllerLinear controllerTimbre0 (0,1) 0.5
               &+&
               (controllerExponentialDim controllerDetune
                   (DN.frequency 50, DN.frequency 5000) (DN.frequency 500)
                &+&
                (MCS.controllerExponential controllerTimbre1 (1,1000) 100
                 &+&
                 MCS.bendWheelPressure 12 0.8 0))))

       stringProc rate vel freq =
          string rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlString
              &+&
              (MCS.controllerExponential controllerTimbre0 (1/pi,10) 0.05
               &+&
               frequencyControlString))

       fmStringProc rate vel freq =
          fmString rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlString
              &+&
              ((MCS.controllerLinear controllerTimbre0 (0,0.5) 0.2
                &+&
                MCS.controllerExponential controllerTimbre1 (1/pi,10) 0.05)
               &+&
               frequencyControlString))

       helixNoiseProc rate vel freq =
          helixNoise rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlString
              &+&
              (MCS.controllerExponential controllerTimbre0 (1,0.01) 0.1
               &+&
               frequencyControlString))

       makeArc proc rate vel freq =
          proc rate vel freq
          <<<
          Zip.arrowSecond
             (timeControlString
              &+&
              (MCS.controllerLinear controllerTimbre0 (0.5,9.5) 1.5
               &+&
               frequencyControlString))

       sampled smp rate vel freq =
          smp rate vel freq
          <<<
          Zip.arrowSecond frequencyControlPercussive

       helixed smp rate vel freq =
          smp rate vel freq
          <<<
          Zip.arrowSecond
             (MCS.controllerExponential Ctrl.attackTime (0.25, 4) 1
              &+&
              frequencyControlPercussive)

       bank =
          Map.fromAscList $ zip [VoiceMsg.toProgram 0 ..] $
          [tineProc, pingProc, filterSawProc, bellNoiseProc,
           stringProc, fmStringProc] ++
          map makeArc arcs ++ windProc : windPhaserProc :
          ([helixed . helixSound, sampled . sampledSound] <*> syllables) ++
          helixNoiseProc :
          []

   return $ \chan initPgm rate ->
      arrange
      <<<
      arr shortTime
      <<<
      MIO.sequenceModulatedMultiProgram chan initPgm
         (\pgm -> Map.findWithDefault pingProc pgm bank rate)
      <<<
      id &+& MCS.fromChannel chan


keyboardMultiChannel ::
   (PathClass.AbsRel ar, Check.C msg) =>
   Path.Dir ar ->
   IO (SampleRate Real ->
       PIO.T (MIO.Events msg) (SV.Vector (Stereo.T Real)))
keyboardMultiChannel smpDir = do
   proc <-
      keyboardDetuneFMCore
         (Causal.map StereoInt.interleave)
         smpDir
   mix <- CausalRender.run Causal.mix

   return $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      foldl1
         (\x y -> mix <<< Zip.arrowFanout x y)
         (map
             (\chan ->
                proc (ChannelMsg.toChannel chan) (VoiceMsg.toProgram 0)
                     sampleRate)
             [0 .. 3])



data Phoneme = Phoneme Bool VoiceMsg.Velocity VoiceMsg.Pitch

instance Check.C Phoneme where
   note _chan (Phoneme on v p) = Just (v, p, on)


voderSplit ::
   (Check.C msg, Construct.C msg, Arrow arrow) =>
   ChannelMsg.Channel ->
   arrow
      (MIO.Events msg)
      (Zip.T
          (MIO.Events Phoneme)
          (MIO.Events msg))
voderSplit chan =
   arr $
   uncurry Zip.Cons .
   EventListTT.unzip .
   fmap
      (ListHT.unzipEithers .
       fmap (\ev ->
          case Check.noteExplicitOff chan ev of
             Nothing -> Right ev
             Just (v,p,b) ->
                if p >= VoiceMsg.toPitch 36
                  then
                     let p0 = VoiceMsg.increasePitch (-36) p
                     in  if p0 <= VoiceMsg.toPitch 29
                           then Left $ Phoneme b v p0
                           else Right $ Construct.note chan
                                   (v, VoiceMsg.increasePitch (-12) p, b)
                  else Right ev))

voder ::
   (PathClass.AbsRel ar, Check.C msg, Construct.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   Speech.VowelSynth ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> PIO.T (MIO.Events msg) b)
voder emitStereo voice smpDir = do
   carrier <- keyboardDetuneFMCore (arr Stereo.multiValue) smpDir
   arrange <- CausalSt.makeArranger
   interleave <- CausalRender.run (emitStereo <<^ Stereo.unMultiValue)

   return $ \chan initPgm sampleRate ->
      interleave
      <<<
      arrange
      <<<
      arr shortTime
      <<<
      MIO.sequenceModulatedMultiProgramVelocityPitch
         chan (VoiceMsg.toProgram 0)
         (\ _pgm _vel -> voice sampleRate)
      <<<
      Zip.arrowSecond (carrier chan initPgm sampleRate)
      <<<
      voderSplit chan

voderBand ::
   (PathClass.AbsRel ar, Check.C msg, Construct.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> PIO.T (MIO.Events msg) b)
voderBand emitStereo smpDir = do
   voice <- Speech.vowelBand
   voder emitStereo voice smpDir

voderMask ::
   (PathClass.AbsRel ar, Check.C msg, Construct.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> PIO.T (MIO.Events msg) b)
voderMask emitStereo smpDir = do
   voice <-
      Speech.vowelMask <*>
      fmap
         (Map.mapMaybe (\(typ,smp) ->
            toMaybe (typ==Speech.Filtered Speech.Continuous Speech.Voiced) smp))
         Speech.loadMasksKeyboard
   voder emitStereo voice smpDir


voderEnv ::
   (PathClass.AbsRel ar, Check.C msg, Construct.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   Speech.VowelSynthEnv ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> PIO.T (MIO.Events msg) b)
voderEnv emitStereo voice smpDir = do
   carrier <- keyboardDetuneFMConstVolume smpDir
   arrange <- CausalSt.makeArranger
   amp <-
      CausalRender.run
         (emitStereo <<<
          Causal.envelopeStereo <<<
          Causal.map Serial.upsample *** arr Stereo.unMultiValue)

   return $ \chan initPgm sampleRate ->
      amp
      <<<
      MIO.controllerExponential chan controllerVolume (0.001, 1) (0.2::Float)
      &+&
      (arrange
       <<<
       arr shortTime
       <<<
       MIO.sequenceModulatedMultiProgramVelocityPitch
          chan (VoiceMsg.toProgram 0)
          (\ _pgm vel -> voice sampleRate (MV.velocity vel))
       <<<
       Zip.arrowSecond
          (Zip.arrowFanout
             (timeControlString <<< MCS.fromChannel chan)
             (carrier chan initPgm sampleRate))
       <<<
       voderSplit chan)

voderMaskEnv ::
   (PathClass.AbsRel ar, Check.C msg, Construct.C msg, POut.Default b) =>
   Causal.T (Stereo.T VectorValue) (POut.Element b) ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> PIO.T (MIO.Events msg) b)
voderMaskEnv emitStereo smpDir = do
   voice <- Speech.phonemeMask <*> Speech.loadMasksKeyboard
   voderEnv emitStereo voice smpDir


voderSeparated ::
   (PathClass.AbsRel ar, Render.RunArg p,
    Check.C msg, Construct.C msg, POut.Default b) =>
   (Render.DSLArg p -> Causal.T (Stereo.T VectorValue) (POut.Element b)) ->
   Speech.VowelSynthEnv ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> p -> PIO.T (MIO.Events msg) b)
voderSeparated emitStereo voice smpDir = do
   carrier <- keyboardDetuneFMCore (arr Stereo.multiValue) smpDir
   arrange <- CausalSt.makeArranger
   amp <-
      CausalRender.run $ \p ->
         (emitStereo p <<<
          Causal.envelopeStereo <<<
          Causal.map Serial.upsample *** arr Stereo.unMultiValue)

   return $ \carrierChan phonemeChan initPgm sampleRate p ->
      amp p
      <<<
      MIO.controllerExponential phonemeChan controllerVolume (0.001, 1) (0.2::Float)
      &+&
      (arrange
       <<<
       arr shortTime
       <<<
       MIO.sequenceModulatedMultiProgramVelocityPitch
          phonemeChan (VoiceMsg.toProgram 0)
          (\ _pgm vel -> voice sampleRate (MV.velocity vel))
       <<<
       Zip.arrowFanout id
          (Zip.arrowFanout
             (timeControlString <<< MCS.fromChannel phonemeChan)
             (carrier carrierChan initPgm sampleRate)))

voderMaskSeparated ::
   (PathClass.AbsRel ar, Render.RunArg p,
    Check.C msg, Construct.C msg, POut.Default b) =>
   (Render.DSLArg p -> Causal.T (Stereo.T VectorValue) (POut.Element b)) ->
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> ChannelMsg.Channel -> VoiceMsg.Program ->
       SampleRate Real -> p -> PIO.T (MIO.Events msg) b)
voderMaskSeparated emitStereo smpDir = do
   voice <- Speech.phonemeMask <*> Speech.loadMasksGrouped
   voderSeparated emitStereo voice smpDir

voderMaskMulti ::
   (PathClass.AbsRel ar, Check.C msg, Construct.C msg) =>
   Path.Dir ar ->
   IO (SampleRate Real ->
       PIO.T (MIO.Events msg) (SV.Vector (Stereo.T Real)))
voderMaskMulti smpDir = do
   mix <- CausalRender.run Causal.mix
   proc <-
      voderMaskSeparated
         (const $ Causal.map StereoInt.interleave)
         smpDir

   return $ \ sampleRate ->
      arr SigStL.unpackStereoStrict
      <<<
      foldl1
         (\x y -> mix <<< Zip.arrowFanout x y)
         (map
             (\chan ->
                proc
                   (ChannelMsg.toChannel chan)
                   (ChannelMsg.toChannel $ succ chan)
                   (VoiceMsg.toProgram 4)
                   sampleRate ())
             [0, 2, 4, 6])
