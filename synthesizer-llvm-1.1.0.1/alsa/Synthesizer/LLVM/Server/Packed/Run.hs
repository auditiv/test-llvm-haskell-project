module Synthesizer.LLVM.Server.Packed.Run where

import qualified Synthesizer.LLVM.Server.Packed.Instrument as Instr
import qualified Synthesizer.LLVM.Server.SampledSound as Sample
import qualified Synthesizer.LLVM.Server.Option as Option
import Synthesizer.LLVM.Server.ALSA (Output, play, startMessage)
import Synthesizer.LLVM.Server.CommonPacked
          (Vector, VectorSize, vectorSize, stair)
import Synthesizer.LLVM.Server.Common

import qualified Synthesizer.ALSA.EventList as Ev
import qualified Sound.ALSA.Sequencer.Event as Event

import qualified Synthesizer.MIDI.EventList as MidiEv
import qualified Synthesizer.MIDI.PiecewiseConstant as PC
import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.MIDI.Generic as Gen

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified Synthesizer.LLVM.Filter.Universal as UniFilterL
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.Wave as WaveL
import Synthesizer.LLVM.Causal.Process (($<), ($*))

import LLVM.DSL.Expression (Exp)

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Data.StorableVector.Lazy as SVL

import qualified Synthesizer.Plain.Filter.Recursive    as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter

import qualified Sound.MIDI.Controller as Ctrl
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.MixedTime as EventListMT

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path

import qualified Control.Applicative.HT as App
import Control.Arrow (arr, (<<<), (^<<), (<<^))
import Control.Applicative (pure, liftA2, liftA3, (<*>))
import Control.Monad.Trans.State (evalState)

import Control.Exception (bracket)

import Algebra.IntegralDomain (divUp)

import NumericPrelude.Numeric (zero, (^?), (+))
import Prelude hiding (Real, break, (+))



{-# INLINE withMIDIEventsMono #-}
withMIDIEventsMono ::
   Option.T ->
   Output handle (SigSt.T Real) a ->
   (SigSt.ChunkSize -> SampleRate Real ->
    EventList.T Ev.StrictTime [Event.T] -> SigSt.T Vector) -> IO a
withMIDIEventsMono opt output process = do
   putStrLn startMessage
   case output opt of
      (open,close,write) ->
         bracket open (close . snd) $ \((chunkSize,rate),h) ->
            let rrate = fromIntegral rate :: Double
            in  Ev.withMIDIEvents
                   (Option.clientName opt)
                   (fromIntegral chunkSize / rrate)
                   (rrate / fromIntegral vectorSize)
                   (write h .
                    SigStL.unpack .
                    process (SigSt.chunkSize $ divUp chunkSize vectorSize)
                       (Option.SampleRate $ fromIntegral rate))

type StereoVector = StereoInt.T VectorSize Real

{-# INLINE withMIDIEventsStereo #-}
withMIDIEventsStereo ::
   Option.T ->
   Output handle (SigSt.T (Stereo.T Real)) a ->
   (SigSt.ChunkSize -> SampleRate Real ->
    EventList.T Ev.StrictTime [Event.T] -> SigSt.T StereoVector) ->
   IO a
withMIDIEventsStereo opt output process = do
   putStrLn startMessage
   case output opt of
      (open,close,write) ->
         bracket open (close . snd) $ \((chunkSize,rate),h) ->
            let rrate = fromIntegral rate :: Double
            in  Ev.withMIDIEvents
                   (Option.clientName opt)
                   (fromIntegral chunkSize / rrate)
                   (rrate / fromIntegral vectorSize)
                   (write h .
                    SigStL.unpackStereo .
                    process (SigSt.chunkSize $ divUp chunkSize vectorSize)
                       (Option.SampleRate $ fromIntegral rate))


frequencyModulation :: IO ()
frequencyModulation = do
   opt <- Option.get
   osc <-
      Render.run $
      wrapped $ \(Instr.Modulation fm) ->
      constant frequency 10 $ \speed _sr ->
         ((CausalPS.osci WaveL.triangle $< zero)
           $* Instr.frequencyFromBendModulation speed fm)
   withMIDIEventsMono opt play $ \vectorChunkSize sampleRate ->
      osc vectorChunkSize sampleRate . flip (,) (880::Real) .
      evalState (PC.bendWheelPressure (Option.channel opt) 2 0.04 (0.03::Real))



keyboard :: IO ()
keyboard = do
   opt <- Option.get
   sound <- Instr.pingRelease $/ 0.4 $/ 0.1
   amp <- CausalRender.run CausalPS.amplify
   arrange <- SigStL.makeArranger
   withMIDIEventsMono opt play $ \vectorChunkSize sampleRate ->
      pioApply (amp (0.2::Real)) .
      arrange vectorChunkSize .
      evalState
         (Gen.sequence (Option.channel opt) $
          sound vectorChunkSize sampleRate)

keyboardStereo :: IO ()
keyboardStereo = do
   opt <- Option.get
   sound <- Instr.pingStereoRelease $/ 0.4 $/ 0.1
   amp <-
      CausalRender.run $ \vol ->
         Causal.map StereoInt.interleave <<<
         CausalPS.amplifyStereo vol <<^ Stereo.unMultiValue
   arrange <- SigStL.makeArranger
   withMIDIEventsStereo opt play $ \vectorChunkSize sampleRate ->
      pioApply (amp (0.2 :: Real)) .
      arrange vectorChunkSize .
      evalState
         (Gen.sequence (Option.channel opt) $
          sound vectorChunkSize sampleRate)

keyboardFM :: IO ()
keyboardFM = do
   opt <- Option.get
   str <- Instr.softStringFM
   amp <-
      CausalRender.run $ \vol ->
         Causal.map StereoInt.interleave <<<
         CausalPS.amplifyStereo vol <<^ Stereo.unMultiValue
   arrange <- SigStL.makeArranger
   withMIDIEventsStereo opt play $ \vectorChunkSize sampleRate ->
      pioApply (amp (0.2 :: Real)) .
      arrange vectorChunkSize .
      evalState
         (do fm <- PC.bendWheelPressure (Option.channel opt) 2 0.04 0.03
             Gen.sequenceModulated
                fm (Option.channel opt) (flip str sampleRate))

keyboardFMMulti :: IO ()
keyboardFMMulti = do
   opt <- Option.get
   str <- Instr.softStringFM
   tin <- Instr.tineStereoFM $/ 0.4 $/ 0.1
   amp <-
      CausalRender.run $ \vol ->
         Causal.map StereoInt.interleave <<<
         CausalPS.amplifyStereo vol <<^ Stereo.unMultiValue
   arrange <- SigStL.makeArranger
   withMIDIEventsStereo opt play $ \vectorChunkSize sampleRate ->
      pioApply (amp (0.2 :: Real)) .
      arrange vectorChunkSize .
      evalState
         (do fm <- PC.bendWheelPressure (Option.channel opt) 2 0.04 0.03
             Gen.sequenceModulatedMultiProgram
                fm (Option.channel opt)
                (VoiceMsg.toProgram 1)
                (map (\sound fmlocal -> sound fmlocal $ sampleRate)
                    [str, tin vectorChunkSize]))

controllerFMDepth1, controllerFMDepth2, controllerFMDepth3, controllerFMDepth4,
   controllerFMPartial1, controllerFMPartial2, controllerFMPartial3, controllerFMPartial4
   :: VoiceMsg.Controller
controllerFMDepth1 = Ctrl.soundController3
controllerFMDepth2 = Ctrl.soundController5
controllerFMDepth3 = Ctrl.soundController7
controllerFMDepth4 = Ctrl.soundController8
controllerFMPartial1 = Ctrl.generalPurpose1
controllerFMPartial2 = Ctrl.generalPurpose2
controllerFMPartial3 = Ctrl.effect1Depth
controllerFMPartial4 = Ctrl.effect2Depth

keyboardDetuneFMCore ::
   (PathClass.AbsRel ar) =>
   Path.Dir ar ->
   IO (ChannelMsg.Channel -> VoiceMsg.Program ->
       SVL.ChunkSize -> SampleRate Real ->
       MidiEv.Filter Event.T (SigSt.T StereoVector))
keyboardDetuneFMCore smpDir = do
   str0 <- Instr.softStringDetuneFM
   ssh0 <- Instr.softStringShapeFM
   css0 <- Instr.cosineStringStereoFM
   asw0 <- Instr.arcSawStringStereoFM
   asn0 <- Instr.arcSineStringStereoFM
   asq0 <- Instr.arcSquareStringStereoFM
   atr0 <- Instr.arcTriangleStringStereoFM
   wnd0 <- Instr.wind
   wnp0 <- Instr.windPhaser
   fms0 <- Instr.fmStringStereoFM
   tin0 <- Instr.tineStereoFM
   tnc0 <- Instr.tineControlledFM
   fnd0 <- Instr.fenderFM
   tnb0 <- Instr.tineBankFM
   rfm0 <- Instr.resonantFMSynth
   png0 <- Instr.pingStereoRelease
   pngFM0 <- Instr.pingStereoReleaseFM
   sqr0 <- Instr.squareStereoReleaseFM
   bel0 <- Instr.bellStereoFM
   ben0 <- Instr.bellNoiseStereoFM
   flt0 <- Instr.filterSawStereoFM
   brs0 <- Instr.brass

   syllables <-
      liftA2 map Instr.sampledSound $
      fmap concat $
      mapM (Sample.loadRanges smpDir) $
      Sample.tomatensalat :
      Sample.hal :
      Sample.graphentheorie :
      []


   arrange <- SigStL.makeArranger
   amp <-
      CausalRender.run $ \ctrl ->
         (Causal.map StereoInt.interleave <<<
          Causal.envelopeStereo $< Instr.piecewiseConstantVector ctrl)
            <<^ Stereo.unMultiValue
   return $ \chan pgm vcsize sr -> do
      let
       evHead =
          fmap (EventListMT.switchBodyL
             (error "empty controller stream") const)
       flt = evalState $
          App.lift6 (\rel -> flt0 (4*rel) rel)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             (evHead $
              PCS.controllerExponential controllerTimbre0 (100,10000) 1000)
             (evHead $
              PCS.controllerExponential controllerTimbre1 (0.1,1) 0.1)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.04 0.03)
       png =
          (\rel -> png0 (4*rel) rel vcsize) .
          evalState
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
       pngFM = evalState $
          App.lift6 (\rel det phs shp -> pngFM0 (4*rel) rel det shp 2 phs)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             (evHead $
              PCS.controllerLinear controllerTimbre0 (0,1) 1)
             (PCS.controllerExponential controllerTimbre1 (1/pi,0.001) 0.05)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.04 0.03)
       sqr = evalState $
          App.lift6 (\rel -> sqr0 (4*rel) rel)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             (PCS.controllerExponential controllerTimbre0 (1/pi,0.001) 0.05)
             (PCS.controllerLinear controllerTimbre1 (0,0.25) 0.25)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.04 0.03)
       tin = evalState $
          liftA3 (\rel -> tin0 (4*rel) rel)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.04 0.03)
       tnc = evalState $
          App.lift6 (\rel -> tnc0 (4*rel) rel)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             (fmap (fmap stair) $
              PCS.controllerLinear controllerTimbre0 (0.5,6.5) 2)
             (PCS.controllerLinear controllerTimbre1 (0,1.5) 1)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.04 0.03)
       fnd = evalState $
          pure (\rel -> fnd0 (4*rel) rel)
             <*> (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             <*> (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             <*> (fmap (fmap stair) $
              PCS.controllerLinear controllerTimbre0 (0.5,20.5) 14)
             <*> (PCS.controllerLinear controllerTimbre1 (0,1.5) 0.3)
             <*> (PCS.controllerLinear controllerFMDepth1 (0,1) 0.25)
             <*> (pure vcsize)
             <*> (PCS.bendWheelPressure 2 0.04 0.03)
       tnb = evalState $
          pure (\rel -> tnb0 (4*rel) rel)
             <*> (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             <*> (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             <*> (PCS.controllerLinear controllerFMDepth1 (0,2) 0)
             <*> (PCS.controllerLinear controllerFMDepth2 (0,2) 0)
             <*> (PCS.controllerLinear controllerFMDepth3 (0,2) 0)
             <*> (PCS.controllerLinear controllerFMDepth4 (0,2) 0)
             <*> (PCS.controllerLinear controllerFMPartial1 (0,1) 1)
             <*> (PCS.controllerLinear controllerFMPartial2 (0,1) 0)
             <*> (PCS.controllerLinear controllerFMPartial3 (0,1) 0)
             <*> (PCS.controllerLinear controllerFMPartial4 (0,1) 0)
             <*> (pure vcsize)
             <*> (PCS.bendWheelPressure 2 0.04 0.03)
       rfm = evalState $
          pure (\rel -> rfm0 (4*rel) rel)
             <*> (evHead $
              PCS.controllerExponential controllerAttack (0.03,0.3) 0.1)
             <*> (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             <*> (PCS.controllerExponential controllerTimbre1 (1,100) 30)
             <*> (PCS.controllerLinear controllerTimbre0 (1,15) 3)
             <*> (PCS.controllerExponential controllerFMDepth1 (0.005,0.5) 0.1)
             <*> (pure vcsize)
             <*> (PCS.bendWheelPressure 2 0.04 0.03)
       bel = evalState $
          App.lift4 (\rel -> bel0 (2*rel) rel)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,1.0) 0.3)
             (PCS.controllerLinear controllerDetune (0,0.005) 0.001)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.05 0.02)
       ben = evalState $
          App.lift5 (\rel -> ben0 (2*rel) rel)
             (evHead $
              PCS.controllerExponential controllerAttack (0.03,1.0) 0.3)
             (PCS.controllerLinear controllerTimbre0 (0,1) 0.3)
             (PCS.controllerExponential controllerTimbre1 (1,1000) 100)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.05 0.02)
       str = evalState $
          liftA3 str0
             (evHead $
              PCS.controllerExponential controllerAttack (0.02,2) 0.5)
             (PCS.controllerLinear controllerDetune (0,0.01) 0.005)
             (PCS.bendWheelPressure 2 0.04 0.03)
       ssh = evalState $
          App.lift4 ssh0
             (evHead $
              PCS.controllerExponential controllerAttack (0.02,2) 0.5)
             (PCS.controllerLinear controllerDetune (0,0.01) 0.005)
             (PCS.controllerExponential controllerTimbre0 (1/pi,0.001) 0.05)
             (PCS.bendWheelPressure 2 0.04 0.03)
       makeArc gen = evalState $
          App.lift4 gen
             (evHead $
              PCS.controllerExponential controllerAttack (0.02,2) 0.5)
             (PCS.controllerLinear controllerDetune (0,0.01) 0.005)
             (PCS.controllerLinear controllerTimbre0 (0.5,9.5) 1.5)
             (PCS.bendWheelPressure 2 0.04 0.03)
       css = makeArc css0
       asw = makeArc asw0
       asn = makeArc asn0
       asq = makeArc asq0
       atr = makeArc atr0
       fms = evalState $
          App.lift5 fms0
             (evHead $
              PCS.controllerExponential controllerAttack (0.02,2) 0.5)
             (PCS.controllerLinear controllerDetune (0,0.01) 0.005)
             (PCS.controllerLinear controllerTimbre0 (0,0.5) 0.2)
             (PCS.controllerExponential controllerTimbre1 (0.001,10) 0.1)
             (PCS.bendWheelPressure 2 0.04 0.03)
       wnd = evalState $
          liftA3 wnd0
             (evHead $
              PCS.controllerExponential controllerAttack (0.02,2) 0.5)
             (PCS.controllerExponential controllerTimbre1 (1,1000) 100)
             (PCS.bendWheelPressure 12 0.8 0)
       wnp = evalState $
          App.lift5 wnp0
             (evHead $
              PCS.controllerExponential controllerAttack (0.02,2) 0.5)
             (PCS.controllerLinear controllerTimbre0 (0,1) 0.5)
             (PCS.controllerExponential controllerDetune (50,5000) 500)
             (PCS.controllerExponential controllerTimbre1 (1,1000) 100)
             (PCS.bendWheelPressure 12 0.8 0)
       brs = evalState $
          App.lift6
             (\rel det t0 peak -> brs0 (rel/2) 1.5 (rel/2) rel rel peak det t0)
             (evHead $
              PCS.controllerExponential controllerAttack (0.01,0.1) 0.01)
             (PCS.controllerLinear controllerDetune (0,0.01) 0.005)
             (PCS.controllerExponential controllerTimbre0 (1/pi,0.001) 0.05)
             (evHead $
              PCS.controllerLinear controllerTimbre1 (1,5) 3)
             (pure vcsize)
             (PCS.bendWheelPressure 2 0.04 0.03)
       freqMod =
          evalState
             (PCS.bendWheelPressure 2 0.04 0.03)


      volume <-
         PC.controllerExponential chan
            controllerVolume
            (0.001, 1) 0.2

      ctrls <- PCS.fromChannel chan

      fmap (pioApply (amp volume) . arrange vcsize) $
         Gen.sequenceModulatedMultiProgram
            ctrls chan pgm
            (map (\sound fm -> sound fm $ sr) $
             [tnc, fnd, pngFM, flt, bel, ben, sqr, brs,
              ssh, fms, css, asn, atr, asq, asw, wnp] ++
             map (.freqMod) syllables ++
             [str, wnd, png, rfm, tin, tnb])


keyboardDetuneFM :: IO ()
keyboardDetuneFM = do
   opt <- Option.get
   proc <- keyboardDetuneFMCore (Option.sampleDirectory opt)
   withMIDIEventsStereo opt play $ \vectorChunkSize sampleRate ->
      evalState
         (proc (Option.channel opt) (VoiceMsg.toProgram 0)
            vectorChunkSize sampleRate)

keyboardFilter :: IO ()
keyboardFilter = do
   opt <- Option.get
   proc <- keyboardDetuneFMCore (Option.sampleDirectory opt)
   mix <- CausalRender.run $ \xs ->
      arr id
      +
      (Causal.map (StereoInt.amplify 0.5)
       <<<
       Causal.fromSignal xs)

   lowpass0 <-
      CausalRender.run $ \cutoff ->
      Causal.map StereoInt.interleave
      <<<
--      CausalPS.amplifyStereo 0.1 <<<
      CausalPS.pack
         (Causal.stereoFromMonoControlled
             (UniFilter.lowpass ^<< UniFilterL.causalExp) $<
          Sig.interpolateConstant (fromIntegral vectorSize :: Exp Int)
             (UniFilterL.unMultiValueParameter <$> piecewiseConstant cutoff))
      <<<
      Causal.map StereoInt.deinterleave
   let lowpass ::
          Option.SampleRate Real -> PC.T Real -> PC.T Real ->
          SigSt.T StereoVector -> SigSt.T StereoVector
       lowpass (Option.SampleRate sr) resons freqs =
          pioApply $
          lowpass0 $ fmap UniFilter.parameter $
          PC.zipWith FiltR.Pole resons $ fmap (/ sr) freqs

   withMIDIEventsStereo opt play $ \vectorChunkSize sampleRate ->
      evalState
         (do {-
             It is important to retrieve the global controllers
             before they are filtered out by PCS.fromChannel.
             -}
             let freqBnd v = 880 * 2^?(v/24)
             freq <-
                PC.controllerExponential (Option.extraChannel opt)
                   controllerFilterCutoff
                   (freqBnd (-64), freqBnd 63) 5000
             resonance <-
                PC.controllerExponential (Option.extraChannel opt)
                   controllerFilterResonance
                   (1, 100) 1
             filterMusic <-
                proc (Option.extraChannel opt) (VoiceMsg.toProgram 8)
                   vectorChunkSize sampleRate
             pureMusic <-
                proc (Option.channel opt) (VoiceMsg.toProgram 0)
                   vectorChunkSize sampleRate
             return
                (pioApply (mix pureMusic) $
                 lowpass sampleRate resonance freq filterMusic))
