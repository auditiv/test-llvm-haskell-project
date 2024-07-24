module Synthesizer.LLVM.Server.CausalPacked.Test where

import qualified Synthesizer.LLVM.Server.CausalPacked.Speech as Speech
import qualified Synthesizer.LLVM.Server.CausalPacked.InstrumentPlug as InstrFP
import qualified Synthesizer.LLVM.Server.CausalPacked.Instrument as Instr
import qualified Synthesizer.LLVM.Server.SampledSound as Sample
import qualified Synthesizer.LLVM.Server.Option as Option
import qualified Synthesizer.LLVM.Server.Default as Default
import Synthesizer.LLVM.Server.CausalPacked.Common (chopEvents)
import Synthesizer.LLVM.Server.CausalPacked.Arrange
          ((&+&), shortTime, controllerExponentialDim)
import Synthesizer.LLVM.Server.CommonPacked (Vector)
import Synthesizer.LLVM.Server.Common hiding (Instrument)

import qualified Sound.ALSA.Sequencer.Event as Event
-- import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Synthesizer.MIDI.Generic as Gen

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial

import qualified Sound.MIDI.Controller as Ctrl
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Synthesizer.CausalIO.Gate as Gate
import qualified Synthesizer.Zip as Zip

import qualified Synthesizer.ALSA.Storable.Play as Play
import qualified Synthesizer.ALSA.CausalIO.Process as PAlsa
import Synthesizer.MIDI.Storable (Instrument)

import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.MIDI.CausalIO.ControllerSet as MCS
import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.LLVM.Causal.FunctionalPlug as FP
import qualified Synthesizer.LLVM.Causal.Functional as F
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Storable.Process as CausalSt
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.Wave as Wave
import Synthesizer.LLVM.Causal.Process (($*), ($<))

import qualified Synthesizer.Generic.Cut          as CutG
import qualified Synthesizer.Storable.Cut         as CutSt
import qualified Data.StorableVector.Lazy         as SVL
import qualified Data.StorableVector              as SV

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.BodyTime  as EventListBT

import qualified LLVM.DSL.Expression as Expr

import System.Path ((</>))

import Control.Arrow (arr, (***), (<<<), (<<^), (^<<))
import Control.Category (id)
import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.Trans.State (evalState)

import qualified Data.Map as Map

import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Number.DimensionTerm as DN

import Data.Word (Word8, Word32)
import Data.Int (Int32)

import qualified System.Unsafe as Unsafe
import qualified System.IO as IO
import Foreign.Storable (Storable)
import Control.Exception (bracket)

import Prelude hiding (Real, id)


sampleRate :: SampleRate Real
sampleRate = Default.sampleRate

{- |
try to reproduce a space leak
-}
sequencePlain :: IO ()
sequencePlain =
   SVL.writeFile "/tmp/test.f32" $
--   print $ last $ SVL.chunks $
      CutSt.arrange Play.defaultChunkSize $
      evalState (Gen.sequence Default.channel (error "no sound" :: Instrument Real Real)) $
      let evs = EventList.cons 10 ([]::[Event.T]) evs
      in  evs


-- see playFromEvents
writeTest ::
   (CutG.Read t, Storable a) =>
   PIO.T t (SV.Vector a) -> [t] -> IO ()
writeTest (PIO.Cons next create delete) evsChunky =
   IO.withFile "/tmp/test.f32" IO.WriteMode $ \h ->
      bracket create delete $
         let loop evs s0 =
                case evs of
                   [] -> return ()
                   chunk : rest -> do
                      (pcm, s1) <- next chunk s0
                      SV.hPut h pcm
                      when
                         (CutG.length pcm >= CutG.length chunk)
                         (loop rest s1)
         in  loop evsChunky

render :: IO ()
render = do
   ping <- Instr.pingRelease $/ 1 $/ 0.1  -- leaky
--   ping <- Instr.ping  -- not leaky

   writeTest (ping sampleRate 10 440) $
      replicate 10000 $ Gate.chunk 512 Nothing

sequenceEvents :: [PAlsa.Events] -> IO ()
sequenceEvents evs = do
   arrange <- CausalSt.makeArranger

   ping <- Instr.pingRelease $/ 1 $/ 0.1  -- leaky
--   ping <- Instr.ping  -- not leaky

   let proc =
          arrange
          <<<
          arr shortTime
          <<<
          MIO.sequenceCore
             Default.channel
             (\ _pgm -> ping sampleRate)

   writeTest proc evs

sequenceNothing :: IO ()
sequenceNothing =
   sequenceEvents $
      let evs = EventList.cons 10 [] evs
      in  chopEvents 512 $ EventListTM.takeTime (10^(7::Int)) evs


noteEvent ::
   Event.NoteEv ->
   Word8 ->
   Word8 ->
   Word8 ->
   Event.T
noteEvent mode chan pitch velocity =
   -- Event.simple (Connect.toSubscribers Addr.subscribers) $
   Event.simple Addr.subscribers $ Event.NoteEv mode $
   Event.simpleNote
      (Event.Channel $ fromIntegral chan)
      (Event.Pitch $ fromIntegral pitch)
      (Event.Velocity $ fromIntegral velocity)

ctrlEvent ::
   Word8 ->
   Word32 ->
   Int32 ->
   Event.T
ctrlEvent chan cc cval =
   -- Event.simple (Connect.toSubscribers Addr.subscribers) $
   Event.simple Addr.subscribers $
   Event.CtrlEv Event.Controller $
   Event.Ctrl
      (Event.Channel $ fromIntegral chan)
      (Event.Parameter $ fromIntegral cc)
      (Event.Value $ fromIntegral cval)

sequenceSingleLong :: IO ()
sequenceSingleLong = do
   sequenceEvents $
      let evs = EventList.cons 10 [] evs
      in  chopEvents 512 $
          EventListTM.takeTime (10^(7::Int)) $
          EventList.cons 0 [noteEvent Event.NoteOn 0 60 64] evs

sequenceSingleShort :: IO ()
sequenceSingleShort = do
   sequenceEvents $
      let evs = EventList.cons 10 [] evs
      in  chopEvents 512 $
          EventListTM.takeTime (10^(7::Int)) $
          EventList.cons 0 [noteEvent Event.NoteOn 0 60 64] $
          EventList.cons 10 [noteEvent Event.NoteOff 0 60 64] evs

{-
Although it consumes constant memory,
the memory usage is quite high,
e.g. 40MB for chunk size 100000 and peiod 1100.
This might be caused by the large overlapping in the release phases.
You need only 6MB heap for the same chunksize and period 11000.
-}
sequenceLoop :: IO ()
sequenceLoop = do
   sequenceEvents $
      let evs =
             EventList.cons 11001
                [noteEvent Event.NoteOff 0 60 50,
                 noteEvent Event.NoteOn  0 60 50] evs
      in  chopEvents 100000 $
          EventListTM.takeTime (10^(7::Int)) $
          EventList.cons 0 [noteEvent Event.NoteOn 0 60 50] evs

sequenceStaccato :: IO ()
sequenceStaccato = do
   sequenceEvents $
      let evs =
             EventList.cons 551 [noteEvent Event.NoteOff 0 60 50] $
             EventList.cons 550 [noteEvent Event.NoteOn  0 60 50] evs
      in  chopEvents 100000 $
          EventListTM.takeTime (10^(7::Int)) $
          EventList.cons 0 [noteEvent Event.NoteOn 0 60 50] evs



sequenceControlledEvents :: [PAlsa.Events] -> IO ()
sequenceControlledEvents chunkedEvents = do
   opt <- Option.get
   arrange <- CausalSt.makeArranger
   amp <-
      CausalRender.run
         (Causal.map StereoInt.interleave <<<
          Causal.envelopeStereo <<<
          Causal.map Serial.upsample *** arr Stereo.unMultiValue)

   ping <- Instr.pingStereoReleaseFM

   let timeControlPercussive =
          controllerExponentialDim Ctrl.attackTime
             (DN.time 0.1, DN.time 2.5) (DN.time 0.8)
          &+&
          controllerExponentialDim Ctrl.releaseTime
             (DN.time 0.03, DN.time 0.3) (DN.time 0.1)

       frequencyControlPercussive =
          MCS.controllerLinear controllerDetune (0,0.005) 0.001
          &+&
          MCS.bendWheelPressure 2 0.04 0.03

       pingProc vel freq =
          ping sampleRate vel freq
          <<<
          Zip.arrowSecond
             (timeControlPercussive
              &+&
              ((MCS.controllerExponential controllerTimbre0 (0.3,10) 0.05
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

   let proc =
          arr SigStL.unpackStereoStrict
          <<<
          amp
          <<<
          (MCS.controllerExponential controllerVolume (0.001, 1) (0.2::Float)
           <<^ Zip.second)
          &+&
          (arrange
           <<<
           arr shortTime
           <<<
           MIO.sequenceModulated
              (Option.channel opt) (\ _pgm -> pingProc))
          <<<
          id &+& MCS.fromChannel (Option.channel opt)

   writeTest proc chunkedEvents


sequenceControlled :: IO ()
sequenceControlled =
   sequenceControlledEvents $
      let evs = EventList.cons 10 [] evs
      in  chopEvents 512 $
          EventListTM.takeTime (10^(7::Int)) $
          EventList.cons 0 [noteEvent Event.NoteOn 0 60 64] evs

sequenceControlledModulated :: IO ()
sequenceControlledModulated =
   sequenceControlledEvents $
      chopEvents 512 $
      EventListTM.takeTime (10^(7::Int)) $
      EventList.cons 0 [noteEvent Event.NoteOn 0 60 64] $
      EventList.fromPairList $
      map (\ev -> (10,[ev])) $ cycle $
      map (ctrlEvent 0 1) [0..127]


makeSampledSounds ::
   Option.T ->
   IO [SampleRate Real -> Real -> Real ->
       PIO.T
          (Zip.T MIO.GateChunk Instr.DetuneBendModControl)
          Instr.StereoChunk]
makeSampledSounds opt =
   liftA2 map Instr.sampledSound $
   Sample.loadRanges (Option.sampleDirectory opt) Sample.tomatensalat


sampledSound :: IO ()
sampledSound = do
   opt <- Option.get

   amp <-
      CausalRender.run
         (Causal.map StereoInt.interleave <<^ Stereo.unMultiValue)

   tomatoSmps <- makeSampledSounds opt

   let tomato smp vel freq =
          smp sampleRate vel freq
          <<<
          Zip.arrowSecond
             (MCS.controllerLinear controllerDetune (0,0.005) 0.001
              &+&
              MCS.bendWheelPressure 2 0.04 0.03)

   writeTest
      (arr SigStL.unpackStereoStrict
       <<<
       amp
       <<<
       tomato (last tomatoSmps) 0 440) $
      map
         (\m ->
            Zip.consChecked "Test.sampledSound"
               (Gate.chunk 512 m)
               (PCS.Cons Map.empty (EventListTT.pause 512))) $
      replicate 10 Nothing ++
      Just (100, VoiceMsg.normalVelocity) :
      replicate 4 Nothing

loadTomato :: Option.T -> IO (SVL.Vector Real)
loadTomato opt =
   case Sample.tomatensalat of
      Sample.Info name _sampleRate _positions ->
         Sample.load (Option.sampleDirectory opt </> name)

sampledSoundMono :: IO ()
sampledSoundMono = do
   opt <- Option.get

   case Sample.tomatensalat of
      Sample.Info _name rate positions -> do
         smp <- loadTomato opt
         case Sample.parts (Sample.Cons smp (DN.frequency rate) (last positions)) of
            (_attack, _sustain, release) ->
               SVL.writeFile "/tmp/release.f32" release

   tomatoSmps <-
      liftA2 map Instr.sampledSoundMono $
      Sample.loadRanges (Option.sampleDirectory opt) Sample.tomatensalat

   let tomato smp vel freq =
          smp sampleRate vel freq
          <<<
          Zip.arrowSecond (MCS.bendWheelPressure 2 0.04 0.03)

   writeTest (tomato (last tomatoSmps) 0 220) $
      map
         (\m ->
            Zip.consChecked "Test.sampledSound"
               (Gate.chunk 512 m)
               (PCS.Cons Map.empty (EventListTT.pause 512))) $
      replicate 10 Nothing ++
      Just (256, VoiceMsg.normalVelocity) :
      replicate 10 Nothing

{-
This one crashes sometimes in LLVM-3.0 when optimizations are enabled.
-}
sampledSoundCrash :: IO ()
sampledSoundCrash = do
   opt <- Option.get

   amp <-
      CausalRender.run
         (Causal.map StereoInt.interleave <<^ Stereo.unMultiValue)

   tomatoSmps <- makeSampledSounds opt

   let tomato smp vel freq =
          smp sampleRate vel freq
          <<<
          Zip.arrowSecond
             (MCS.controllerLinear controllerDetune (0,0.005) 0.001
              &+&
              MCS.bendWheelPressure 2 0.04 0.03)

   writeTest
      (arr SigStL.unpackStereoStrict
       <<<
       amp
       <<<
       tomato (head tomatoSmps) 0 440) $
      map
         (\m ->
            Zip.consChecked "Test.sampledSound"
               (Gate.chunk 512 m)
               (PCS.Cons Map.empty (EventListTT.pause 512))) $
      replicate 10 Nothing ++
      Just (100, VoiceMsg.normalVelocity) :
      replicate 10 Nothing


lfo :: SVL.Vector Real
lfo =
   Unsafe.performIO $
   fmap ($ SVL.chunkSize 512) $
   Render.run (1 + 0.1 * Sig.osci Wave.approxSine2 Expr.zero 0.0001)

asMono :: vector Real -> vector Real
asMono = id

frequencyModulation :: IO ()
frequencyModulation = do
   opt <- Option.get
   smp <- loadTomato opt

   SVL.writeFile "/tmp/test.f32" .
      asMono .
      (\f -> pioApply (f smp) lfo) =<<
      CausalRender.run Causal.frequencyModulationLinear


frequencyModulationIO :: IO ()
frequencyModulationIO = do
   opt <- Option.get
   smp <- loadTomato opt

   proc <- CausalRender.run Causal.frequencyModulationLinear

   writeTest (proc smp :: PIO.T (SV.Vector Real) (SV.Vector Real)) $
      SVL.chunks lfo

frequencyModulationStrictIO :: IO ()
frequencyModulationStrictIO = do
   opt <- Option.get
   smp <- loadTomato opt

   proc <- CausalRender.run Causal.frequencyModulationLinear

   writeTest
      (proc (SV.concat $ SVL.chunks smp) ::
         PIO.T (SV.Vector Real) (SV.Vector Real)) $
      SVL.chunks lfo

frequencyModulationSawIO :: IO ()
frequencyModulationSawIO = do
   proc <-
      CausalRender.run $ \freq ->
         Causal.frequencyModulationLinear
             (Causal.take 50000 $* Sig.osci Wave.saw 0 freq)

   writeTest (proc (0.01::Real) :: PIO.T (SV.Vector Real) (SV.Vector Real)) $
      SVL.chunks lfo

envelopeIO :: IO ()
envelopeIO = do
   opt <- Option.get
   smp <- loadTomato opt

   proc <- CausalRender.run $ \env -> Causal.envelope $< env

   writeTest (proc smp :: PIO.T (SV.Vector Real) (SV.Vector Real)) $
      SVL.chunks lfo


functional :: IO ()
functional = do
   phaser <-
      CausalRender.run $
      wrapped $ \(NoiseReference noiseRef) (SampleRate _sr) ->
      F.withArgs $ \ratio ->
         let noise = F.fromSignal $ Sig.noise 12 noiseRef
         in  (1-ratio) * noise +
             ratio * (Causal.delayZero 100 F.$& noise)

   writeTest
      (phaser sampleRate (200000 :: Real) ::
         PIO.T (EventListBT.T NonNegW.Int Float) (SV.Vector Float)) $
      map (\y -> EventListBT.singleton y 10000)
          [0, 0.25, 0.5, 0.75, 1.00]


functionalPlug :: IO ()
functionalPlug = do
   phaser <-
      FP.withArgs $ \ratio0 pl ->
      (\f ->
         case Expr.unzip pl of
            (sr,noiseRef) -> f (expSampleRate sr) noiseRef) $
      wrapped $ \(NoiseReference noiseRef) (SampleRate _sr) ->
         let ratio = FP.plug ratio0
             noise = FP.fromSignal $ Sig.noise 12 noiseRef
         in  (1-ratio) * noise +
             ratio * (Causal.delayZero 100 FP.$& noise)

   writeTest
      (phaser () (sampleRate, 200000) ::
         PIO.T (EventListBT.T NonNegW.Int Float) (SV.Vector Float)) $
      map (\y -> EventListBT.singleton y 10000)
          [0, 0.25, 0.5, 0.75, 1.00]


makeUnpackStereoStrict ::
   IO (PIO.T (SV.Vector (Stereo.T Vector)) (SV.Vector (Stereo.T Real)))
makeUnpackStereoStrict =
   fmap (SigStL.unpackStereoStrict ^<<) $
   CausalRender.run
      (Causal.map StereoInt.interleave <<^ Stereo.unMultiValue)
{-
makeUnpackStereoStrict ::
   IO (SV.Vector (Stereo.T Vector) -> SV.Vector (Stereo.T Real))
makeUnpackStereoStrict =
   SigStL.makeUnpackGenericStrict
-}

functionalTineControl ::
   Instr.WithEnvelopeControl
      (Zip.T
         (Zip.T (Instr.Control Real) (Instr.Control Real))
         Instr.DetuneBendModControl)
functionalTineControl =
   let cs :: Num a => a
       cs = 512
   in  Zip.Cons
          (Gate.chunk cs Nothing)
          (Zip.Cons
             (Zip.Cons
                (EventListBT.singleton (DN.time 1) cs)
                (EventListBT.singleton (DN.time 1) cs))
             (Zip.Cons
                (Zip.Cons
                   (EventListBT.singleton 2 cs)
                   (EventListBT.singleton 1 cs))
                (Zip.Cons
                   (EventListBT.singleton 0.001 cs)
                   (EventListBT.singleton (BM.Cons 1 0.01) cs))))

functionalTine :: IO ()
functionalTine = do
   ping <- Instr.tineStereoFM
   unpack <- makeUnpackStereoStrict
   writeTest (unpack <<< ping sampleRate 0 440) $
      replicate 100 functionalTineControl

functionalPlugTine :: IO ()
functionalPlugTine = do
   ping <- InstrFP.tineStereoFM
   unpack <- makeUnpackStereoStrict
   writeTest (unpack <<< ping sampleRate 0 440) $
      replicate 100 functionalTineControl


stringControl ::
   PC.ShortStrictTime ->
   Instr.WithEnvelopeControl
      (Zip.T (Instr.Control Real) Instr.DetuneBendModControl)
stringControl cs =
   Zip.Cons
      (Gate.chunk (PC.longFromShortTime cs) Nothing)
      (Zip.Cons
         (Zip.Cons
            (EventListBT.singleton (DN.time 0.5) cs)
            (EventListBT.singleton (DN.time 1) cs))
         (Zip.Cons
            (EventListBT.singleton 10 cs)
            (Zip.Cons
               (EventListBT.singleton 0.001 cs)
               (EventListBT.singleton (BM.Cons 1 0) cs))))

phonemeControl ::
   PC.ShortStrictTime ->
   (PC.ShortStrictTime -> ctrl) ->
   Instr.WithEnvelopeControl ctrl
phonemeControl cs ctrl =
   Zip.Cons
      (Gate.chunk (PC.longFromShortTime cs) Nothing)
      (Zip.Cons
         (Zip.Cons
            (EventListBT.singleton (DN.time 0.5) cs)
            (EventListBT.singleton (DN.time 0.02) cs))
         (ctrl cs))

speech :: IO ()
speech = do
   string <- Instr.softStringShapeFM
   unpack <- makeUnpackStereoStrict
   when False $
      writeTest (unpack <<< string sampleRate 0 440) $
         replicate 100 $ stringControl 512

   phoneme <- Speech.phonemeMask
   masks <- Speech.loadMasks Speech.maskNamesGrouped
   writeTest
      (unpack <<< phoneme masks sampleRate 0 (VoiceMsg.toPitch 64) <<<
       Zip.arrowSecond (Zip.arrowSecond (string sampleRate 0 440))) $
      replicate 100 $ phonemeControl 512 stringControl
