{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.LLVM.Server.CausalPacked.Speech (
   loadMasks,
   loadMasksGrouped,
   loadMasksKeyboard,
   maskNamesGrouped,
   phonemeMask,
   vowelMask,
   vowelBand,
   filterFormant,
   filterFormants,
   VowelSynth,
   VowelSynthEnv,
   EnvelopeType(..),
   CarrierType(..),
   PhonemeType(..),
   ) where

import Synthesizer.LLVM.Server.CausalPacked.Instrument
          (StereoChunk, Control, Frequency, frequencyControl,
           WithEnvelopeControl, zipEnvelope,
           stringControlledEnvelope, pingControlledEnvelope)
import Synthesizer.LLVM.Server.CommonPacked (Vector)
import Synthesizer.LLVM.Server.Common
          (SampleRate(SampleRate), Real, wrapped,
           Arg(Frequency), constant, noiseReference)
import qualified Synthesizer.LLVM.Server.SampledSound as Sample

import qualified Synthesizer.MIDI.CausalIO.Process as MIO
import qualified Synthesizer.CausalIO.Gate as Gate
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Filter.Universal as UniFilterL
import qualified Synthesizer.LLVM.Filter.NonRecursive as FiltNR
import qualified Synthesizer.LLVM.Causal.FunctionalPlug as FP
import qualified Synthesizer.LLVM.Causal.ControlledPacked as CtrlPS
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import Synthesizer.LLVM.Causal.FunctionalPlug (($&), (&|&))
import Synthesizer.LLVM.Causal.Process (($*), ($<), ($>))

import qualified Synthesizer.Zip as Zip
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified Synthesizer.PiecewiseConstant.Signal as PC

import qualified Synthesizer.Generic.Control as CtrlG
import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import Synthesizer.Plain.Filter.Recursive (Pole(Pole))

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import qualified Data.Map as Map ; import Data.Map (Map)

import qualified LLVM.Extra.Multi.Value as MultiValue

import qualified System.Path as Path
import System.Path ((</>), (<.>))

import Control.Arrow (arr, second, (^<<), (<<^), (***))
import Control.Category ((.))
import Control.Applicative (pure, liftA, liftA3, (<$>), (<*>))

import Data.Traversable (Traversable, traverse, forM)

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.))


{-
stimmhaft
a, e, i, o, u, ae, oe, ue
l, m, n, ng

Diphtong
ai, oi, au, ui, ei

stimmlos/Zischlaute
f, h, w, s, sch, th, ch (weich), ch (kochen), r

plosiv
b, p, g, k, d, t
-}

{-
Formanten:
a -  700 Hz
i -  400 Hz, 2200 Hz
o -  600 Hz, 3000 Hz
f -  white noise
sch - highpass cutoff 1500 Hz
-}

type
   VowelSynth =
      SampleRate Real -> VoiceMsg.Pitch ->
      PIO.T (Zip.T MIO.GateChunk StereoChunk) StereoChunk

{- |
Synthesize vowels using bandpass filters.
-}
vowelBand :: IO VowelSynth
vowelBand =
   liftA
      (\filt sr p ->
         case formants p of
            Nothing -> arr $ const SV.empty
            Just fs ->
               filt sr fs
               .
               Gate.shorten)
      (CausalRender.run $
       wrapped $ \(Frequency low, Frequency high) (SampleRate _sr) ->
         Stereo.multiValue
         ^<<
         Causal.stereoFromMono
             (let lowpass q f =
                     UniFilter.bandpass
                     ^<<
                     CtrlPS.process
                     $<
                     Sig.constant (UniFilter.parameter $ Pole q f)
              in  lowpass 100 low + lowpass 20 high)
         <<^
         Stereo.unMultiValue)

formants :: VoiceMsg.Pitch -> Maybe (Real, Real)
formants p =
   case VoiceMsg.fromPitch p of
      00 -> Just ( 320,  800) -- u
      02 -> Just ( 500, 1000) -- o
      04 -> Just (1000, 1400) -- a
      05 -> Just (1500,  500) -- oe
      07 -> Just (1650,  320) -- ue
      09 -> Just (1800,  700) -- ae
      11 -> Just (2300,  500) -- e
      12 -> Just (3200,  320) -- i
      _ -> Nothing


{- |
Synthesize vowels using sampled impulse responses.
-}
vowelMask ::
   IO (Map VoiceMsg.Pitch (SV.Vector Real) -> VowelSynth)
vowelMask =
   liftA
      (\filt dict _sr p ->
         case Map.lookup p dict of
            Nothing -> arr $ const SV.empty
            Just mask -> filt (Render.buffer mask) . Gate.shorten)
      (CausalRender.run $ \mask ->
         Stereo.multiValue
         ^<<
         Causal.stereoFromMono (FiltNR.convolvePacked mask)
         <<^
         Stereo.unMultiValue)


type
   VowelSynthEnv =
      SampleRate Real -> Real {- Velocity -} -> VoiceMsg.Pitch ->
      PIO.T (WithEnvelopeControl StereoChunk) StereoChunk

data EnvelopeType = Continuous | Percussive
   deriving (Eq, Ord, Show)

data CarrierType = Voiced | Unvoiced | Rasp
   deriving (Eq, Ord, Show)

data PhonemeType = Filtered EnvelopeType CarrierType | Sampled
   deriving (Eq, Ord, Show)

{- |
Like 'vowelMask', but it does not simply open and close the gate abruptly.
Instead we use an envelope for fading the filtered sound in and out.
-}
phonemeMask ::
   IO (Map VoiceMsg.Pitch (PhonemeType, SV.Vector Real) -> VowelSynthEnv)
phonemeMask =
   pure
      (\filt filtRasp filtNoise smp contEnv percEnv dict sr vel p ->
         case Map.lookup p dict of
            Nothing -> arr $ const SV.empty
            Just (typ, mask) ->
               let maskBuf = Render.buffer mask in
               case typ of
                  Filtered env carrier ->
                     (case carrier of
                        Voiced -> filt maskBuf
                        Unvoiced -> filtNoise sr maskBuf . arr Zip.first
                        Rasp ->
                           filtRasp maskBuf $
                              case sr of
                                 SampleRate r ->
                                    SVL.cycle $ SVL.take (round $ r/20) $
                                    CtrlG.exponential SigG.defaultLazySize
                                       (r/40) 1)
                     .
                     zipEnvelope
                        (case env of
                           Continuous -> contEnv sr vel
                           Percussive -> percEnv sr vel)
                  Sampled ->
                     smp (SVL.fromChunks $ repeat mask)
                     .
                     arr Zip.first
                     .
                     zipEnvelope (contEnv sr vel))
   <*> (CausalRender.run $ \mask ->
         Stereo.multiValue <$>
          Causal.envelopeStereo
          .
          second
             (Causal.stereoFromMono (FiltNR.convolvePacked mask)
                  <<^ Stereo.unMultiValue))
   <*> (CausalRender.run $ \mask env ->
         Stereo.multiValue <$>
          Causal.envelopeStereo
          .
          ((Causal.envelope $< SigPS.pack env)
           ***
           (Causal.stereoFromMono (FiltNR.convolvePacked mask)
               <<^ Stereo.unMultiValue)))
   <*> (CausalRender.run $
        constant noiseReference 1e7 $ \noiseRef _sr mask ->
         Stereo.multiValue <$>
         Causal.envelopeStereo $>
             traverse
                (\seed ->
                   FiltNR.convolvePacked mask $* SigPS.noise seed noiseRef)
                (Stereo.cons 42 23))
   <*> (CausalRender.run $ \smp ->
         (\x -> Stereo.consMultiValue x x)
         ^<<
         (Causal.envelope $> SigPS.pack smp))
   <*> stringControlledEnvelope
   <*> pingControlledEnvelope (Just 0.01)


phonemeRr,
   phonemeU,
   phonemeO,
   phonemeA,
   phonemeOe,
   phonemeOn,
   phonemeUe,
   phonemeUn,
   phonemeAe,
   phonemeE,
   phonemeI,

   phonemeNg,
   phonemeL,
   phonemeM,
   phonemeN,
   phonemeR,
   phonemeJ,

   phonemeW,
   phonemeF,
   phonemeSch,
   phonemeH,
   phonemeTh,
   phonemeIch,
   phonemeAch,
   phonemeS,

   phonemeP,
   phonemeK,
   phonemeT,

   phonemeB,
   phonemeG,
   phonemeD
      :: (PhonemeType, FilePath)
phonemeU   = (Filtered Continuous Voiced, "u")
phonemeO   = (Filtered Continuous Voiced, "o")
phonemeA   = (Filtered Continuous Voiced, "a")
phonemeOe  = (Filtered Continuous Voiced, "oe")
phonemeOn  = (Filtered Continuous Voiced, "on")
phonemeUe  = (Filtered Continuous Voiced, "ue")
phonemeUn  = (Filtered Continuous Voiced, "un")
phonemeAe  = (Filtered Continuous Voiced, "ae")
phonemeE   = (Filtered Continuous Voiced, "e")
phonemeI   = (Filtered Continuous Voiced, "i")

phonemeNg  = (Filtered Continuous Voiced, "ng")
phonemeL   = (Filtered Continuous Voiced, "l")
phonemeM   = (Filtered Continuous Voiced, "m")
phonemeN   = (Filtered Continuous Voiced, "n")
phonemeR   = (Filtered Continuous Voiced, "r")
phonemeJ   = (Filtered Continuous Voiced, "j")

phonemeW   = (Filtered Continuous Unvoiced, "w")
phonemeF   = (Filtered Continuous Unvoiced, "f")
phonemeSch = (Filtered Continuous Unvoiced, "sch")
phonemeH   = (Filtered Continuous Unvoiced, "h")
phonemeTh  = (Filtered Continuous Unvoiced, "th")
phonemeIch = (Filtered Continuous Unvoiced, "ich")
phonemeAch = (Filtered Continuous Unvoiced, "ach")
phonemeS   = (Filtered Continuous Unvoiced, "s")

phonemeP  = (Filtered Percussive Unvoiced, "p")
phonemeK  = (Filtered Percussive Unvoiced, "k")
phonemeT  = (Filtered Percussive Unvoiced, "t")

phonemeB  = (Filtered Percussive Voiced, "b")
phonemeG  = (Filtered Percussive Voiced, "g")
phonemeD  = (Filtered Percussive Voiced, "d")

-- phonemeRr = (Sampled, "r")) :
phonemeRr = (Filtered Continuous Rasp, "ng")


maskNamesKeyboard :: Map VoiceMsg.Pitch (PhonemeType, FilePath)
maskNamesKeyboard =
   Map.fromList $
   zip [VoiceMsg.toPitch 0 ..] $

   phonemeL :   phonemeNg :
   phonemeM :   phonemeJ :
   phonemeN :
   phonemeR :
                phonemeP :
   phonemeB :   phonemeK :
   phonemeG :   phonemeT :
   phonemeD :

   phonemeU :   phonemeUe :
   phonemeO :   phonemeOe :
   phonemeA :
   phonemeE :   phonemeAe :
   phonemeI :
                phonemeRr :

   phonemeW :   phonemeF :
   phonemeSch :
   phonemeH :   phonemeTh :
   phonemeIch : phonemeAch :
   phonemeS :
   []

loadMasksKeyboard :: IO (Map VoiceMsg.Pitch (PhonemeType, SV.Vector Real))
loadMasksKeyboard =
   fmap (Map.insert (VoiceMsg.toPitch 29)
           (Filtered Continuous Voiced, SV.singleton 1)) $
   loadMasks maskNamesKeyboard


maskNamesGrouped :: Map VoiceMsg.Pitch (PhonemeType, FilePath)
maskNamesGrouped =
   Map.fromList $

   (zip [VoiceMsg.toPitch 0 ..] $
      phonemeU :
      phonemeO :
      phonemeA :
      phonemeOe :
      phonemeUe :
      phonemeAe :
      phonemeE :
      phonemeI :
      phonemeOn :
      phonemeUn :
      [])
   ++
   (zip [VoiceMsg.toPitch 16 ..] $
      phonemeJ :
      phonemeL :
      phonemeM :
      phonemeN :
      phonemeNg :
      phonemeR :
      [])
   ++
   (zip [VoiceMsg.toPitch 32 ..] $
      phonemeW :
      phonemeF :
      phonemeSch :
      phonemeH :
      phonemeTh :
      phonemeIch :
      phonemeAch :
      phonemeS :
      [])
   ++
   (zip [VoiceMsg.toPitch 48 ..] $
      phonemeRr :
      [])
   ++
   (zip [VoiceMsg.toPitch 64 ..] $
      phonemeP :
      phonemeK :
      phonemeT :
      [])
   ++
   (zip [VoiceMsg.toPitch 80 ..] $
      phonemeB :
      phonemeG :
      phonemeD :
      [])

loadMasksGrouped :: IO (Map VoiceMsg.Pitch (PhonemeType, SV.Vector Real))
loadMasksGrouped =
   fmap (Map.insert (VoiceMsg.toPitch 127)
           (Filtered Continuous Voiced, SV.singleton 8)) $
   loadMasks maskNamesGrouped


loadMasks ::
   (Traversable dict) =>
   dict (PhonemeType, FilePath) ->
   IO (dict (PhonemeType, SV.Vector Real))
loadMasks maskNames =
   forM maskNames $ \(typ, name) ->
      (,) typ . SV.concat . SVL.chunks <$>
      Sample.load
         (Path.relDir (if typ==Sampled then "phoneme" else "mask")
            </> Path.relFile name <.> "wav")



type Input a = FP.Input (SampleRate Real) a

plugUniFilterParameter ::
   Input a (Control Real) ->
   Input a (Control Frequency) ->
   FP.T (SampleRate Real) a (UniFilter.Parameter (MultiValue.T Real))
plugUniFilterParameter reson freq =
   fmap UniFilterL.unMultiValueParameter $
   FP.plug $
   liftA3
      (\resonChunk freqChunk sr ->
         PC.zipWith
            (\ r f -> UniFilter.parameter $ Pole r f)
            resonChunk $ frequencyControl sr freqChunk)
      reson freq FP.askParameter


type FormantControl =
        Zip.T (Control Real)
           (Zip.T (Control Real) (Control Frequency))

singleFormant ::
   (Input inp (Control Real),
      (Input inp (Control Real), Input inp (Control Frequency))) ->
   Input inp StereoChunk ->
   FP.T (SampleRate Real) inp (MultiValue.T (Stereo.T Vector))
singleFormant (amp, (reson, freq)) x =
   Stereo.multiValue <$>
   Causal.envelopeStereo $&
      (Causal.map Serial.upsample $& FP.plug amp)
      &|&
      (Causal.stereoFromMonoControlled
           (UniFilter.bandpass ^<< CtrlPS.process) $&
         plugUniFilterParameter reson freq
         &|&
         (Stereo.unMultiValue <$> FP.plug x))

filterFormant ::
   IO (SampleRate Real ->
       PIO.T
          (Zip.T FormantControl StereoChunk)
          StereoChunk)
filterFormant =
   liftA
      (\filt sr -> filt sr ())
      (FP.withArgs $ \(fmt, x) _unit -> singleFormant fmt x)

filterFormants ::
   IO (SampleRate Real ->
       PIO.T (Zip.T
                 (Zip.T FormantControl
                     (Zip.T FormantControl
                         (Zip.T FormantControl
                             (Zip.T FormantControl FormantControl))))
                 StereoChunk)
             StereoChunk)
filterFormants =
   liftA
      (\filt sr -> filt sr ())
      (FP.withArgs $ \((fmt0, (fmt1, (fmt2, (fmt3, fmt4)))), x) _unit ->
         foldl1 (+) $ map (flip singleFormant x) [fmt0, fmt1, fmt2, fmt3, fmt4])
