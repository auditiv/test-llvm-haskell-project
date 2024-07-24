{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{- |
Convert MIDI events of a MIDI controller to a control signal.
-}
module Synthesizer.LLVM.MIDI (
   frequencyFromBendModulation,
   frequencyFromBendModulationPacked,
   Gen.applyModulation,
   ) where

import qualified Synthesizer.MIDI.Generic as Gen
import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.Frame.SerialVector as SerialExp
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial

import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Wave as Wave
import Synthesizer.LLVM.Causal.Process (($>))

import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Core as LLVM

import Control.Arrow (second, (<<<), (<<^))

import NumericPrelude.Numeric
import Prelude (($))


frequencyFromBendModulation ::
   (Marshal.C y, MultiValue.T y ~ ym,
    MultiValue.PseudoRing y, MultiValue.IntegerConstant y,
    MultiValue.Fraction y) =>
   Exp y -> Causal.T (BM.T ym) ym
frequencyFromBendModulation speed =
   frequencyFromPair Sig.osci speed
   <<^
   (\(BM.Cons b m) -> (b,m))


frequencyFromBendModulationPacked ::
   (Marshal.Vector n a) =>
   (MultiVector.PseudoRing a, MultiVector.IntegerConstant a) =>
   (MultiVector.Fraction a) =>
   Exp a -> Causal.T (BM.T (MultiValue.T a)) (Serial.Value n a)
frequencyFromBendModulationPacked speed =
   frequencyFromPair SigPS.osci speed
   <<<
   Causal.map (\(BM.Cons b m) -> (SerialExp.upsample b, SerialExp.upsample m))

frequencyFromPair, _frequencyFromPair ::
   (MultiValue.Additive y,
    A.PseudoRing ym, A.IntegerConstant ym, A.Fraction ym) =>
   ((forall r. ym -> LLVM.CodeGenFunction r ym) ->
    Exp y -> Exp y -> Sig.T ym) ->
   Exp y -> Causal.T (ym,ym) ym
frequencyFromPair osci speed =
   Func.withGuidedArgs (Func.atom, Func.atom) $ \(b, m) ->
      b * (1 + m * Func.fromSignal (osci Wave.approxSine2 zero speed))

_frequencyFromPair osci speed =
   Causal.envelope
   <<<
   second (1 + (Causal.envelope $> osci Wave.approxSine2 zero speed))
