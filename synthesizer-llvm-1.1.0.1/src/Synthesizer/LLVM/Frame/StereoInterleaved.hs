module Synthesizer.LLVM.Frame.StereoInterleaved (
   T,
   Value,
   interleave,
   deinterleave,
   amplify,
   envelope,
   ) where

import qualified Synthesizer.LLVM.Frame.StereoInterleavedCode as StereoInt
import Synthesizer.LLVM.Frame.StereoInterleavedCode (T, Value)

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Vector as MultiVector

import qualified Type.Data.Num.Decimal as TypeNum


interleave ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Stereo.T (Exp (Serial.T n a)) -> Exp (T n a)
interleave = Expr.liftM StereoInt.interleave

deinterleave ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Exp (T n a) -> Stereo.T (Exp (Serial.T n a))
deinterleave x =
   Stereo.cons
      (Expr.liftM (fmap Stereo.left  . StereoInt.deinterleave) x)
      (Expr.liftM (fmap Stereo.right . StereoInt.deinterleave) x)

amplify ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   Exp a -> Exp (T n a) -> Exp (T n a)
amplify = Expr.liftM2 StereoInt.scale

envelope ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   Exp (Serial.T n a) -> Exp (T n a) -> Exp (T n a)
envelope = Expr.liftM2 StereoInt.envelope
