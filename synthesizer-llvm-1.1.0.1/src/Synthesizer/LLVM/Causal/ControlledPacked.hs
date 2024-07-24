{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
This is like "Synthesizer.LLVM.CausalExp.Controlled"
but for vectorised signals.
-}
module Synthesizer.LLVM.Causal.ControlledPacked (
   C(..),
   processCtrlRate,
   ) where

import qualified Synthesizer.LLVM.Filter.SecondOrderCascade as Cascade
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter

import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalP
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as Serial

import Synthesizer.Causal.Class (($<))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal.Number ((:*:))

import qualified Algebra.Module as Module
import qualified NumericPrelude.Numeric as NP

import Control.Arrow ((<<<), arr, first)



processCtrlRate ::
   (C parameter av bv, Memory.C parameter,
    Serial.Read  av, n ~ Serial.Size av,
    Serial.Write bv, n ~ Serial.Size bv) =>
   (Marshal.C r, MultiValue.RationalConstant r,
    MultiValue.Field r, MultiValue.Comparison r) =>
   Exp r -> (Exp r -> Sig.T parameter) -> Causal.T av bv
processCtrlRate reduct ctrlGen = Serial.withSize $ \n ->
   process $<
      Sig.interpolateConstant (reduct / NP.fromIntegral n) (ctrlGen reduct)


{- |
A filter parameter type uniquely selects a filter function.
However it does not uniquely determine the input and output type,
since the same filter can run on mono and stereo signals.
-}
class (Output parameter a ~ b, Input parameter b ~ a) => C parameter a b where
   type Output parameter a
   type Input  parameter b
   process :: Causal.T (parameter, a) b


{-
Instances for the particular filters shall be defined here
in order to avoid orphan instances.
-}

instance
   (Serial.Write v, Serial.Element v ~ a,
    A.PseudoRing v, A.IntegerConstant v,
    A.PseudoRing a, A.IntegerConstant a, Expr.Aggregate ae a,
    Tuple.Phi a, Tuple.Undefined a, Memory.C a) =>
      C (Filt1.Parameter a) v (Filt1.Result v) where
   type Input  (Filt1.Parameter a) (Filt1.Result v) = v
   type Output (Filt1.Parameter a) v = Filt1.Result v
   process = Filt1.causalPacked

instance
   (Serial.Write v, Serial.Element v ~ a,
    A.PseudoRing v, A.IntegerConstant v,
    A.PseudoRing a, A.IntegerConstant a, Expr.Aggregate ae a,
    Tuple.Phi a, Tuple.Undefined a, Memory.C a, Memory.C v) =>
      C (Filt2.Parameter a) v v where
   type Input  (Filt2.Parameter a) v = v
   type Output (Filt2.Parameter a) v = v
   process = Filt2.causalPacked

instance
   (Serial.Write v, Serial.Element v ~ MultiValue.T a,
    Memory.C v, A.PseudoRing v, A.IntegerConstant v,
    Marshal.C a, MultiValue.PseudoRing a, MultiValue.IntegerConstant a,
    TypeNum.Positive (n :*: LLVM.UnknownSize),
    TypeNum.Natural n) =>
      C (Cascade.ParameterValue n a) v v where
   type Input  (Cascade.ParameterValue n a) v = v
   type Output (Cascade.ParameterValue n a) v = v
   process = Cascade.causalPacked

instance
   (Serial.Write v, Serial.Element v ~ a,
    A.PseudoRing a, A.IntegerConstant a, Memory.C a,
    A.PseudoRing v, A.IntegerConstant v) =>
      C (Allpass.Parameter a) v v where
   type Input  (Allpass.Parameter a) v = v
   type Output (Allpass.Parameter a) v = v
   process = Allpass.causalPacked

instance
   (TypeNum.Natural n,
    Serial.Write v, Serial.Element v ~ a,
    A.PseudoRing a, A.IntegerConstant a, Memory.C a,
    A.PseudoRing v, A.RationalConstant v) =>
      C (Allpass.CascadeParameter n a) v v where
   type Input  (Allpass.CascadeParameter n a) v = v
   type Output (Allpass.CascadeParameter n a) v = v
   process = Allpass.cascadePacked


instance
   (TypeNum.Natural n,
    Serial.Write v, Serial.Element v ~ b, Memory.C b,
    Tuple.Phi a, Tuple.Undefined a,
    Expr.Aggregate ae a, Expr.Aggregate be b, Module.C ae be) =>
      C (Moog.Parameter n a) v v where
   type Input  (Moog.Parameter n a) v = v
   type Output (Moog.Parameter n a) v = v
   process = CausalP.pack Moog.causal <<< first (arr Serial.constant)


instance
   (Serial.Write v, Serial.Element v ~ b, Memory.C b,
    Tuple.Phi a, Tuple.Undefined a,
    Expr.Aggregate ae a, Expr.Aggregate be b, Module.C ae be) =>
      C (UniFilter.Parameter a) v (UniFilter.Result v) where
   type Input  (UniFilter.Parameter a) (UniFilter.Result v) = v
   type Output (UniFilter.Parameter a) v = UniFilter.Result v
   process =
      CausalP.pack UniFilter.causalExp <<< first (arr Serial.constant)
