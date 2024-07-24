{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
This module provides a type class that automatically selects a filter
for a given parameter type.
We choose the dependency this way
because there may be different ways to specify the filter parameters
but there is only one implementation of the filter itself.
-}
module Synthesizer.LLVM.Causal.Controlled (
   C(..),
   processCtrlRate,
   ) where

import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked
                                                           as ComplexFiltPack
import qualified Synthesizer.LLVM.Filter.ComplexFirstOrder as ComplexFilt
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Filter.SecondOrderCascade as Cascade
import qualified Synthesizer.LLVM.Filter.SecondOrderPacked as Filt2P
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter

import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import Synthesizer.Causal.Class (($<))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal.Number ((:*:))

import qualified Algebra.Module as Module



processCtrlRate ::
   (C parameter a b, Memory.C parameter) =>
   (Marshal.C r, MultiValue.IntegerConstant r,
    MultiValue.Additive r, MultiValue.Comparison r) =>
   Exp r -> (Exp r -> Sig.T parameter) -> Causal.T a b
processCtrlRate reduct ctrlGen =
   process $< Sig.interpolateConstant reduct (ctrlGen reduct)


{- |
A filter parameter type uniquely selects a filter function.
However it does not uniquely determine the input and output type,
since the same filter can run on mono and stereo signals.
-}
class (a ~ Input parameter b, b ~ Output parameter a) => C parameter a b where
   type Input  parameter b
   type Output parameter a
   process :: Causal.T (parameter, a) b


{-
Instances for the particular filters shall be defined here
in order to avoid orphan instances.
-}

instance
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v,
    Memory.C a, Memory.C v) =>
      C (Filt1.Parameter a) v (Filt1.Result v) where
   type Input  (Filt1.Parameter a) (Filt1.Result v) = v
   type Output (Filt1.Parameter a) v = Filt1.Result v
   process = Filt1.causal

instance
   (a ~ A.Scalar v, A.PseudoModule v, A.RationalConstant a,
    Memory.C a, Memory.C v) =>
      C (Filt2.Parameter a) v v where
   type Input  (Filt2.Parameter a) v = v
   type Output (Filt2.Parameter a) v = v
   process = Filt2.causal

instance
   (Marshal.C a, Marshal.Vector TypeNum.D4 a, MultiVector.PseudoRing a) =>
      C (Filt2P.Parameter a) (MultiValue.T a) (MultiValue.T a) where
   type Input  (Filt2P.Parameter a) (MultiValue.T a) = MultiValue.T a
   type Output (Filt2P.Parameter a) (MultiValue.T a) = MultiValue.T a
   process = Filt2P.causal

instance
   (a ~ MultiValue.Scalar v, MultiValue.PseudoModule v,
    Marshal.C a, MultiValue.IntegerConstant a, Marshal.C v,
    TypeNum.Natural n, TypeNum.Positive (n :*: LLVM.UnknownSize),
    inp ~ MultiValue.T v, out ~ MultiValue.T v) =>
      C (Cascade.ParameterValue n a) inp out where
   type Input  (Cascade.ParameterValue n a) out = out
   type Output (Cascade.ParameterValue n a) inp = inp
   process = Cascade.causal


instance
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v,
    Memory.C a, Memory.C v) =>
      C (Allpass.Parameter a) v v where
   type Input  (Allpass.Parameter a) v = v
   type Output (Allpass.Parameter a) v = v
   process = Allpass.causal

instance
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v,
    Memory.C a, Memory.C v, TypeNum.Natural n) =>
      C (Allpass.CascadeParameter n a) v v where
   type Input  (Allpass.CascadeParameter n a) v = v
   type Output (Allpass.CascadeParameter n a) v = v
   process = Allpass.cascade


instance
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v,
    Memory.C v, TypeNum.Natural n) =>
      C (Moog.Parameter n a) v v where
   type Input  (Moog.Parameter n a) v = v
   type Output (Moog.Parameter n a) v = v
   process = Moog.causal


instance
   (A.PseudoModule v, A.Scalar v ~ a, A.RationalConstant a,
    Memory.C a, Memory.C v) =>
      C (UniFilter.Parameter a) v (UniFilter.Result v) where
   type Input  (UniFilter.Parameter a) (UniFilter.Result v) = v
   type Output (UniFilter.Parameter a) v = UniFilter.Result v
   process = UniFilter.causal

instance
   (A.PseudoRing a, A.RationalConstant a, Memory.C a) =>
      C (ComplexFilt.Parameter a) (Stereo.T a) (Stereo.T a) where
   type Input  (ComplexFilt.Parameter a) (Stereo.T a) = Stereo.T a
   type Output (ComplexFilt.Parameter a) (Stereo.T a) = Stereo.T a
   process = ComplexFilt.causal

instance
   (Marshal.Vector n a, n ~ TypeNum.D3, MultiVector.PseudoRing a,
    inp ~ MultiValue.T a, out ~ MultiValue.T a) =>
      C (ComplexFiltPack.ParameterMV a) (Stereo.T inp) (Stereo.T out) where
   type Input  (ComplexFiltPack.ParameterMV a) (Stereo.T out) = Stereo.T out
   type Output (ComplexFiltPack.ParameterMV a) (Stereo.T inp) = Stereo.T inp
   process = ComplexFiltPack.causal
