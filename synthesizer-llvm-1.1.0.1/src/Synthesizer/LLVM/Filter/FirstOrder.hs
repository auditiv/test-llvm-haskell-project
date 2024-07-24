{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Synthesizer.LLVM.Filter.FirstOrder (
   Result(Result,lowpass_,highpass_), Parameter, FirstOrder.parameter,
   causal, lowpassCausal, highpassCausal,
   causalInit, lowpassCausalInit, highpassCausalInit,
   causalPacked, lowpassCausalPacked, highpassCausalPacked,
   causalInitPacked, lowpassCausalInitPacked, highpassCausalInitPacked,
   causalRecursivePacked, -- for Allpass
   ) where

import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as FirstOrder
import qualified Synthesizer.Plain.Modifier as Modifier
import Synthesizer.Plain.Filter.Recursive.FirstOrder
          (Parameter(Parameter), Result(Result,lowpass_,highpass_))

import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as SerialCode

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import Control.Arrow (arr, (&&&), (<<<))
import Control.Monad (foldM)
import Control.Applicative (liftA2)

import qualified Algebra.Module as Module

import NumericPrelude.Numeric
import NumericPrelude.Base


instance (Tuple.Phi a) => Tuple.Phi (Parameter a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Parameter a) where
   undef = Tuple.undefPointed

instance (Memory.C a) => Memory.C (Parameter a) where
   type Struct (Parameter a) = Memory.Struct a
   load = Memory.loadNewtype Parameter
   store = Memory.storeNewtype (\(Parameter k) -> k)
   decompose = Memory.decomposeNewtype Parameter
   compose = Memory.composeNewtype (\(Parameter k) -> k)


instance
   (Expr.Aggregate e mv) =>
      Expr.Aggregate (Parameter e) (Parameter mv) where
   type MultiValuesOf (Parameter e) = Parameter (Expr.MultiValuesOf e)
   type ExpressionsOf (Parameter mv) = Parameter (Expr.ExpressionsOf mv)
   bundle (Parameter p) = fmap Parameter $ Expr.bundle p
   dissect (Parameter p) = Parameter $ Expr.dissect p


instance (Expr.Aggregate e mv) => Expr.Aggregate (Result e) (Result mv) where
   type MultiValuesOf (Result e) = Result (Expr.MultiValuesOf e)
   type ExpressionsOf (Result mv) = Result (Expr.ExpressionsOf mv)
   bundle (Result f k) = liftA2 Result (Expr.bundle f) (Expr.bundle k)
   dissect (Result f k) = Result (Expr.dissect f) (Expr.dissect k)

causal ::
   (Expr.Aggregate ae a, Module.C ae ve,
    Expr.Aggregate ve v, Memory.C v) =>
   Causal.T (Parameter a, v) (Result v)
causal = Causal.fromModifier FirstOrder.modifier

lowpassCausal, highpassCausal ::
   (Expr.Aggregate ae a, Module.C ae ve,
    Expr.Aggregate ve v, Memory.C v) =>
   Causal.T (Parameter a, v) v
lowpassCausal  = Causal.fromModifier FirstOrder.lowpassModifier
highpassCausal = Causal.fromModifier FirstOrder.highpassModifier


causalInit ::
   (Expr.Aggregate ae a, Memory.C a, Module.C ae ve,
    Expr.Aggregate ve v, Memory.C v) =>
   ve -> Causal.T (Parameter a, v) (Result v)
causalInit =
   Causal.fromModifier . Modifier.initialize FirstOrder.modifierInit

lowpassCausalInit, highpassCausalInit ::
   (Expr.Aggregate ae a, Memory.C a, Module.C ae ve,
    Expr.Aggregate ve v, Memory.C v) =>
   ve -> Causal.T (Parameter a, v) v
lowpassCausalInit =
   Causal.fromModifier . Modifier.initialize FirstOrder.lowpassModifierInit
highpassCausalInit =
   Causal.fromModifier . Modifier.initialize FirstOrder.highpassModifierInit


lowpassCausalPacked, highpassCausalPacked, causalRecursivePacked,
      preampPacked ::
   (SerialCode.Write v, SerialCode.Element v ~ a,
    A.PseudoRing v, A.IntegerConstant v,
    A.PseudoRing a, A.IntegerConstant a, Memory.C a) =>
   Causal.T (Parameter a, v) v
highpassCausalPacked =
   CausalPriv.zipWith A.sub <<< arr snd &&& lowpassCausalPacked
lowpassCausalPacked =
   causalRecursivePacked <<< arr fst &&& preampPacked

causalRecursivePacked =
   CausalPriv.mapAccum causalRecursivePackedStep (return A.zero)

preampPacked =
   CausalPriv.map
      (\(Parameter k, x) -> A.mul x =<< SerialCode.upsample =<< A.sub A.one k)



{-
x = [x0, x1, x2, x3]

filter k y1 x
  = [x0 + k*y1,
     x1 + k*x0 + k^2*y1,
     x2 + k*x1 + k^2*x0 + k^3*y1,
     x3 + k*x2 + k^2*x1 + k^3*x0 + k^4*y1,
     ... ]

f0x = insert 0 (k*y1) x
f1x = f0x + k * f0x->1
f2x = f1x + k^2 * f1x->2
-}
causalRecursivePackedStep ::
   (SerialCode.Write v, SerialCode.Element v ~ a,
    A.PseudoRing v, A.IntegerConstant v, A.PseudoRing a) =>
   (Parameter a, v) -> a -> LLVM.CodeGenFunction r (v,a)
causalRecursivePackedStep (Parameter k, xk0) y1 = do
   y1k <- A.mul k y1
   xk1 <- SerialCode.modify A.zero (A.add y1k) xk0
   kv <- SerialCode.upsample k
   xk2 <-
      fmap fst $
      foldM
         (\(y,k0) d ->
            liftA2 (,)
               (A.add y =<< SerialCode.shiftUpMultiZero d =<< A.mul y k0)
               (A.mul k0 k0))
         (xk1,kv)
         (takeWhile (< SerialCode.size xk0) $ iterate (2*) 1)
   y0 <- SerialCode.last xk2
   return (xk2, y0)

{-
We can also optimize filtering with time-varying filter parameter.

k = [k0, k1, k2, k3]
x = [x0, x1, x2, x3]

filter k y1 x
  = [x0 + k0*y1,
     x1 + k1*x0 + k1*k0*y1,
     x2 + k2*x1 + k2*k1*x0 + k2*k1*k0*y1,
     x3 + k3*x2 + k3*k2*x1 + k3*k2*k1*x0 + k3*k2*k1*k0*y1,
     ... ]

f0x = insert 0 (k0*y1) x
f1x = f0x + k  * f0x->1      k'  = k * k->1
f2x = f1x + k' * f1x->2


We can even interpret vectorised first order filtering
as first order filtering with matrix coefficients.

[x0 + k0*y1,
 x1 + k1*x0 + k1*k0*y1,
 x2 + k2*x1 + k2*k1*x0 + k2*k1*k0*y1,
 x3 + k3*x2 + k3*k2*x1 + k3*k2*k1*x0 + k3*k2*k1*k0*y1]
  =
  / 1                   \   /x0\    / k0          0 0 0 \   /y1\
  | k1       1          | . |x1| +  | k1*k0       0 0 0 | . |y2|
  | k2*k1    k2    1    |   |x2|    | k2*k1*k0    0 0 0 |   |y3|
  \ k3*k2*k1 k3*k2 k3 1 /   \x3/    \ k3*k2*k1*k0 0 0 0 /   \y4/


  / 1                   \   / 1                 \   / 1          \
  | k1       1          | = |         1         | . | k1  1      |
  | k2*k1    k2    1    |   | k2*k1        1    |   |    k2  1   |
  \ k3*k2*k1 k3*k2 k3 1 /   \       k3*k2     1 /   \       k3 1 /
-}


addHighpass ::
   (A.Additive v) =>
   Causal.T (param,v) v -> Causal.T (param,v) (Result v)
addHighpass lowpass =
   CausalPriv.map
      (\(l,x) -> do
         h <- A.sub x l
         return (Result{lowpass_ = l, highpass_ = h}))
   <<<
   lowpass &&& arr snd

causalPacked ::
   (SerialCode.Write v, SerialCode.Element v ~ a,
    A.PseudoRing v, A.IntegerConstant v,
    A.PseudoRing a, A.IntegerConstant a, Memory.C a) =>
   Causal.T (Parameter a, v) (Result v)
causalPacked = addHighpass lowpassCausalPacked


lowpassCausalInitPacked, highpassCausalInitPacked,
      causalRecursiveInitPacked ::
   (A.PseudoRing v, A.IntegerConstant v,
    SerialCode.Write v, SerialCode.Element v ~ a,
    Expr.Aggregate ae a, A.PseudoRing a, A.IntegerConstant a, Memory.C a) =>
   ae -> Causal.T (Parameter a, v) v
causalRecursiveInitPacked a =
   CausalPriv.mapAccum causalRecursivePackedStep (Expr.bundle a)

highpassCausalInitPacked a = arr snd - lowpassCausalInitPacked a
lowpassCausalInitPacked a =
   causalRecursiveInitPacked a <<< arr fst &&& preampPacked

causalInitPacked ::
   (A.PseudoRing v, A.IntegerConstant v,
    SerialCode.Write v, SerialCode.Element v ~ a,
    Expr.Aggregate ae a, A.PseudoRing a, A.IntegerConstant a, Memory.C a) =>
   ae -> Causal.T (Parameter a, v) (Result v)
causalInitPacked a = addHighpass (lowpassCausalInitPacked a)
