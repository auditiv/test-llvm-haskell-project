{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Synthesizer.LLVM.Filter.SecondOrder (
   Parameter(Parameter),
   Filt2.c0, Filt2.c1, Filt2.c2, Filt2.d1, Filt2.d2,
   bandpassParameter,
   bandpassParameterCode,
   ParameterStruct, composeParameter, decomposeParameter, -- for cascade
   composeParameterMV, decomposeParameterMV,
   causalExp,
   causal, causalPacked,
   ) where

import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2
import Synthesizer.Plain.Filter.Recursive.SecondOrder (Parameter(Parameter))

import qualified Synthesizer.Plain.Modifier as Modifier

import qualified Synthesizer.LLVM.Causal.Process as CausalExp
import qualified Synthesizer.LLVM.Causal.ProcessValue as Causal
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as Serial
import qualified Synthesizer.LLVM.Value as Value

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value.Marshal as MarshalMV
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction, valueOf)

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (d0, d1, d2, d3, d4)

import qualified Control.Monad.HT as M
import qualified Control.Applicative.HT as App
import Control.Arrow (arr, (<<<), (&&&))
import Control.Monad (liftM2, foldM)
import Control.Applicative (pure, liftA2, (<$>), (<*>))

import qualified Data.Foldable as Fold
import Data.Traversable (traverse)

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module as Module

import NumericPrelude.Numeric
import NumericPrelude.Base


instance (Tuple.Phi a) => Tuple.Phi (Parameter a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Parameter a) where
   undef = Tuple.undefPointed

instance (Tuple.Value a) => Tuple.Value (Parameter a) where
   type ValueOf (Parameter a) = Parameter (Tuple.ValueOf a)
   valueOf = Tuple.valueOfFunctor


type ParameterStruct a = LLVM.Struct (a, (a, (a, (a, (a, ())))))

parameterMemory ::
   (Memory.C a) =>
   Memory.Record r (ParameterStruct (Memory.Struct a)) (Parameter a)
parameterMemory =
   App.lift5 Parameter
      (Memory.element Filt2.c0 d0)
      (Memory.element Filt2.c1 d1)
      (Memory.element Filt2.c2 d2)
      (Memory.element Filt2.d1 d3)
      (Memory.element Filt2.d2 d4)

decomposeParameter ::
   LLVM.Value (ParameterStruct a) ->
   CodeGenFunction r (Filt2.Parameter (LLVM.Value a))
decomposeParameter param =
   pure Filt2.Parameter
      <*> LLVM.extractvalue param TypeNum.d0
      <*> LLVM.extractvalue param TypeNum.d1
      <*> LLVM.extractvalue param TypeNum.d2
      <*> LLVM.extractvalue param TypeNum.d3
      <*> LLVM.extractvalue param TypeNum.d4

decomposeParameterMV ::
   (MarshalMV.C a) =>
   LLVM.Value (MarshalMV.Struct (Parameter a)) ->
   CodeGenFunction r (Filt2.Parameter (MultiValue.T a))
decomposeParameterMV param =
   pure Filt2.Parameter
      <*> (Memory.decompose =<< LLVM.extractvalue param TypeNum.d0)
      <*> (Memory.decompose =<< LLVM.extractvalue param TypeNum.d1)
      <*> (Memory.decompose =<< LLVM.extractvalue param TypeNum.d2)
      <*> (Memory.decompose =<< LLVM.extractvalue param TypeNum.d3)
      <*> (Memory.decompose =<< LLVM.extractvalue param TypeNum.d4)

composeParameter ::
   (LLVM.IsSized a) =>
   Filt2.Parameter (LLVM.Value a) ->
   CodeGenFunction r (LLVM.Value (ParameterStruct a))
composeParameter (Filt2.Parameter c0_ c1_ c2_ d1_ d2_) =
   (\param -> LLVM.insertvalue param c0_ TypeNum.d0) =<<
   (\param -> LLVM.insertvalue param c1_ TypeNum.d1) =<<
   (\param -> LLVM.insertvalue param c2_ TypeNum.d2) =<<
   (\param -> LLVM.insertvalue param d1_ TypeNum.d3) =<<
   (\param -> LLVM.insertvalue param d2_ TypeNum.d4) =<<
   return (LLVM.value LLVM.undef)

composeParameterMV ::
   (MarshalMV.C a) =>
   Filt2.Parameter (MultiValue.T a) ->
   CodeGenFunction r (LLVM.Value (MarshalMV.Struct (Parameter a)))
composeParameterMV (Filt2.Parameter c0_ c1_ c2_ d1_ d2_) =
   let insert field ix param =
         Memory.compose field >>= flip (LLVM.insertvalue param) ix in
   insert c0_ TypeNum.d0 =<<
   insert c1_ TypeNum.d1 =<<
   insert c2_ TypeNum.d2 =<<
   insert d1_ TypeNum.d3 =<<
   insert d2_ TypeNum.d4 =<<
   return (LLVM.value LLVM.undef)

instance (Memory.C a) => Memory.C (Parameter a) where
   type Struct (Parameter a) = ParameterStruct (Memory.Struct a)
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory

instance (Marshal.C a) => Marshal.C (Parameter a) where
   pack p =
      case Marshal.pack <$> p of
         Filt2.Parameter c0_ c1_ c2_ d1_ d2_ ->
            LLVM.consStruct c0_ c1_ c2_ d1_ d2_
   unpack = fmap Marshal.unpack . LLVM.uncurryStruct Filt2.Parameter

instance (Storable.C a) => Storable.C (Parameter a) where
   load = Storable.loadApplicative
   store = Storable.storeFoldable


instance (Value.Flatten a) => Value.Flatten (Parameter a) where
   type Registers (Parameter a) = Parameter (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable

instance (MultiValue.C a) => MultiValue.C (Parameter a) where
   type Repr (Parameter a) = Parameter (MultiValue.Repr a)
   cons = parameterMultiValue . fmap MultiValue.cons
   undef = parameterMultiValue $ pure MultiValue.undef
   zero = parameterMultiValue $ pure MultiValue.zero
   phi bb =
      fmap parameterMultiValue .
      traverse (MultiValue.phi bb) .
      parameterUnMultiValue
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb)
         (parameterUnMultiValue a) (parameterUnMultiValue b)

instance (MarshalMV.C a) => MarshalMV.C (Parameter a) where
   pack p =
      case MarshalMV.pack <$> p of
         Filt2.Parameter c0_ c1_ c2_ d1_ d2_ ->
            LLVM.consStruct c0_ c1_ c2_ d1_ d2_
   unpack = fmap MarshalMV.unpack . LLVM.uncurryStruct Filt2.Parameter

parameterMultiValue ::
   Parameter (MultiValue.T a) -> MultiValue.T (Parameter a)
parameterMultiValue =
   MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

parameterUnMultiValue ::
   MultiValue.T (Parameter a) -> Parameter (MultiValue.T a)
parameterUnMultiValue (MultiValue.Cons x) =
   fmap MultiValue.Cons x

instance
   (Expr.Aggregate e mv) =>
      Expr.Aggregate (Parameter e) (Parameter mv) where
   type MultiValuesOf (Parameter e) = Parameter (Expr.MultiValuesOf e)
   type ExpressionsOf (Parameter mv) = Parameter (Expr.ExpressionsOf mv)
   bundle = traverse Expr.bundle
   dissect = fmap Expr.dissect



instance (Tuple.Phi a) => Tuple.Phi (Filt2.State a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Filt2.State a) where
   undef = Tuple.undefPointed


type StateStruct a = LLVM.Struct (a, (a, (a, (a, (a, ())))))

stateMemory ::
   (Memory.C a) =>
   Memory.Record r (StateStruct (Memory.Struct a)) (Filt2.State a)
stateMemory =
   App.lift4 Filt2.State
      (Memory.element Filt2.u1 d0)
      (Memory.element Filt2.u2 d1)
      (Memory.element Filt2.y1 d2)
      (Memory.element Filt2.y2 d3)


instance (Memory.C a) => Memory.C (Filt2.State a) where
   type Struct (Filt2.State a) = StateStruct (Memory.Struct a)
   load = Memory.loadRecord stateMemory
   store = Memory.storeRecord stateMemory
   decompose = Memory.decomposeRecord stateMemory
   compose = Memory.composeRecord stateMemory

instance (Value.Flatten a) => Value.Flatten (Filt2.State a) where
   type Registers (Filt2.State a) = Filt2.State (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable

instance
   (Expr.Aggregate e mv) =>
      Expr.Aggregate (Filt2.State e) (Filt2.State mv) where
   type MultiValuesOf (Filt2.State e) = Filt2.State (Expr.MultiValuesOf e)
   type ExpressionsOf (Filt2.State mv) = Filt2.State (Expr.ExpressionsOf mv)
   bundle = traverse Expr.bundle
   dissect = fmap Expr.dissect


{-# DEPRECATED bandpassParameter "only for testing, use Universal or Moog filter for production code" #-}
bandpassParameterCode ::
   (A.Transcendental a, A.RationalConstant a) =>
   a -> a ->
   CodeGenFunction r (Parameter a)
bandpassParameterCode reson cutoff = do
   rreson <- A.fdiv A.one reson
   k <- A.sub A.one rreson
   k2 <- A.neg =<< A.mul k k
   kcos <-
      A.mul (A.fromInteger' 2) =<< A.mul k =<<
      A.cos =<< A.mul cutoff =<<
      Value.decons Value.tau
   return $ Filt2.Parameter  rreson A.zero A.zero  kcos k2

-- ToDo: move to synthesizer-core:Filter.SecondOrder (it is not the universal filter)
bandpassParameter :: (Trans.C a) => a -> a -> Parameter a
bandpassParameter reson cutoff =
   let rreson = recip reson
       k = one - rreson
   in Filt2.Parameter  rreson zero zero  (2*k*cos(2*pi*cutoff)) (-k*k)

modifier ::
   (a ~ A.Scalar v, A.PseudoModule v, A.IntegerConstant a) =>
   Modifier.Simple
      (Filt2.State (Value.T v))
      (Parameter (Value.T a))
      (Value.T v) (Value.T v)
modifier =
   Filt2.modifier

causal ::
   (a ~ A.Scalar v, A.PseudoModule v, A.IntegerConstant a, Memory.C v) =>
   Causal.T (Parameter a, v) v
causal =
   Causal.fromModifier modifier

causalExp ::
   (Expr.Aggregate ae a, Memory.C a, Module.C ae ve,
    Expr.Aggregate ve v, Memory.C v) =>
   CausalExp.T (Parameter a, v) v
causalExp =
   CausalExp.fromModifier Filt2.modifier


{- |
Vector size must be at least D2.
-}
causalPacked,
  causalRecursivePacked ::
   (Serial.Write v, Serial.Element v ~ a,
    Memory.C v, Memory.C a, A.IntegerConstant v, A.IntegerConstant a,
    A.PseudoRing v, A.PseudoRing a) =>
   Causal.T (Parameter a, v) v
causalPacked =
   causalRecursivePacked <<<
   (arr fst &&& causalNonRecursivePacked)

_causalRecursivePackedAlt,
  causalNonRecursivePacked ::
   (Serial.Write v, Serial.Element v ~ a,
    Memory.C a, A.IntegerConstant v, A.IntegerConstant a,
    A.PseudoRing v, A.PseudoRing a) =>
   Causal.T (Parameter a, v) v
causalNonRecursivePacked =
   Causal.mapAccum
      (\(p, v0) (x1,x2) -> do
         (u1n,v1) <- Serial.shiftUp x1 v0
         (u2n,v2) <- Serial.shiftUp x2 v1
         w0 <- A.mul v0 =<< Serial.upsample (Filt2.c0 p)
         w1 <- A.mul v1 =<< Serial.upsample (Filt2.c1 p)
         w2 <- A.mul v2 =<< Serial.upsample (Filt2.c2 p)
         y  <- A.add w0 =<< A.add w1 w2
         return (y, (u1n,u2n)))
      (return (A.zero, A.zero))

{-
A filter of second order can be considered
as the convolution of two filters of first order.

[1,r]*[1,0,r^2] = [1,r,r^2,r^3]
[1,r,r^2,r^3] * [1,s,s^2,s^3]
 = [1,r]*[1,s]*[1,0,r^2]*[1,0,s^2]
     with
       a=r+s
       b=r*s
 = [1,a,b]*[1,0,r^2]*[1,0,s^2]
 = [1,a,b]*[1,0,a^2-2*b,0,b^2]

[1,0,0,0,r^4]*[1,0,0,0,s^4]
 = [1,0,0,0,(a^2-2*b)^2-2*b^2,0,0,0,b^4]
 = [1,0,0,0,a^4-4*a^2*b+2*b^2,0,0,0,b^4]
-}

{-
x = [x0, x1, x2, x3]

filter2 (a,-b) (y1,y2) x
  = [x0 + a*y1 - b*y2,
     x1 + a*x0 + (a^2-b)*y1 - a*b*y2,
     x2 + a*x1 + (a^2-b)*x0 + (a^3-2*a*b)*y1 + (-a^2*b+b^2)*y2,
     x3 + a*x2 + (a^2-b)*x1 + (a^3-2*a*b)*x0 + (a^4-3*a^2*b+b^2)*y1 + (-a^3*b+2*a*b^2)*y2]

(f0x = insert 0 (k*y1) x)
f1x = f0x + a * f0x->1 + b * f0x->2
f2x = f1x + (a^2-2*b) * f1x->2 + b^2 * f1x->4
-}
causalRecursivePacked =
   Causal.mapAccum
      (\(p, x0) y1v -> do
         let size = Serial.size x0

         d1v  <- Serial.upsample (Filt2.d1 p)
         d2v  <- Serial.upsample (Filt2.d2 p)
         d2vn <- A.neg d2v

         y1  <- Serial.last y1v
         xk1 <-
            Serial.modify (valueOf 0)
               (\u0 -> A.add u0 =<< A.mul (Filt2.d1 p) y1) =<<
            A.add x0 =<< A.mul d2v =<<
            Serial.shiftDownMultiZero (size - 2) y1v

         -- let xk2 = xk1
         xk2 <-
            fmap fst $
            foldM
               (\(y,(a,b)) d ->
                  liftM2 (,)
                     (A.add y =<<
                      M.liftJoin2 A.add
                         {-
                         Possibility for optimization:
                         In the last step the second operand is a zero vector
                         (LLVM already optimizes this away)
                         and the first operand could be merged
                         with the second operand of the previous step.
                         -}
                         (Serial.shiftUpMultiZero d =<< A.mul y a)
                         (Serial.shiftUpMultiZero (2*d) =<< A.mul y b)) $
                  liftM2 (,)
                     (M.liftJoin2 A.sub
                         (A.mul a a)
                         (A.mul b (A.fromInteger' 2)))
                     (A.mul b b))
               (xk1,(d1v,d2vn))
               (takeWhile (< size) $ iterate (2*) 1)

         return (xk2, xk2))
      (return A.zero)

_causalRecursivePackedAlt =
   Causal.mapAccum
      (\(p, x0) (x1,x2) -> do
         let size = Serial.size x0
         -- let xk1 = x0
         xk1 <-
            Serial.modify (valueOf 0)
               (\u0 ->
                  A.add u0 =<<
                  M.liftJoin2 A.add (A.mul (Filt2.d2 p) x2) (A.mul (Filt2.d1 p) x1)) =<<
            Serial.modify (valueOf 1)
               (\u1 -> A.add u1 =<< A.mul (Filt2.d2 p) x1)
            x0

         -- let xk2 = xk1
         d1v <- Serial.upsample (Filt2.d1 p)
         d2v <- Serial.upsample =<< A.neg (Filt2.d2 p)
         xk2 <-
            fmap fst $
            foldM
               (\(y,(a,b)) d ->
                  liftM2 (,)
                     (A.add y =<<
                      M.liftJoin2 A.add
                         (Serial.shiftUpMultiZero d =<< A.mul y a)
                         (Serial.shiftUpMultiZero (2*d) =<< A.mul y b)) $
                  liftM2 (,)
                     (M.liftJoin2 A.sub
                         (A.mul a a)
                         (A.mul b (A.fromInteger' 2)))
                     (A.mul b b))
               (xk1,(d1v,d2v))
               (takeWhile (< size) $ iterate (2*) 1)

         y0 <- Serial.extract (valueOf $ fromIntegral size - 1) xk2
         y1 <- Serial.extract (valueOf $ fromIntegral size - 2) xk2
         return (xk2, (y0,y1)))
      (return (A.zero, A.zero))

{-
A filter of second order can also be represented
by a filter of first order with 2x2-matrix coefficients.

filter1 ((d1,d2), (1,0)) (y1,y2) [(x0,0), (x1,0), (x2,0), (x3,0)]

/d1i d2i\ . /d1j d2j\ = /d1i*d1j + d2i  d1i*d2j\
\ 1   0 /   \ 1   0 /   \    d1j            d2j/


With this representation we can also implement filters
with time-variant filter parameters
using time-variant first-order filter.
-}
