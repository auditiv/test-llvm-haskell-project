{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Synthesizer.LLVM.Filter.SecondOrderPacked (
   Parameter, ParameterExp, bandpassParameter, State, causal,
   ) where

import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2L
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2

import qualified Synthesizer.LLVM.Causal.Private as Causal

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp(Exp))

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import Type.Data.Num.Decimal (D4, d0, d1)

import Control.Applicative (liftA2)

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
Layout:

> c0 [c1 d1 c2 d2]
-}
data Parameter a = Parameter (MultiValue.T a) (MultiVector.T D4 a)

instance (MultiVector.C a) => Tuple.Phi (Parameter a) where
   phi bb (Parameter r i) = do
      r' <- Tuple.phi bb r
      i' <- Tuple.phi bb i
      return (Parameter r' i')
   addPhi bb (Parameter r i) (Parameter r' i') = do
      Tuple.addPhi bb r r'
      Tuple.addPhi bb i i'

instance (MultiVector.C a) => Tuple.Undefined (Parameter a) where
   undef = Parameter Tuple.undef Tuple.undef


type ParameterStruct a = Memory.Struct (MultiValue.T a, MultiVector.T D4 a)

parameterMemory ::
   (Marshal.C a, Marshal.Vector D4 a) =>
   Memory.Record r (ParameterStruct a) (Parameter a)
parameterMemory =
   liftA2 Parameter
      (Memory.element (\(Parameter c0 _) -> c0) d0)
      (Memory.element (\(Parameter _ cd) -> cd) d1)

instance (Marshal.C a, Marshal.Vector D4 a) => Memory.C (Parameter a) where
   type Struct (Parameter a) = ParameterStruct a
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory


data ParameterExp a =
   ParameterExp (forall r. LLVM.CodeGenFunction r (Parameter a))

instance Expr.Aggregate (ParameterExp a) (Parameter a) where
   type MultiValuesOf (ParameterExp a) = Parameter a
   type ExpressionsOf (Parameter a) = ParameterExp a
   dissect x = ParameterExp (return x)
   bundle (ParameterExp code) = code


type State = MultiVector.T D4


{-# DEPRECATED bandpassParameter "only for testing, use Universal or Moog filter for production code" #-}
bandpassParameter ::
   (MultiVector.C a, MultiValue.Transcendental a,
    MultiValue.RationalConstant a) =>
   Exp a -> Exp a -> ParameterExp a
bandpassParameter (Exp reson) (Exp cutoff) =
   ParameterExp (do
      r <- reson
      c <- cutoff
      bandpassParameterCode r c)

bandpassParameterCode ::
   (MultiVector.C a, MultiValue.Transcendental a,
    MultiValue.RationalConstant a) =>
   MultiValue.T a ->
   MultiValue.T a ->
   LLVM.CodeGenFunction r (Parameter a)
bandpassParameterCode reson cutoff = do
   p <- Filt2L.bandpassParameterCode reson cutoff
   v <-
      MultiVector.assembleFromVector $ fmap ($ p) $
      LLVM.consVector Filt2.c1 Filt2.d1 Filt2.c2 Filt2.d2
   return $ Parameter (Filt2.c0 p) v


next ::
   (MultiVector.PseudoRing a) =>
   (Parameter a, MultiValue.T a) ->
   State a ->
   LLVM.CodeGenFunction r (MultiValue.T a, State a)
next (Parameter c0 k1, x0) y1 = do
   s0 <- A.mul c0 x0
   s1 <- MultiVector.dotProduct k1 y1
   y0 <- A.add s0 s1
   x1new <- MultiVector.extract (LLVM.valueOf 0) y1
   y1new <- MultiVector.extract (LLVM.valueOf 1) y1
   yv <- MultiVector.assembleFromVector $ LLVM.consVector x0 y0 x1new y1new
   return (y0, yv)

causal ::
   (MultiVector.PseudoRing a) =>
   (Marshal.Vector D4 a) =>
   Causal.T (Parameter a, MultiValue.T a) (MultiValue.T a)
causal = Causal.mapAccum next (return A.zero)
