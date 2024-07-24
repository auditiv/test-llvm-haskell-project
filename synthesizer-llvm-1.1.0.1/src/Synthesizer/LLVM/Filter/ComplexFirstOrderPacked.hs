{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Filter.ComplexFirstOrderPacked (
   Parameter(Parameter), parameterPlain, parameter, causal,
   ParameterMV,
   ) where

import qualified Synthesizer.LLVM.Filter.ComplexFirstOrder as ComplexFilter

import qualified Synthesizer.LLVM.Causal.Private as Causal

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp(Exp))

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import Type.Data.Num.Decimal (D3, d0, d1)

import Control.Applicative (liftA2)

import qualified Algebra.Transcendental as Trans

import qualified Number.Complex as Complex

import NumericPrelude.Numeric
import NumericPrelude.Base


data Parameter a = Parameter (LLVM.Vector D3 a) (LLVM.Vector D3 a)

data ParameterMV a = ParameterMV (MultiVector.T D3 a) (MultiVector.T D3 a)

instance (MultiVector.C a) => Tuple.Phi (ParameterMV a) where
   phi bb (ParameterMV r i) = do
      r' <- Tuple.phi bb r
      i' <- Tuple.phi bb i
      return (ParameterMV r' i')
   addPhi bb
        (ParameterMV r i)
        (ParameterMV r' i') = do
      Tuple.addPhi bb r r'
      Tuple.addPhi bb i i'

instance (MultiVector.C a) => Tuple.Undefined (ParameterMV a) where
   undef = ParameterMV Tuple.undef Tuple.undef


type ParameterStruct a = Marshal.Struct (LLVM.Vector D3 a, LLVM.Vector D3 a)

parameterMemory ::
   (Marshal.Vector D3 a) =>
   Memory.Record r (ParameterStruct a) (ParameterMV a)
parameterMemory =
   liftA2 ParameterMV
      (Memory.element (\(ParameterMV kr _) -> kr) d0)
      (Memory.element (\(ParameterMV _ ki) -> ki) d1)

instance (Marshal.Vector D3 a) => Memory.C (ParameterMV a) where
   type Struct (ParameterMV a) = ParameterStruct a
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory


data ParameterExp a =
   ParameterExp (forall r. LLVM.CodeGenFunction r (ParameterMV a))

instance Expr.Aggregate (ParameterExp a) (ParameterMV a) where
   type MultiValuesOf (ParameterExp a) = ParameterMV a
   type ExpressionsOf (ParameterMV a) = ParameterExp a
   dissect x = ParameterExp (return x)
   bundle (ParameterExp code) = code


parameterPlain :: (Trans.C a) => a -> a -> Parameter a
parameterPlain reson freq =
   let (ComplexFilter.Parameter amp k) = ComplexFilter.parameter reson freq
       kr = Complex.real k
       ki = Complex.imag k
   in Parameter
         (LLVM.consVector kr (-ki) amp)
         (LLVM.consVector ki   kr  amp)

parameter ::
   (MultiVector.Transcendental a, MultiVector.RationalConstant a) =>
   Exp a -> Exp a -> ParameterExp a
parameter (Exp reson) (Exp freq) =
   ParameterExp (do
      r <- reson
      f <- freq
      ~(ComplexFilter.Parameter amp k) <- ComplexFilter.parameterCode r f
      let kr = Complex.real k
      let ki = Complex.imag k
      kin <- A.neg ki
      liftA2 ParameterMV
         (MultiVector.assembleFromVector $ LLVM.consVector kr kin amp)
         (MultiVector.assembleFromVector $ LLVM.consVector ki kr  amp))


type State a = MultiVector.T D3 a

next ::
   (MultiVector.PseudoRing a) =>
   (ParameterMV a, Stereo.T (MultiValue.T a)) ->
   State a -> LLVM.CodeGenFunction r (Stereo.T (MultiValue.T a), State a)
next (ParameterMV kr ki, x) s = do
   let two = LLVM.valueOf 2
   sr <- MultiVector.insert two (Stereo.left  x) s
   yr <- MultiVector.dotProduct kr sr

   si <- MultiVector.insert two (Stereo.right x) s
   yi <- MultiVector.dotProduct ki si

   sv <- MultiVector.assembleFromVector $ LLVM.consVector yr yi Tuple.undef
   return (Stereo.cons yr yi, sv)

causal ::
   (Marshal.Vector n a, n ~ D3, MultiVector.PseudoRing a) =>
   Causal.T
      (ParameterMV a, Stereo.T (MultiValue.T a))
      (Stereo.T (MultiValue.T a))
causal = Causal.mapAccum next (return A.zero)
