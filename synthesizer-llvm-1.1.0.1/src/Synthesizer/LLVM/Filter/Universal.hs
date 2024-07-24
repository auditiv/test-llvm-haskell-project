{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Synthesizer.LLVM.Filter.Universal (
   Result(Result, lowpass, highpass, bandpass, bandlimit),
   Parameter, parameter, causal,
   parameterCode, causalExp,
   multiValueResult, unMultiValueResult,
   multiValueParameter, unMultiValueParameter,
   ) where

import qualified Synthesizer.Plain.Filter.Recursive.Universal as Universal
import Synthesizer.Plain.Filter.Recursive.Universal
          (Parameter(Parameter), Result(..))
import Synthesizer.Plain.Filter.Recursive (Pole(..))

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
import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction)

import Type.Data.Num.Decimal (d0, d1, d2, d3, d4, d5)

import qualified Control.Applicative.HT as App
import Control.Applicative (liftA2, (<$>))

import qualified Data.Foldable as Fold
import Data.Traversable (traverse)

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module as Module


instance (Tuple.Phi a) => Tuple.Phi (Parameter a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Parameter a) where
   undef = Tuple.undefPointed


type ParameterStruct a = LLVM.Struct (a, (a, (a, (a, (a, (a, ()))))))

parameterMemory ::
   (Memory.C a) =>
   Memory.Record r (ParameterStruct (Memory.Struct a)) (Parameter a)
parameterMemory =
   App.lift6 Parameter
      (Memory.element Universal.k1       d0)
      (Memory.element Universal.k2       d1)
      (Memory.element Universal.ampIn    d2)
      (Memory.element Universal.ampI1    d3)
      (Memory.element Universal.ampI2    d4)
      (Memory.element Universal.ampLimit d5)


instance (Memory.C a) => Memory.C (Parameter a) where
   type Struct (Parameter a) = ParameterStruct (Memory.Struct a)
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory

instance (Marshal.C a) => Marshal.C (Parameter a) where
   pack p =
      case Marshal.pack <$> p of
         Parameter k1 k2 ampIn ampI1 ampI2 ampLimit ->
            LLVM.consStruct k1 k2 ampIn ampI1 ampI2 ampLimit
   unpack = fmap Marshal.unpack . LLVM.uncurryStruct Parameter

instance (Storable.C a) => Storable.C (Parameter a) where
   load = Storable.loadApplicative
   store = Storable.storeFoldable



type ResultStruct a = LLVM.Struct (a, (a, (a, (a, ()))))

resultMemory ::
   (Memory.C a) =>
   Memory.Record r (ResultStruct (Memory.Struct a)) (Result a)
resultMemory =
   App.lift4 Result
      (Memory.element Universal.highpass  d0)
      (Memory.element Universal.bandpass  d1)
      (Memory.element Universal.lowpass   d2)
      (Memory.element Universal.bandlimit d3)


instance (Memory.C a) => Memory.C (Result a) where
   type Struct (Result a) = ResultStruct (Memory.Struct a)
   load = Memory.loadRecord resultMemory
   store = Memory.storeRecord resultMemory
   decompose = Memory.decomposeRecord resultMemory
   compose = Memory.composeRecord resultMemory

instance (Tuple.Value a) => Tuple.Value (Result a) where
   type ValueOf (Result a) = Result (Tuple.ValueOf a)
   valueOf = Tuple.valueOfFunctor

instance (Value.Flatten a) => Value.Flatten (Result a) where
   type Registers (Result a) = Result (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable

instance (MultiValue.C a) => MultiValue.C (Result a) where
   type Repr (Result a) = Result (MultiValue.Repr a)
   cons = multiValueResult . fmap MultiValue.cons
   undef = multiValueResult $ pure MultiValue.undef
   zero = multiValueResult $ pure MultiValue.zero
   phi bb =
      fmap multiValueResult .
      traverse (MultiValue.phi bb) . unMultiValueResult
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb)
         (unMultiValueResult a) (unMultiValueResult b)

multiValueResult ::
   Result (MultiValue.T a) -> MultiValue.T (Result a)
multiValueResult = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValueResult ::
   MultiValue.T (Result a) -> Result (MultiValue.T a)
unMultiValueResult (MultiValue.Cons x) = fmap MultiValue.Cons x

instance (MarshalMV.C a) => MarshalMV.C (Result a) where
   pack p =
      case MarshalMV.pack <$> p of
         Result hp bp lp bl -> LLVM.consStruct hp bp lp bl
   unpack = fmap MarshalMV.unpack . LLVM.uncurryStruct Result

instance (Expr.Aggregate e mv) => Expr.Aggregate (Result e) (Result mv) where
   type MultiValuesOf (Result e) = Result (Expr.MultiValuesOf e)
   type ExpressionsOf (Result mv) = Result (Expr.ExpressionsOf mv)
   bundle = traverse Expr.bundle
   dissect = fmap Expr.dissect


instance (Tuple.Value a) => Tuple.Value (Parameter a) where
   type ValueOf (Parameter a) = Parameter (Tuple.ValueOf a)
   valueOf = Tuple.valueOfFunctor

instance (Value.Flatten a) => Value.Flatten (Parameter a) where
   type Registers (Parameter a) = Parameter (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable

instance (MultiValue.C a) => MultiValue.C (Parameter a) where
   type Repr (Parameter a) = Parameter (MultiValue.Repr a)
   cons = multiValueParameter . fmap MultiValue.cons
   undef = multiValueParameter $ pure MultiValue.undef
   zero = multiValueParameter $ pure MultiValue.zero
   phi bb =
      fmap multiValueParameter .
      traverse (MultiValue.phi bb) . unMultiValueParameter
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb)
         (unMultiValueParameter a) (unMultiValueParameter b)

multiValueParameter ::
   Parameter (MultiValue.T a) -> MultiValue.T (Parameter a)
multiValueParameter = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValueParameter ::
   MultiValue.T (Parameter a) -> Parameter (MultiValue.T a)
unMultiValueParameter (MultiValue.Cons x) = fmap MultiValue.Cons x

instance (MarshalMV.C a) => MarshalMV.C (Parameter a) where
   pack p =
      case MarshalMV.pack <$> p of
         Parameter k1 k2 ampIn ampI1 ampI2 ampLimit ->
            LLVM.consStruct k1 k2 ampIn ampI1 ampI2 ampLimit
   unpack = fmap MarshalMV.unpack . LLVM.uncurryStruct Parameter

instance
   (Expr.Aggregate e mv) =>
      Expr.Aggregate (Parameter e) (Parameter mv) where
   type MultiValuesOf (Parameter e) = Parameter (Expr.MultiValuesOf e)
   type ExpressionsOf (Parameter mv) = Parameter (Expr.ExpressionsOf mv)
   bundle = traverse Expr.bundle
   dissect = fmap Expr.dissect


instance (Vector.Simple v) => Vector.Simple (Parameter v) where
   type Element (Parameter v) = Parameter (Vector.Element v)
   type Size (Parameter v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (Parameter v) where
   insert = Vector.insertTraversable


instance (Tuple.Phi a) => Tuple.Phi (Result a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Result a) where
   undef = Tuple.undefPointed

instance (Vector.Simple v) => Vector.Simple (Result v) where
   type Element (Result v) = Result (Vector.Element v)
   type Size (Result v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (Result v) where
   insert  = Vector.insertTraversable

instance (Serial.Sized v) => Serial.Sized (Result v) where
   type Size (Result v) = Serial.Size v

instance (Serial.Read v) => Serial.Read (Result v) where
   type Element (Result v) = Result (Serial.Element v)
   type ReadIt (Result v) = Result (Serial.ReadIt v)
   extract = Serial.extractTraversable
   readStart = Serial.readStartTraversable
   readNext = Serial.readNextTraversable

instance (Serial.Write v) => Serial.Write (Result v) where
   type WriteIt (Result v) = Result (Serial.WriteIt v)
   insert  = Serial.insertTraversable
   writeStart = Serial.writeStartTraversable
   writeNext = Serial.writeNextTraversable
   writeStop = Serial.writeStopTraversable


parameterCode ::
   (A.Transcendental a, A.RationalConstant a) =>
   a -> a -> CodeGenFunction r (Parameter a)
parameterCode =
   Value.unlift2 $ \reson freq ->
   Universal.parameter (Pole reson freq)

parameter :: (Trans.C a) => a -> a -> Parameter a
parameter reson freq = Universal.parameter (Pole reson freq)


modifier ::
   (a ~ A.Scalar v, A.PseudoModule v, A.IntegerConstant a) =>
   Modifier.Simple
      (Universal.State (Value.T v))
      (Parameter (Value.T a))
      (Value.T v) (Result (Value.T v))
modifier =
   Universal.modifier

causal ::
   (a ~ A.Scalar v, A.PseudoModule v, A.IntegerConstant a, Memory.C v) =>
   Causal.T (Parameter a, v) (Result v)
causal = Causal.fromModifier modifier

causalExp ::
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v, Memory.C v) =>
   CausalExp.T (Parameter a, v) (Result v)
causalExp = CausalExp.fromModifier Universal.modifier

{-
The state variable filter could be vectorised
by writing the integrator network as matrix recursion
and applying the doubling trick to that recursion.
However the initially sparse matrix with several 1s in it
has dense power matrices with no nice structure.
This will only payoff for large vectors.

We could write another version,
that expresses the state variable filter in terms of the general second order filter.
The general second order filter is already vectorized.
-}
