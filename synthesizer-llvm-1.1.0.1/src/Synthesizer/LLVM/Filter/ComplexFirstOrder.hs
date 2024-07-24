{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Synthesizer.LLVM.Filter.ComplexFirstOrder (
   Parameter(Parameter), parameter, causal,
   parameterCode, causalExp,
   ) where

import qualified Synthesizer.LLVM.Causal.Process as CausalExp
import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Value as Value

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Complex as Complex

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction)

import Type.Data.Num.Decimal (d0, d1, d2)

import qualified Control.Applicative as App
import Control.Applicative (liftA2, liftA3, (<*>))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Ring as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base


data Parameter a =
   Parameter a (Complex.T a)

instance Functor Parameter where
   {-# INLINE fmap #-}
   fmap f (Parameter k c) =
      Parameter (f k) (fmap f c)

instance App.Applicative Parameter where
   {-# INLINE pure #-}
   pure x = Parameter x (x Complex.+: x)
   {-# INLINE (<*>) #-}
   Parameter fk fc <*> Parameter pk pc =
      Parameter (fk pk) $
         (Complex.real fc $ Complex.real pc)
         Complex.+:
         (Complex.imag fc $ Complex.imag pc)

instance Fold.Foldable Parameter where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Parameter where
   {-# INLINE sequenceA #-}
   sequenceA (Parameter k c) =
      liftA2 Parameter k $
      liftA2 (Complex.+:) (Complex.real c) (Complex.imag c)


instance (Tuple.Phi a) => Tuple.Phi (Parameter a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Parameter a) where
   undef = Tuple.undefPointed


type ParameterStruct a = LLVM.Struct (a, (a, (a, ())))

parameterMemory ::
   (Memory.C a) =>
   Memory.Record r (ParameterStruct (Memory.Struct a)) (Parameter a)
parameterMemory =
   liftA3 (\amp kr ki -> Parameter amp (kr Complex.+: ki))
      (Memory.element (\(Parameter  amp _) -> amp) d0)
      (Memory.element (\(Parameter _amp k) -> Complex.real k) d1)
      (Memory.element (\(Parameter _amp k) -> Complex.imag k) d2)

instance (Memory.C a) => Memory.C (Parameter a) where
   type Struct (Parameter a) = ParameterStruct (Memory.Struct a)
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory

instance (Value.Flatten a) => Value.Flatten (Parameter a) where
   type Registers (Parameter a) = Parameter (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable

instance
   (Expr.Aggregate e mv) =>
      Expr.Aggregate (Parameter e) (Parameter mv) where
   type MultiValuesOf (Parameter e) = Parameter (Expr.MultiValuesOf e)
   type ExpressionsOf (Parameter mv) = Parameter (Expr.ExpressionsOf mv)
   bundle = Trav.traverse Expr.bundle
   dissect = fmap Expr.dissect


parameterCode, _parameterCode ::
   (A.Transcendental a, A.RationalConstant a) =>
   a -> a -> CodeGenFunction r (Parameter a)
parameterCode reson freq =
   let amp = recip $ Value.unfold reson
   in  Value.flatten $ Parameter amp $
       Complex.scale (1-amp) $ Complex.cis $
       Value.unfold freq * Value.tau

_parameterCode reson freq = do
   amp <- A.fdiv A.one reson
   k   <- A.sub  A.one amp
   w  <- A.mul freq =<< Value.decons Value.tau
   kr <- A.mul k =<< A.cos w
   ki <- A.mul k =<< A.sin w
   return (Parameter amp (kr Complex.+: ki))

parameter :: (Trans.C a) => a -> a -> Parameter a
parameter reson freq =
   let amp = recip reson
   in Parameter amp $
      Complex.scale (1-amp) $ Complex.cis $ freq * 2*pi


{-
Synthesizer.Plain.Filter.Recursive.FirstOrderComplex.step
cannot be used directly, because Filt1C has complex amplitude
-}
next, _next ::
   (A.PseudoRing a, A.IntegerConstant a) =>
   (Parameter a, Stereo.T a) ->
   Complex.T a ->
   CodeGenFunction r (Stereo.T a, Complex.T a)
next inp state =
   let stereoFromComplexVal :: Complex.T (Value.T a) -> Stereo.T (Value.T a)
       stereoFromComplexVal = stereoFromComplex
       (Parameter amp k, x) = Value.unfold inp
       xc = Stereo.left x  Complex.+:  Stereo.right x
       y = Complex.scale amp xc + k * Value.unfold state
   in  Value.flatten (stereoFromComplexVal y, y)

_next (Parameter amp k, x) s = do
   let kr = Complex.real k
       ki = Complex.imag k
       sr = Complex.real s
       si = Complex.imag s
   yr <- Value.decons $
      Value.lift0 (A.mul (Stereo.left x) amp) +
      Value.lift0 (A.mul kr sr) - Value.lift0 (A.mul ki si)
   yi <- Value.decons $
      Value.lift0 (A.mul (Stereo.right x) amp) +
      Value.lift0 (A.mul kr si) + Value.lift0 (A.mul ki sr)
   return (Stereo.cons yr yi, yr Complex.+: yi)


start ::
   (A.Additive a) =>
   CodeGenFunction r (Complex.T a)
start =
   return (A.zero Complex.+: A.zero)

causal ::
   (A.PseudoRing a, A.IntegerConstant a, Memory.C a) =>
   Causal.T
      (Parameter a, Stereo.T a)
      (Stereo.T a)
causal =
   Causal.mapAccum next start


stereoFromComplex :: Complex.T a -> Stereo.T a
stereoFromComplex c = Stereo.cons (Complex.real c) (Complex.imag c)

nextPlain ::
   (Ring.C a) =>
   (Parameter a, Stereo.T a) -> Complex.T a -> (Stereo.T a, Complex.T a)
nextPlain (Parameter amp k, x) state =
   let xc = Stereo.left x  Complex.+:  Stereo.right x
       y = Complex.scale amp xc + k * state
   in (stereoFromComplex y, y)

causalExp ::
   (Ring.C ae, Expr.Aggregate ae a, Memory.C a) =>
   CausalExp.T (Parameter a, Stereo.T a) (Stereo.T a)
causalExp =
   CausalExp.mapAccum nextPlain zero
