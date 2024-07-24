{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
module Synthesizer.LLVM.Filter.Moog (
   Parameter, parameter,
   causal, causalInit,
   ) where

import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1 ()

import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as FirstOrder
import qualified Synthesizer.Plain.Filter.Recursive.Moog as Moog
import Synthesizer.Plain.Filter.Recursive (Pole(..))

import qualified Synthesizer.LLVM.Causal.Process as Causal

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (d0, d1)
import Type.Base.Proxy (Proxy(Proxy))

import qualified Control.Arrow as Arrow
import qualified Control.Applicative as App
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import Control.Arrow (arr, (>>>), (&&&))
import Control.Applicative (liftA2)

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module as Module
import NumericPrelude.Numeric
import NumericPrelude.Base


newtype Parameter n a = Parameter {getParam :: Moog.Parameter a}
   deriving (Functor, App.Applicative, Fold.Foldable, Trav.Traversable)


instance (Tuple.Phi a, TypeNum.Natural n) =>
      Tuple.Phi (Parameter n a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance (Tuple.Undefined a, TypeNum.Natural n) =>
      Tuple.Undefined (Parameter n a) where
   undef = Tuple.undefPointed

instance (Tuple.Zero a, TypeNum.Natural n) =>
      Tuple.Zero (Parameter n a) where
   zero = Tuple.zeroPointed


type ParameterStruct a =
   LLVM.Struct (Memory.Struct a, (Memory.Struct (FirstOrder.Parameter a), ()))

parameterMemory ::
   (Memory.C a, TypeNum.Natural n) =>
   Memory.Record r (ParameterStruct a) (Parameter n a)
parameterMemory =
   liftA2 (\f k -> Parameter (Moog.Parameter f k))
      (Memory.element (Moog.feedback     . getParam) d0)
      (Memory.element (Moog.lowpassParam . getParam) d1)

instance
   (Memory.C a, TypeNum.Natural n) =>
      Memory.C (Parameter n a) where
   type Struct (Parameter n a) = ParameterStruct a
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory


instance
   (Vector.Simple v, TypeNum.Natural n) =>
      Vector.Simple (Parameter n v) where
   type Element (Parameter n v) = Parameter n (Vector.Element v)
   type Size (Parameter n v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v, TypeNum.Natural n) => Vector.C (Parameter n v) where
   insert = Vector.insertTraversable


parameter ::
   (TypeNum.Natural n, Trans.C a) =>
   Proxy n -> a -> a -> Parameter n a
parameter order reson freq =
   Parameter $
   Moog.parameter (TypeNum.integralFromProxy order) (Pole reson freq)

instance
   (n ~ m, Expr.Aggregate e mv) =>
      Expr.Aggregate (Parameter n e) (Parameter m mv) where
   type MultiValuesOf (Parameter n e) = Parameter n (Expr.MultiValuesOf e)
   type ExpressionsOf (Parameter m mv) = Parameter m (Expr.ExpressionsOf mv)
   bundle (Parameter (Moog.Parameter f k)) =
      fmap Parameter $ liftA2 Moog.Parameter (Expr.bundle f) (Expr.bundle k)
   dissect (Parameter (Moog.Parameter f k)) =
      Parameter (Moog.Parameter (Expr.dissect f) (Expr.dissect k))


merge ::
   (Module.C a v) => (Parameter n a, v) -> v -> (FirstOrder.Parameter a, v)
merge (Parameter (Moog.Parameter f k), x) y0 = (k, x - f *> y0)

amplify :: (Module.C a v) => Parameter n a -> v -> v
amplify p y1 = (1 + Moog.feedback (getParam p)) *> y1

causal ::
   (TypeNum.Natural n, Memory.C v,
    Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v) =>
   Causal.T (Parameter n a, v) v
causal =
   causalSize
      (flip (Causal.feedbackControlled zero) (arr snd))
      Proxy


causalInit ::
   (TypeNum.Natural n, Memory.C v,
    Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v) =>
   ve -> Causal.T (Parameter n a, v) v
causalInit initial =
   causalSize
      (flip
         (Causal.feedbackControlled initial)
         (arr snd))
      Proxy


causalSize ::
   (TypeNum.Natural n, Memory.C v,
    Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v) =>
   (Causal.T ((Parameter n a, v), v) v ->
    Causal.T (Parameter n a, v) v) ->
   Proxy n ->
   Causal.T (Parameter n a, v) v
causalSize feedback n =
   let order = TypeNum.integralFromProxy n
   in  Arrow.arr fst &&&
       feedback
          (Causal.zipWith merge >>>
           Causal.replicateControlled order
             (Causal.fromModifier FirstOrder.lowpassModifier))
        >>> Causal.zipWith amplify
