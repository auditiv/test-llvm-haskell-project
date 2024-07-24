{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Data type that allows handling of piecewise constant signals
independently from the source.
-}
module Synthesizer.LLVM.ConstantPiece (
   T(..),
   Struct,
   parameterMemory,
   flatten,
   causalMap,
   ) where

import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Private as Sig

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.MaybeContinuation as Maybe
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Arithmetic as A
import LLVM.Extra.Control (whileLoop)

import qualified LLVM.Core as LLVM
import LLVM.Core (Value, valueOf)

import Type.Data.Num.Decimal (d0, d1)

import Data.Tuple.HT (mapSnd)
import Data.Word (Word)

import Control.Applicative (liftA2, (<$>))

import NumericPrelude.Numeric ()
import NumericPrelude.Base


data T a = Cons (Value Word) a

instance Functor T where
   fmap f (Cons len y) = Cons len (f y)

instance (Tuple.Phi a) => Tuple.Phi (T a) where
   phi bb (Cons len y) =
      liftA2 Cons (Tuple.phi bb len) (Tuple.phi bb y)
   addPhi bb (Cons lenA ya) (Cons lenB yb) =
      Tuple.addPhi bb lenA lenB >> Tuple.addPhi bb ya yb

instance (Tuple.Undefined a) => Tuple.Undefined (T a) where
   undef = Cons Tuple.undef Tuple.undef

instance (Tuple.Zero a) => Tuple.Zero (T a) where
   zero = Cons Tuple.zero Tuple.zero

type Struct a = LLVM.Struct (Word, (a, ()))

parameterMemory ::
   (Memory.C a) =>
   Memory.Record r (Struct (Memory.Struct a)) (T a)
parameterMemory =
   liftA2 Cons
      (Memory.element (\(Cons len _y) -> len) d0)
      (Memory.element (\(Cons _len y) -> y)   d1)

instance (Memory.C a) => Memory.C (T a) where
   type Struct (T a) = Struct (Memory.Struct a)
   load = Memory.loadRecord parameterMemory
   store = Memory.storeRecord parameterMemory
   decompose = Memory.decomposeRecord parameterMemory
   compose = Memory.composeRecord parameterMemory


causalMap ::
   (Expr.Aggregate a am, Expr.Aggregate b bm) =>
   (a -> b) -> Causal.T (T am) (T bm)
causalMap f = Causal.map (\(Cons len y) -> Cons len <$> Expr.unliftM1 f y)


flatten :: (Memory.C a) => Sig.T (T a) -> Sig.T a
flatten (Sig.Cons next start stop) =
   Sig.Cons
      (\global local state0 -> do
         ~(Cons length1 y1, s1) <-
            Maybe.fromBool $
            whileLoop (valueOf True, state0)
               (\(cont, (Cons len _y, _s)) ->
                  LLVM.and cont =<< A.cmp LLVM.CmpEQ len A.zero)
               (\(_cont, (Cons _len _y, s)) ->
                  Maybe.toBool $ next global local s)
         length2 <- Maybe.lift (A.dec length1)
         return (y1, (Cons length2 y1, s1)))
      (mapSnd ((,) (Cons A.zero Tuple.undef)) <$> start)
      stop
