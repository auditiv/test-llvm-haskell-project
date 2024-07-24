{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Fold where

import qualified LLVM.Extra.Arithmetic as A
import LLVM.Core (CodeGenFunction)

import Control.Applicative (liftA2, liftA3)

import Prelude hiding (sum)


data T a b = Cons (forall r. b -> a -> CodeGenFunction r b) b

premap :: (forall r. a -> CodeGenFunction r b) -> T b c -> T a c
premap f (Cons acc b0) = Cons (\b a -> acc b =<< f a) b0


maxZero :: (A.Real a) => T a a
maxZero = Cons A.max A.zero

maxAbs :: (A.Real a) => T a a
maxAbs = premap A.abs maxZero

sum :: (A.Additive a) => T a a
sum = Cons A.add A.zero

sumSquare :: (A.PseudoRing a) => T a a
sumSquare = premap A.square sum


pair :: T a0 b0 -> T a1 b1 -> T (a0,a1) (b0,b1)
pair (Cons acc0 b00) (Cons acc1 b10) =
   Cons (\(a0,a1) (b0,b1) -> liftA2 (,) (acc0 a0 b0) (acc1 a1 b1)) (b00,b10)

triple :: T a0 b0 -> T a1 b1 -> T a2 b2 -> T (a0,a1,a2) (b0,b1,b2)
triple (Cons acc0 b00) (Cons acc1 b10) (Cons acc2 b20) =
   Cons
      (\(a0,a1,a2) (b0,b1,b2) ->
         liftA3 (,,) (acc0 a0 b0) (acc1 a1 b1) (acc2 a2 b2))
      (b00,b10,b20)
