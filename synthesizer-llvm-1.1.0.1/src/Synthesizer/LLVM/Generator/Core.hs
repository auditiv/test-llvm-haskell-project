{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Synthesizer.LLVM.Generator.Core where

import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Private as Sig
import qualified Synthesizer.LLVM.Random as Rnd

import Synthesizer.Causal.Class (($*))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A

import Control.Applicative ((<$>))

import Data.Word (Word32)

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (map, iterate, takeWhile, tail)



type MV a = Sig.T (MultiValue.T a)

iterate :: (Marshal.C a) => (Exp a -> Exp a) -> Exp a -> MV a
iterate f a = Sig.iterate (Expr.unliftM1 f) (Expr.unExp a)

-- ToDo: replace by constantSharing and scanl
iterateParam ::
   (Marshal.C a, Marshal.C b) =>
   (Exp b -> Exp a -> Exp a) -> Exp b -> Exp a -> MV a
iterateParam f b a =
   MultiValue.snd <$>
   iterate (Expr.uncurry $ \bi ai -> Expr.zip bi $ f bi ai) (Expr.zip b a)


ramp ::
   (Marshal.C a, MultiValue.Additive a) =>
   Exp a -> Exp a -> MV a
ramp = iterateParam Expr.add

parabola ::
   (Marshal.C a, MultiValue.Additive a) =>
   Exp a -> Exp a -> Exp a -> MV a
parabola d2 d1 start = integrate start $* ramp d2 d1

integrate ::
   (Marshal.C a, MultiValue.Additive a, MultiValue.T a ~ al) =>
   Exp a -> Causal.T al al
integrate start =
   Causal.mapAccum (\a s -> (,) s <$> A.add s a) (Expr.unExp start)


osci ::
   (MultiValue.Fraction t, Marshal.C t) =>
   Exp t -> Exp t -> MV t
osci phase freq  =  iterate (Expr.liftM2 A.incPhase freq) phase

exponential ::
   (Marshal.C a, MultiValue.PseudoRing a) =>
   Exp a -> Exp a -> MV a
exponential  =  iterateParam Expr.mul

exponentialBounded ::
   (Marshal.C a, MultiValue.PseudoRing a,
    MultiValue.Real a, MultiValue.IntegerConstant a) =>
   Exp a -> Exp a -> Exp a -> MV a
exponentialBounded bound decay =
   iterateParam
      (\bk y -> case Expr.unzip bk of (b,k) -> Expr.max b $ k*y)
      (Expr.zip bound decay)


noise, noiseAlt :: Exp Word32 -> MV Word32
noise seed =
   iterate (Expr.liftReprM Rnd.nextCG)
      (Expr.irem seed (Expr.cons Rnd.modulus-1) + 1)

noiseAlt seed =
   iterate (Expr.liftReprM Rnd.nextCG32)
      (Expr.irem seed (Expr.cons Rnd.modulus-1) + 1)
