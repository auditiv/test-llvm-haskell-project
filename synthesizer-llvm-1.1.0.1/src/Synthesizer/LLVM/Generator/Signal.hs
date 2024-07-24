{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Generator.Signal (
   Sig.T,
   MV,

   constant,
   fromArray,
   Core.iterate,
   takeWhile,
   take,
   tail,
   drop,
   Sig.append,
   cycle,

   amplify,

   osci,
   exponential2,
   exponentialBounded2,
   noise,

   adjacentNodes02,
   adjacentNodes13,
   interpolateConstant,

   rampSlope,
   rampInf,
   ramp,
   parabolaFadeInInf,
   parabolaFadeOutInf,
   parabolaFadeIn,
   parabolaFadeOut,
   parabolaFadeInMap,
   parabolaFadeOutMap,
   ) where

import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Core as Core
import qualified Synthesizer.LLVM.Generator.Private as Sig
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import qualified Synthesizer.LLVM.Frame as Frame
import qualified Synthesizer.LLVM.Random as Rnd
import Synthesizer.LLVM.Generator.Private (arraySize)
import Synthesizer.LLVM.Private (noLocalPtr)

import qualified Synthesizer.Causal.Class as CausalC
import Synthesizer.Causal.Class (apply, ($*), ($<))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Iterator as Iter
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction)

import qualified Type.Data.Num.Decimal.Number as TypeNum
import Type.Data.Num.Decimal.Number ((:*:))

import Control.Monad.HT ((<=<))
import Control.Applicative (liftA2)

import Data.Word (Word32, Word)
import Data.Int (Int32)

import NumericPrelude.Numeric
import NumericPrelude.Base hiding
         (map, iterate, takeWhile, take, tail, drop, cycle)



type MV a = Sig.T (MultiValue.T a)

constant :: (Expr.Aggregate ae al, Memory.C al) => ae -> Sig.T al
constant a = Sig.iterate return (Expr.bundle a)


fromArray ::
   (TypeNum.Natural n, Marshal.C a) =>
   ((n :*: LLVM.SizeOf (Marshal.Struct a)) ~ arrSize,
    TypeNum.Natural arrSize) =>
   Exp (MultiValue.Array n a) -> MV a
fromArray arrExp = Sig.Cons
   (\arrPtr -> noLocalPtr $ \i -> do
      inRange <- MaybeCont.lift $
         LLVM.cmp LLVM.CmpLT i $ LLVM.valueOf $
            TypeNum.integralFromProxy $ arraySize arrExp
      MaybeCont.guard inRange
      MaybeCont.lift $ do
         ptr <- LLVM.getElementPtr0 arrPtr (i, ())
         liftA2 (,) (Memory.load ptr) (A.inc i))
   (do
      arrPtr <- LLVM.malloc
      flip Memory.store arrPtr =<< Expr.unExp arrExp
      return (arrPtr, A.zero :: LLVM.Value Word))
   LLVM.free


takeWhile :: (Expr.Aggregate ae a) => (ae -> Exp Bool) -> Sig.T a -> Sig.T a
takeWhile p =
   Sig.takeWhile (fmap (\(MultiValue.Cons cont) -> cont) . Expr.unliftM1 p)

take :: Exp Word -> Sig.T a -> Sig.T a
take len =
   liftA2 (flip const) $ takeWhile (0 Expr.<*) (Core.iterate (subtract 1) len)

{- |
@tail empty@ generates the empty signal.
-}
tail :: Sig.T a -> Sig.T a
tail (Sig.Cons next start stop) = Sig.Cons
   next
   (do
      local <- LLVM.alloca
      (global,s0) <- start
      MaybeCont.resolve (next global local s0)
         (return (global,s0))
         (\(_a,s1) -> return (global,s1)))
   stop

drop :: Exp Word -> Sig.T a -> Sig.T a
drop n (Sig.Cons next start stop) = Sig.Cons
   next
   (do
      local <- LLVM.alloca
      (global,state0) <- start
      ~(MultiValue.Cons nv) <- Expr.unExp n
      state1 <-
         Iter.mapWhileState_
            (\_ s0 ->
               MaybeCont.resolve (next global local s0)
                  (return (LLVM.valueOf False, s0))
                  (\(_a,s1) -> return (LLVM.valueOf True, s1)))
            (Iter.countDown nv) state0
      return (global,state1))
   stop


{- |
> cycle empty == empty
-}
cycle :: (Tuple.Phi a, Tuple.Undefined a) => Sig.T a -> Sig.T a
cycle (Sig.Cons next start stop) =
   Sig.Cons
      (\globalPtr local s0 ->
         MaybeCont.alternative
            (do
               c0 <- MaybeCont.lift $ Memory.load globalPtr
               next c0 local s0)
            (do
               (c1,s1) <- MaybeCont.lift $ do
                  stop =<< Memory.load globalPtr
                  cs1 <- start
                  Memory.store (fst cs1) globalPtr
                  return cs1
               next c1 local s1))
      (do
         globalPtr <- LLVM.malloc
         (global,state) <- start
         Memory.store global globalPtr
         return (globalPtr, state))
      (\globalPtr -> do
         stop =<< Memory.load globalPtr
         LLVM.free globalPtr)


amplify ::
   (Expr.Aggregate ea a, Memory.C a, A.PseudoRing a) =>
   ea -> Sig.T a -> Sig.T a
amplify x = apply (Causal.zipWith Frame.amplifyMono $< constant x)


rampInf, rampSlope,
 parabolaFadeInInf, parabolaFadeOutInf ::
   (Marshal.C a, MultiValue.Field a, MultiValue.IntegerConstant a) =>
   Exp a -> MV a
rampSlope slope  =  Core.ramp slope Expr.zero
rampInf dur  =  rampSlope (Expr.recip dur)

{-
t*(2-t) = 1 - (t-1)^2

(t+d)*(2-t-d) - t*(2-t)
   = d*(2-t) - d*t - d^2
   = 2*d*(1-t) - d^2
   = d*(2*(1-t) - d)

2*d*(1-t-d) + d^2  -  (2*d*(1-t) + d^2)
   = -2*d^2
-}
parabolaFadeInInf dur =
   Core.parabola
      ((\d -> -2*d*d)  $ Expr.recip dur)
      ((\d -> d*(2-d)) $ Expr.recip dur)
      Expr.zero

{-
1-t^2
-}
parabolaFadeOutInf dur =
   Core.parabola
      ((\d -> -2*d*d) $ Expr.recip dur)
      ((\d ->   -d*d) $ Expr.recip dur)
      Expr.one

ramp,
 parabolaFadeIn, parabolaFadeOut,
 parabolaFadeInMap, parabolaFadeOutMap ::
   (Marshal.C a, MultiValue.Field a, MultiValue.IntegerConstant a,
    MultiValue.NativeFloating a ar) =>
   Exp Word -> MV a

ramp dur =
   take dur $ rampInf (Expr.fromIntegral dur)

parabolaFadeIn dur =
   take dur $ parabolaFadeInInf (Expr.fromIntegral dur)

parabolaFadeOut dur =
   take dur $ parabolaFadeOutInf (Expr.fromIntegral dur)

parabolaFadeInMap dur =
   Causal.map (Expr.unliftM1 (\t -> t*(2-t))) $* ramp dur

parabolaFadeOutMap dur =
   Causal.map (Expr.unliftM1 (\t -> 1-t*t)) $* ramp dur


osci ::
   (MultiValue.Fraction t, Marshal.C t) =>
   (forall r. MultiValue.T t -> CodeGenFunction r y) ->
   Exp t -> Exp t -> Sig.T y
osci wave phase freq  =  Causal.map wave $* Core.osci phase freq


exponential2 ::
   (Marshal.C a) =>
   (MultiValue.Real a) =>
   (MultiValue.RationalConstant a) =>
   (MultiValue.Transcendental a) =>
   Exp a -> Exp a -> MV a
exponential2 halfLife  =  Core.exponential (1 / 2 ** recip halfLife)

exponentialBounded2 ::
   (Marshal.C a) =>
   (MultiValue.Real a) =>
   (MultiValue.RationalConstant a) =>
   (MultiValue.Transcendental a) =>
   Exp a -> Exp a -> Exp a -> MV a
exponentialBounded2 bound halfLife =
   Core.exponentialBounded bound (1 / 2 ** recip halfLife)


{- |
@noise seed rate@

The @rate@ parameter is for adjusting the amplitude
such that it is uniform across different sample rates
and after frequency filters.
The @rate@ is the ratio of the current sample rate to the default sample rate,
where the variance of the samples would be one.
If you want that at sample rate 22050 the variance is 1,
then in order to get a consistent volume at sample rate 44100
you have to set @rate = 2@.

I use the variance as quantity and not the amplitude,
because the amplitude makes only sense for uniformly distributed samples.
However, frequency filters transform the probabilistic density of the samples
towards the normal distribution according to the central limit theorem.
-}
noise ::
   (Marshal.C a, MultiValue.Transcendental a, MultiValue.RationalConstant a,
    MultiValue.NativeFloating a ar) =>
   Exp Word32 -> Exp a -> MV a
noise seed rate =
   let m2 = Expr.fromInteger' $ div Rnd.modulus 2
       r = sqrt (3 * rate) / m2
   in  Causal.map (Expr.unliftM1 (\y -> r * (int31tofp y - (m2+1)))) $*
       Core.noise seed

{-
sitofp is a single instruction on x86
and thus we use it, since the arguments are below 2^31.
-}
int31tofp ::
   (MultiValue.NativeFloating a ar) =>
   Exp Word32 -> Exp a
int31tofp =
   Expr.liftM
      (MultiValue.fromIntegral <=<
       (MultiValue.liftM LLVM.bitcast ::
         MultiValue.T Word32 -> CodeGenFunction r (MultiValue.T Int32)))


adjacentNodes02 ::
   (Memory.C a) =>
   Sig.T a -> Sig.T (Interpolation.Nodes02 a)
adjacentNodes02 =
   tail
   .
   apply
      (Causal.mapAccum
         (\new old -> return (Interpolation.Nodes02 old new, new))
         (return Tuple.undef))

adjacentNodes13 ::
   (Marshal.C a, MultiValue.T a ~ al) =>
   Exp a -> Sig.T al -> Sig.T (Interpolation.Nodes13 al)
adjacentNodes13 yp0 =
   tail .
   tail .
   apply
      (Causal.mapAccum
         (\new (x0, x1, x2) ->
            return (Interpolation.Nodes13 x0 x1 x2 new, (x1, x2, new)))
         (do
            y0 <- Expr.unExp yp0
            return (MultiValue.undef, MultiValue.undef, y0)))


{- |
Stretch signal in time by a certain factor.

This can be used for doing expensive computations
of filter parameters at a lower rate.
Alternatively, we could provide an adaptive @map@
that recomputes output values only if the input value changes,
or if the input value differs from the last processed one by a certain amount.
-}
interpolateConstant ::
   (Memory.C a, Marshal.C b, MultiValue.IntegerConstant b,
    MultiValue.Additive b, MultiValue.Comparison b) =>
   Exp b -> Sig.T a -> Sig.T a
interpolateConstant k sig =
   CausalC.toSignal (Causal.quantizeLift (CausalC.fromSignal sig) $< constant k)
