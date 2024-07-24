{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Signal generators that generate the signal in chunks
that can be processed natively by the processor.
Some of the functions for plain signals can be re-used without modification.
E.g. rendering a signal and reading from and to signals work
because the vector type as element type warrents correct alignment.
We can convert between atomic and chunked signals.

The article
<http://perilsofparallel.blogspot.com/2008/09/larrabee-vs-nvidia-mimd-vs-simd.html>
explains the difference between Vector and SIMD computing.
According to that the SSE extensions in Intel processors
must be called Vector computing.
But since we use the term Vector already in the mathematical sense,
I like to use the term "packed" that is used in Intel mnemonics like mulps.
-}
module Synthesizer.LLVM.Generator.SignalPacked (
   pack, packRotate,
   packSmall,
   unpack, unpackRotate,
   constant,
   exponential2,
   exponentialBounded2,
   osciCore,
   osci,
   parabolaFadeInInf, parabolaFadeOutInf,
   rampInf, rampSlope,
   noise,
   noiseCore, noiseCoreAlt,
   ) where

import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Private as Priv
import qualified Synthesizer.LLVM.Generator.Core as Core
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as SerialClass
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as SerialCode
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Random as Rnd

import Synthesizer.Causal.Class (($*))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value.Vector as MultiValueVec
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as Maybe
import qualified LLVM.Extra.Control as U
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal ((:*:))

import qualified LLVM.Core as LLVM

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import Control.Monad.HT ((<=<))
import Control.Monad (replicateM)
import Control.Applicative ((<$>))

import qualified Algebra.Ring as Ring

import Data.Tuple.HT (mapSnd)
import Data.Word (Word32, Word)
import Data.Int (Int32)

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Convert a signal of scalar values into one using processor vectors.
If the signal length is not divisible by the chunk size,
then the last chunk is dropped.
-}
pack, packRotate ::
   (SerialClass.Write v, a ~ SerialClass.Element v) =>
   Sig.T a -> Sig.T v
pack = packRotate

packRotate (Priv.Cons next start stop) = Priv.Cons
   (\global local s -> do
      wInit <- Maybe.lift $ SerialClass.writeStart
      (w2,_,s2) <-
         Maybe.fromBool $
         U.whileLoop
            (LLVM.valueOf True,
             (wInit,
              LLVM.valueOf $ (SerialClass.sizeOfIterator wInit :: Word),
              s))
            (\(cont,(_w0,i0,_s0)) ->
               A.and cont =<<
                  A.cmp LLVM.CmpGT i0 A.zero)
            (\(_,(w0,i0,s0)) -> Maybe.toBool $ do
               (a,s1) <- next global local s0
               Maybe.lift $ do
                  w1 <- SerialClass.writeNext a w0
                  i1 <- A.dec i0
                  return (w1,i1,s1))
      v <- Maybe.lift $ SerialClass.writeStop w2
      return (v, s2))
   start
   stop

{-
We could reformulate it in terms of WriteIterator
that accesses elements using LLVM.extract.
We might move the loop counter into the Iterator,
but we have to assert that the counter is not duplicated.

packIndex ::
   (SerialClass.Write v, a ~ SerialClass.Element v) =>
   Sig.T a -> Sig.T v
packIndex = alter (\(Core next start stop) -> Core
   (\param s -> do
      (v2,_,s2) <-
         Maybe.fromBool $
         U.whileLoop
            (LLVM.valueOf True, (Tuple.undef, A.zero, s))
            (\(cont,(v0,i0,_s0)) ->
               A.and cont =<<
                  A.cmp LLVM.CmpLT i0 (LLVM.valueOf $ SerialClass.size v0))
            (\(_,(v0,i0,s0)) -> Maybe.toBool $ do
               (a,s1) <- next param s0
               Maybe.lift $ do
                  v1 <- Vector.insert i0 a v0
                  i1 <- A.inc i0
                  return (v1,i1,s1))
      return (v2, s2))
   start
   stop)
-}


{- |
Like 'pack' but duplicates the code for creating elements.
That is, for vectors of size n, the code of the input signal
will be emitted n times.
This is efficient only for simple input generators.
-}
packSmall ::
   (SerialClass.Write v, a ~ SerialClass.Element v) =>
   Sig.T a -> Sig.T v
packSmall (Priv.Cons next start stop) = Priv.Cons
   (\global local ->
      MS.runStateT $
      SerialClass.withSize $ \n ->
         MT.lift . Maybe.lift . SerialClass.assemble
         =<<
         replicateM n (MS.StateT $ next global local))
   start
   stop


unpack, unpackRotate ::
   (SerialClass.Read v, a ~ SerialClass.Element v,
    SerialClass.ReadIt v ~ itv, Memory.C itv) =>
   Sig.T v -> Sig.T a
unpack = unpackRotate

unpackRotate (Priv.Cons next start stop) = Priv.Cons
   (\global local (i0,r0,s0) -> do
      endOfVector <-
         Maybe.lift $ A.cmp LLVM.CmpEQ i0 (LLVM.valueOf (0::Word))
      (i2,r2,s2) <-
         Maybe.fromBool $
         U.ifThen endOfVector (LLVM.valueOf True, (i0,r0,s0)) $ do
            (cont1, (v1,s1)) <- Maybe.toBool $ next global local s0
            r1 <- SerialClass.readStart v1
            return (cont1, (LLVM.valueOf $ SerialClass.size v1, r1, s1))
      Maybe.lift $ do
         (a,r3) <- SerialClass.readNext r2
         i3 <- A.dec i2
         return (a, (i3,r3,s2)))
   (mapSnd (\s -> (A.zero, Tuple.undef, s)) <$> start)
   stop


{-
We could reformulate it in terms of ReadIterator
that accesses elements using LLVM.extract.
We might move the loop counter into the Iterator,
but we have to assert that the counter is not duplicated.

unpackIndex ::
   (SerialClass.Write v, a ~ SerialClass.Element v, Memory.C v) =>
   Sig.T v -> Sig.T a
unpackIndex = alter (\(Core next start stop) -> Core
   (\param (i0,v0,s0) -> do
      endOfVector <-
         Maybe.lift $ A.cmp LLVM.CmpGE i0 (LLVM.valueOf $ SerialClass.size v0)
      (i2,v2,s2) <-
         Maybe.fromBool $
         U.ifThen endOfVector (LLVM.valueOf True, (i0,v0,s0)) $ do
            (cont1, (v1,s1)) <- Maybe.toBool $ next param s0
            return (cont1, (A.zero, v1, s1))
      Maybe.lift $ do
         a <- Vector.extract i2 v2
         i3 <- A.inc i2
         return (a, (i3,v2,s2)))
   (\p -> do
      s <- start p
      let v = Tuple.undef
      return (LLVM.valueOf $ SerialClass.size v, v, s))
   stop)
-}



type Serial n a = SerialCode.Value n a

withSize ::
   (TypeNum.Positive n) =>
   (TypeNum.Singleton n -> Sig.T (Serial n a)) ->
   Sig.T (Serial n a)
withSize f = f TypeNum.singleton

withSizeRing ::
   (Ring.C b, TypeNum.Positive n) =>
   (b -> Sig.T (Serial n a)) ->
   Sig.T (Serial n a)
withSizeRing f =
   withSize $ f . fromInteger . TypeNum.integerFromSingleton


constant ::
   (Marshal.Vector n a) =>
   Exp a -> Sig.T (Serial n a)
constant = Sig.constant . Serial.upsample


exponential2 ::
   (Marshal.Vector n a, MultiVector.Transcendental a,
    MultiValue.RationalConstant a) =>
   Exp a -> Exp a -> Sig.T (Serial n a)
exponential2 halfLife start = withSizeRing $ \n ->
   Core.exponential
      (Serial.upsample (0.5 ** (n / halfLife)))
      (Serial.iterate (0.5 ** recip halfLife *) start)

exponentialBounded2 ::
   (Marshal.Vector n a, MultiVector.Transcendental a,
    MultiValue.RationalConstant a,
    MultiVector.IntegerConstant a, MultiVector.Real a) =>
   Exp a -> Exp a -> Exp a -> Sig.T (Serial n a)
exponentialBounded2 bound halfLife start = withSizeRing $ \n ->
   Core.exponentialBounded
      (Serial.upsample bound)
      (Serial.upsample (0.5 ** (n / halfLife)))
      (Serial.iterate (0.5 ** recip halfLife *) start)

osciCore ::
   (Marshal.Vector n t, MultiVector.PseudoRing t, MultiVector.Fraction t,
    MultiValue.IntegerConstant t) =>
   Exp t -> Exp t -> Sig.T (Serial n t)
osciCore phase freq = withSizeRing $ \n ->
   Core.osci
      (Serial.iterate (Expr.fraction . (freq +)) phase)
      (Serial.upsample (Expr.fraction (n * freq)))

osci ::
   (Marshal.Vector n t, MultiVector.PseudoRing t, MultiVector.Fraction t,
    MultiValue.IntegerConstant t) =>
   (forall r. Serial n t -> LLVM.CodeGenFunction r y) ->
   Exp t -> Exp t -> Sig.T y
osci wave phase freq = Priv.map wave $ osciCore phase freq


rampInf, rampSlope, parabolaFadeInInf, parabolaFadeOutInf ::
   (Marshal.Vector n a, MultiVector.Field a, MultiVector.IntegerConstant a,
    MultiValue.RationalConstant a) =>
   Exp a -> Sig.T (Serial n a)
rampSlope slope = withSizeRing $ \n ->
   Core.ramp
      (Serial.upsample (n * slope))
      (Serial.iterate (slope +) 0)
rampInf dur = rampSlope (Expr.recip dur)

parabolaFadeInInf dur = withSizeRing $ \n ->
   let d = n/dur
   in Core.parabola
         (Serial.upsample (-2*d*d))
         (Serial.iterate (subtract $ 2 / dur ^ 2) (d*(2-d)))
         ((\t -> t*(2-t)) $ Serial.iterate (recip dur +) 0)

parabolaFadeOutInf dur = withSizeRing $ \n ->
   let d = n/dur
   in Core.parabola
         (Serial.upsample (-2*d*d))
         (Serial.iterate (subtract $ 2 / dur ^ 2) (-d*d))
         ((\t -> 1-t*t) $ Serial.iterate (recip dur +) 0)


{- |
For the mysterious rate parameter see 'Sig.noise'.
-}
noise ::
   (MultiVector.NativeFloating n a ar) =>
   (MultiVector.PseudoRing a, MultiVector.IntegerConstant a) =>
   (MultiValue.Algebraic a, MultiValue.RationalConstant a) =>
   (TypeNum.Positive n, TypeNum.Positive (n :*: TypeNum.D32)) =>
   Exp Word32 -> Exp a -> Sig.T (Serial n a)
noise seed rate =
   let m2 = div Rnd.modulus 2
       r = Serial.upsample $ Expr.sqrt (3*rate) / Expr.fromInteger' m2
   in Causal.map
         (\y -> r * (Expr.liftM int31tofp y - Expr.fromInteger' (m2+1))) $*
      noiseCoreAlt seed

{-
sitofp is a single instruction on x86
and thus we use it, since the arguments are below 2^31.

It would be better to use LLVM's range annotation, instead.
-}
int31tofp ::
   (MultiVector.NativeFloating n a ar,
    TypeNum.Positive n, TypeNum.Positive (n :*: TypeNum.D32)) =>
   Serial n Word32 -> LLVM.CodeGenFunction r (Serial n a)
int31tofp =
   fmap SerialCode.fromOrdinary . MultiValueVec.fromIntegral .
   SerialCode.toOrdinary . forceInt32
      <=< MultiValue.liftM LLVM.bitcast

type Id a = a -> a

forceInt32 :: Id (Serial n Int32)
forceInt32 = id

noiseCore, noiseCoreAlt ::
   (TypeNum.Positive n, TypeNum.Positive (n :*: TypeNum.D32)) =>
   Exp Word32 -> Sig.T (Serial n Word32)
noiseCore    = Sig.iterate (Expr.liftReprM Rnd.nextVector)   . vectorSeed
noiseCoreAlt = Sig.iterate (Expr.liftReprM Rnd.nextVector64) . vectorSeed

vectorSeed :: (TypeNum.Positive n) => Exp Word32 -> Exp (Serial.T n Word32)
vectorSeed seed =
   Serial.iterate (Expr.liftReprM Rnd.nextCG) $
   Expr.irem seed (fromInteger Rnd.modulus - 1) + 1
