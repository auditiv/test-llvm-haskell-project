{-# LANGUAGE TypeFamilies #-}
{- |
Very simple random number generator according to Knuth
which should be fast and should suffice for generating just noise.
<http://www.softpanorama.org/Algorithms/random_generators.shtml>
-}
module Synthesizer.LLVM.Random where

import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Extra.Vector as Vector

import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core.Guided as Guided
import LLVM.Core
          (CodeGenFunction, Value, Vector,
           zext, trunc, lshr, valueOf)
import qualified LLVM.Core as LLVM
import qualified Type.Data.Num.Decimal as TypeNum

import qualified Data.NonEmpty.Class as NonEmptyC
import Data.Function.HT (nest)

import Data.Int (Int32)
import Data.Word (Word32, Word64)


factor :: Integral a => a
factor = 40692

modulus :: Integral a => a
modulus = 2147483399 -- 2^31-249

{-
We have to split the 32 bit integer in order to avoid overflow on multiplication.
'split' must be chosen, such that 'splitRem' is below 2^16.
-}
split :: Word32
split = succ $ div modulus factor

splitRem :: Word32
splitRem = split * factor - modulus


{- |
efficient computation of @mod (s*factor) modulus@
without Integer or Word64, as in 'next64'.
-}
next :: Word32 -> Word32
next s =
   let (sHigh, sLow) = divMod s split
   in  flip mod modulus $
       splitRem*sHigh + factor*sLow

next64 :: Word32 -> Word32
next64 s =
   fromIntegral $
   flip mod modulus $
   factor * (fromIntegral s :: Word64)

nextCG32 :: Value Word32 -> CodeGenFunction r (Value Word32)
nextCG32 s = do
   sHigh <- A.mul (valueOf splitRem) =<< LLVM.idiv s (valueOf split)
   sLow  <- A.mul (valueOf factor)   =<< LLVM.irem s (valueOf split)
   flip A.irem (valueOf modulus) =<< A.add sHigh sLow

nextCG64 :: Value Word32 -> CodeGenFunction r (Value Word32)
nextCG64 s =
   trunc =<<
   {-
   This is slow on x86 since the native @div@ is not used
   since LLVM wants to prevent overflow.
   We know that there cannot be an overflow,
   but I do not know how to tell LLVM.
   -}
   flip A.irem (valueOf (modulus :: Word64)) =<<
   A.mul (valueOf factor) =<<
   zext s

nextCG :: Value Word32 -> CodeGenFunction r (Value Word32)
nextCG s = do
   x <- A.mul (valueOf $ factor :: Value Word64) =<< zext s
   {-
   split 64 result between bit 30 and bit 31
   we cannot split above bit 31,
   since then 'low' can be up to 2^32-1
   and then later addition overflows.
   -}
   let p2e31 = 2^(31::Int)
   low <- A.and (valueOf $ p2e31-1) =<< trunc x
   high <- trunc =<< flip lshr (valueOf (31 :: Word64)) x
   -- fac = mod (2^31) modulus
   let fac = p2e31 - modulus
   {-
   fac < 250
   high < factor
   fac*high < factor*250
   low < 2^31
   low + fac*high
      < 2^31 + factor*250
      < 2*modulus
   Thus modulo by modulus needs at most one subtraction.
   -}
   subtractIfPossible (valueOf modulus)
      =<< A.add low
      =<< A.mul (valueOf fac) high


{-
How to vectorise?
E.g. by repeated distribution of modulus and split at bit 31.
Can we replace div by modulus by mul with (2^31+249) ?
-}
vectorParameter ::
   Integral a =>
   Int -> a
vectorParameter n =
   fromIntegral $ nest n next 1

vectorSeed ::
   (TypeNum.Positive n) =>
   Word32 -> Vector n Word32
vectorSeed seed =
   LLVM.cyclicVector $ NonEmptyC.iterate next seed
-- vector $ NonEmptyC.iterate next seed

vector64 :: Value (Vector n Word64) -> Value (Vector n Word64)
vector64 = id

{-
In case of a vector random generator the factor depends on the vector size
and thus we cannot do optimizations on a constant factor as in nextCG.
Thus we just compute the product @factor*seed@ as is
(this is of type @Word32 -> Word32 -> Word64@)
and try to compute @urem@ without using LLVM's @urem@
that calls __umoddi3 on every element.
Instead we optimize on the constant modulus
and utilize that is slightly smaller than 2^31.

We split the product:
  factor*seed = high0*2^31 + low0

Now it is
mod (factor*seed) modulus
  = mod (high0*2^31 + low0) modulus
  = mod (high0 * mod (2^31) modulus + low0) modulus
  = mod (high0 * 249 + low0) modulus

However, high0 * 249 + low0 is still too big,
it can be up to (excluding) 2^31 * 250.
Thus we repeat the split
high0 * 249 + low0 = high1 * 2^31 + low1

It is high1 < 250, and thus high1*249 < 62500,
high1 * 249 + low1 < 2*modulus.
With x = high1 * 249 + low1
we have
mod (factor*seed) modulus
  = if x<modulus
      then x
      else x-modulus


An alternative approach would be to still multiply @let p = factor*seed@ exactly,
then do an approximate division @let q = approxdiv p modulus@,
then compute @p - q*modulus@ and
do a final adjustment in order to fix rounding errors.
The approximate division could be done by a floating point multiplication
or an integer multiplication with some shifting.
But in the end we will need at least the same number of multiplications
as in the approach that is implemented here.
-}
nextVector ::
   (TypeNum.Positive n) =>
   Value (Vector n Word32) ->
   CodeGenFunction r (Value (Vector n Word32))
nextVector s = do
   {-
   It seems that LLVM-2.6 on x86 does not make use of the fact,
   that the upper doublewords are zero.
   It seems to implement a full 64x64 multiplication in terms of pmuludq.
   -}
   (low0, high0) <-
      splitVector31 =<<
      umul32to64 (SoV.replicateOf (vectorParameter (Vector.size s))) s
   -- fac = mod (2^31) modulus
   let fac :: Integral a => a
       fac = 2^(31::Int) - modulus
   (low1, high1) <-
      splitVector31 =<<
      (\x -> A.add x =<< Vector.map zext low0) =<<
      umul32to64 (SoV.replicateOf fac) high0

   subtractIfPossible (SoV.replicateOf modulus)
      =<< A.add low1
      =<< Vector.mul (SoV.replicateOf fac) high1

{- |
@subtractIfPossible d x@ returns @A.sub x d@
if this is possible without underflow.
Otherwise it returns @x@.

Only works for unsigned types.
-}
subtractIfPossible ::
   (SoV.Real a) =>
   Value a -> Value a -> CodeGenFunction r (Value a)
subtractIfPossible d x = do
   {-
   An element should become smaller by subtraction.
   If it becomes greater, then there was an overflow
   and 'min' chooses the value before subtraction.
   -}
   SoV.min x =<< A.sub x d
   -- alternatively (slower):
   --   flip selectNonNegativeGeneric x =<< A.sub x d

{- |
Select non-negative elements from the first vector,
otherwise select corresponding elements from the second vector.
-}
selectNonNegativeGeneric ::
   (TypeNum.Positive n) =>
   Value (Vector n Int32) ->
   Value (Vector n Int32) ->
   CodeGenFunction r (Value (Vector n Int32))
selectNonNegativeGeneric x y = do
   b <- A.cmp LLVM.CmpGE x A.zero
   LLVM.select b x y


splitVector31 ::
   (TypeNum.Positive n) =>
   Value (Vector n Word64) ->
   CodeGenFunction r (Value (Vector n Word32), Value (Vector n Word32))
splitVector31 x = do
   low  <- A.and (SoV.replicateOf (2^(31::Int)-1)) =<< Vector.map trunc x
   high <- Vector.map trunc =<< flip lshr (SoV.replicateOf (31 :: Word64) `asTypeOf` x) x
   return (low, high)

{- |
This is the most obvious implementation
but unfortunately calls the expensive __umoddi3.
-}
nextVector64 ::
   (TypeNum.Positive n) =>
   Value (Vector n Word32) ->
   CodeGenFunction r (Value (Vector n Word32))
nextVector64 s =
   Vector.map trunc =<<
   flip A.irem (SoV.replicateOf modulus) =<<
   umul32to64 (SoV.replicateOf (vectorParameter (Vector.size s))) s

umul32to64 ::
   (TypeNum.Positive n) =>
   Value (Vector n Word32) ->
   Value (Vector n Word32) ->
   CodeGenFunction r (Value (Vector n Word64))
umul32to64 x y = do
   x64 <- Guided.ext Guided.vector x
   y64 <- Guided.ext Guided.vector y
   A.mul x64 y64
