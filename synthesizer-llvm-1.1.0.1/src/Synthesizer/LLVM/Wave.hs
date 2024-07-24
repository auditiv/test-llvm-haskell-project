{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Synthesizer.LLVM.Wave where

import qualified Synthesizer.LLVM.Value as Value

import qualified LLVM.Extra.Arithmetic as A

import LLVM.Core (CodeGenFunction)

import qualified Control.Monad.HT as M
import Control.Monad.HT ((<=<))

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (replicate)



saw ::
   (A.PseudoRing a, A.IntegerConstant a) =>
   a -> CodeGenFunction r a
saw =
   A.sub (A.fromInteger' 1) <=<
   A.mul (A.fromInteger' 2)

square ::
   (A.PseudoRing a, A.IntegerConstant a, A.Fraction a) =>
   a -> CodeGenFunction r a
square =
   A.sub (A.fromInteger' 1) <=<
   A.mul (A.fromInteger' 2) <=<
   A.truncate <=<
   A.mul (A.fromInteger' 2)

{- |
Discrete interpolation between triangle and square wave.
For exponent 1 we get a triangle wave.
The larger the exponent, the more we approach a square wave,
the.more computing is necessary.
-}
triangleSquarePower ::
   (A.PseudoRing a, A.RationalConstant a, A.Real a) =>
   Integer -> a -> CodeGenFunction r a
triangleSquarePower n = Value.unlift1 $ \x ->
   let y = 2-4*x
       z = abs (1-abs y)
   in  (1-z^n)*signum y

{- |
Continuous interpolation between triangle and square wave.
For factor 0 we get a square wave,
for factor 1 we get a triangle wave.
-}
triangleSquareRatio ::
   (A.Field a, A.RationalConstant a, A.Real a) =>
   a -> a -> CodeGenFunction r a
triangleSquareRatio = Value.unlift2 $ \c x ->
   let y = 2-4*x
       z = abs (1-abs y)
   in  (1-z)/(1+(c-1)*z)*signum y

triangle ::
   (A.PseudoRing a, A.RationalConstant a, A.Fraction a) =>
   a -> CodeGenFunction r a
triangle =
   flip A.sub (A.fromInteger' 1) <=<
   A.abs <=<
   flip A.sub (A.fromInteger' 2) <=<
   A.mul (A.fromInteger' 4) <=<
   A.incPhase (A.fromRational' 0.75)

approxSine2 ::
   (A.PseudoRing a, A.IntegerConstant a, A.Fraction a) =>
   a -> CodeGenFunction r a
approxSine2 t = do
   x <- saw t
   A.mul (A.fromInteger' 4) =<<
      A.mul x =<<
      A.sub (A.fromInteger' 1) =<<
      A.abs x

approxSine3 ::
   (A.PseudoRing a, A.RationalConstant a, A.Fraction a) =>
   a -> CodeGenFunction r a
approxSine3 t = do
   x <- triangle t
   A.mul (A.fromRational' 0.5) =<<
      A.mul x =<<
      A.sub (A.fromInteger' 3) =<<
      A.mul x x

approxSine4 ::
   (A.PseudoRing a, A.RationalConstant a, A.Real a) =>
   a -> CodeGenFunction r a
approxSine4 t = do
   x <- saw t
   ax <- A.abs x
   sax <- A.sub (A.fromInteger' 1) ax
   A.mul (A.fromRational' (16/5)) =<<
      A.mul x =<<
      A.mul sax =<<
      A.add (A.fromInteger' 1) =<<
      A.mul sax ax

{- |
For the distortion factor @recip pi@ you get the closest approximation
to an undistorted cosine or sine.
We have chosen this scaling in order to stay with field operations.
-}
rationalApproxCosine1, rationalApproxSine1 ::
   (A.Field a, A.RationalConstant a, A.Real a) =>
   a -> a -> CodeGenFunction r a
rationalApproxCosine1 k t = do
   num2 <-
      A.square =<<
      A.mul k =<<
      A.add (A.fromInteger' (-1)) =<<
      A.mul (A.fromInteger' 2) t
   den2 <-
      A.square =<<
      A.mul t =<<
      A.sub (A.fromInteger' 1) t
   M.liftJoin2 A.fdiv
      (A.sub num2 den2)
      (A.add num2 den2)

rationalApproxSine1 k t = do
   num <-
      A.mul k =<<
      A.add (A.fromInteger' (-1)) =<<
      A.mul (A.fromInteger' 2) t
   den <-
      A.mul t =<<
      A.sub (A.fromInteger' 1) t
   M.liftJoin2 A.fdiv
      (A.mul (A.fromInteger' (-2)) =<< A.mul num den)
      (M.liftJoin2 A.add (A.square num) (A.square den))


trapezoidSkew ::
   (A.Field a, A.RationalConstant a, A.Real a) =>
   a -> a -> CodeGenFunction r a
trapezoidSkew p =
   A.max (A.fromInteger' (-1)) <=<
   A.min (A.fromInteger' 1) <=<
   flip A.fdiv p <=<
   A.sub (A.fromInteger' 1) <=<
   A.mul (A.fromInteger' 2)

{- |
> trapezoidSlope steepness = trapezoidSkew (recip steepness)
-}
trapezoidSlope ::
   (A.PseudoRing a, A.RationalConstant a, A.Real a) =>
   a -> a -> CodeGenFunction r a
trapezoidSlope p =
   A.max (A.fromInteger' (-1)) <=<
   A.min (A.fromInteger' 1) <=<
   A.mul p <=<
   A.sub (A.fromInteger' 1) <=<
   A.mul (A.fromInteger' 2)

sine ::
   (A.Transcendental a, A.RationalConstant a) =>
   a -> CodeGenFunction r a
sine t =
   A.sin =<< A.mul t =<< Value.decons Value.tau



{- |
This can be used for preprocessing the phase
in order to generate locally faster oscillating waves.
For example

> triangle <=< replicate (valueOf 2.5)

shrinks a triangle wave such that 2.5 periods fit into one.
-}
replicate ::
   (A.PseudoRing a, A.RationalConstant a, A.Fraction a) =>
   a -> a -> CodeGenFunction r a
replicate k =
   A.fraction <=<
   A.mul k <=<
   flip A.sub (A.fromRational' 0.5) <=<
   A.incPhase (A.fromRational' 0.5)

{- |
Preprocess the phase such that the first half of a wave
is expanded to one period and shifted by 90 degree.
E.g.

> sine <=< halfEnvelope

generates a sequence of sine bows that starts and ends with the maximum.
Such a signal can be used to envelope an oscillation
generated using 'replicate'.
-}
halfEnvelope ::
   (A.PseudoRing a, A.RationalConstant a, A.Fraction a) =>
   a -> CodeGenFunction r a
halfEnvelope =
   A.mul (A.fromRational' 0.5) <=<
   A.incPhase (A.fromRational' 0.5)

partial ::
   (A.Fraction v, A.PseudoRing v, A.IntegerConstant v) =>
   (v -> CodeGenFunction r v) ->
   Int ->
   (v -> CodeGenFunction r v)
partial w n t =
   w =<<
   A.signedFraction =<<
   A.mul t (A.fromInteger' (fromIntegral n))
