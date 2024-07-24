{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.LLVM.Filter.Chebyshev (
   parameterCausalA, parameterCausalB,
   parameterA, parameterB, Cascade.ParameterValue,
   Cascade.causal,  Cascade.causalPacked,
   Cascade.fixSize,
   ) where

import qualified Synthesizer.LLVM.Filter.SecondOrderCascade as Cascade
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Private as Sig

import qualified Synthesizer.Plain.Filter.Recursive.Chebyshev as Chebyshev
import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2Core
import Synthesizer.Plain.Filter.Recursive (Passband)
import Synthesizer.Causal.Class (($<))

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Control as U

import qualified LLVM.Core as LLVM
import Data.Word (Word)

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal.Number ((:*:))
import Type.Base.Proxy (Proxy)

import qualified Synthesizer.LLVM.Complex as Complex

import Control.Applicative (liftA2)

import qualified Algebra.Transcendental as Trans

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
@n@ must be at least one in order to allow amplification
by the first partial filter.
The causal processes should be more efficient
than 'parameterA' and 'parameterB'
because they use stack-based @alloca@ instead of @malloc@.
-}
parameterCausalA, parameterCausalB ::
   (TypeNum.Natural n, Trans.C a,
    Marshal.C a, MultiValue.RationalConstant a, MultiValue.Transcendental a) =>
   (TypeNum.Positive (n :*: LLVM.SizeOf (Marshal.Struct a)),
    TypeNum.Positive (n :*: LLVM.UnknownSize)) =>
   Proxy n -> Passband ->
   Causal.T (MultiValue.T a, MultiValue.T a) (Cascade.ParameterValue n a)
parameterCausalA n kind =
   Causal.map
      (\((psine, ps), (ratio, freq)) ->
         fmap Cascade.ParameterValue $
         adjustAmplitude ratio =<<
         parameter Chebyshev.partialParameterA n kind psine ps ratio freq)
   $<
   allocaArrays

parameterCausalB n kind =
   Causal.map
      (\((psine, ps), (ratio, freq)) ->
         fmap Cascade.ParameterValue $
         parameter Chebyshev.partialParameterB n kind psine ps ratio freq)
   $<
   allocaArrays

allocaArrays ::
   (LLVM.IsSized a, LLVM.IsSized b) =>
   Sig.T (LLVM.Value (LLVM.Ptr a), LLVM.Value (LLVM.Ptr b))
allocaArrays = liftA2 (,) Sig.alloca Sig.alloca

parameterA, parameterB ::
   (TypeNum.Natural n, Trans.C a,
    Marshal.C a, MultiValue.RationalConstant a, MultiValue.Transcendental a) =>
   (TypeNum.Positive (n :*: LLVM.SizeOf (Marshal.Struct a)),
    TypeNum.Positive (n :*: LLVM.UnknownSize)) =>
   Proxy n -> Passband -> MultiValue.T a -> MultiValue.T a ->
   LLVM.CodeGenFunction r (Cascade.ParameterValue n a)
parameterA n kind ratio freq =
   withArrays $ \psine ps ->
      fmap Cascade.ParameterValue $
      adjustAmplitude ratio =<<
      parameter Chebyshev.partialParameterA n kind psine ps ratio freq

parameterB n kind ratio freq =
   withArrays $ \psine ps ->
      fmap Cascade.ParameterValue $
      parameter Chebyshev.partialParameterB n kind psine ps ratio freq

withArrays ::
   (LLVM.IsSized a, LLVM.IsSized b) =>
   (LLVM.Value (LLVM.Ptr a) -> LLVM.Value (LLVM.Ptr b) ->
    LLVM.CodeGenFunction r c) ->
   LLVM.CodeGenFunction r c
withArrays act = do
   psine <- LLVM.malloc
   ps <- LLVM.malloc
   x <- act psine ps
   LLVM.free psine
   LLVM.free ps
   return x


-- | adjust amplification of the first filter
adjustAmplitude ::
   (TypeNum.Natural n, Filt2.Parameter a ~ filt2,
    Marshal.C a, MultiValue.IntegerConstant a, MultiValue.PseudoRing a) =>
   MultiValue.T a -> MultiValue.T (MultiValue.Array n filt2) ->
   LLVM.CodeGenFunction r (MultiValue.T (MultiValue.Array n filt2))
adjustAmplitude ratio (MultiValue.Cons pv) = do
   filt0 <- Filt2.decomposeParameterMV =<< LLVM.extractvalue pv (0::Word)
   fmap MultiValue.Cons $
      flip (LLVM.insertvalue pv) (0::Word) =<<
      Filt2.composeParameterMV =<<
      Expr.unliftM2 Filt2Core.amplify ratio filt0

parameter ::
   (TypeNum.Positive (n :*: LLVM.SizeOf (Marshal.Struct a)),
    TypeNum.Positive (n :*: LLVM.UnknownSize),
    TypeNum.Natural n, Trans.C a,
    Marshal.C a, MultiValue.RationalConstant a, MultiValue.Transcendental a,
    Expr.Exp a ~ ae) =>
   (Passband -> Int -> ae -> Complex.T ae -> ae -> Filt2Core.Parameter ae) ->
   Proxy n -> Passband ->
   LLVM.Value (LLVM.Ptr (Marshal.Struct (MultiValue.Array n (Complex.T a)))) ->
   LLVM.Value (LLVM.Ptr (Cascade.ParameterStruct n a)) ->
   MultiValue.T a -> MultiValue.T a ->
   LLVM.CodeGenFunction r (MultiValue.T (Cascade.Parameter n a))
parameter partialParameter n kind psine ps ratio freq = do
   let order = TypeNum.integralFromProxy n
   let evalSines :: (Trans.C a) => mv a -> Int -> [Complex.T a]
       evalSines _ = Chebyshev.makeCirclePoints
   let sines = Cascade.constArray n $ evalSines freq order
   Memory.store sines psine
   s <- LLVM.getElementPtr0 psine (LLVM.valueOf (0::Word), ())
   p <- LLVM.getElementPtr0 ps (LLVM.valueOf (0::Word), ())
   let len = LLVM.valueOf (TypeNum.integralFromProxy n :: Word)
   _ <- U.arrayLoop len p s $ \ptri si -> do
      c <- Memory.load si
      flip Memory.store ptri =<<
         Filt2.composeParameterMV =<<
         Expr.unliftM3 (partialParameter kind order) ratio c freq
      A.advanceArrayElementPtr si

   Memory.load ps
