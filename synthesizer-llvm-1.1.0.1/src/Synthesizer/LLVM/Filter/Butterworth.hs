{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.LLVM.Filter.Butterworth (
   parameter, parameterCausal, Cascade.ParameterValue,
   Cascade.causal, Cascade.causalPacked,
   Cascade.fixSize,
   ) where

import qualified Synthesizer.LLVM.Filter.SecondOrderCascade as Cascade
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Private as Sig

import qualified Synthesizer.Plain.Filter.Recursive.Butterworth as Butterworth
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

import qualified Algebra.Transcendental as Trans

import NumericPrelude.Numeric
import NumericPrelude.Base



parameterCausal ::
   (TypeNum.Positive (n :*: LLVM.SizeOf (Marshal.Struct a)),
    TypeNum.Natural (n :*: LLVM.UnknownSize),
    TypeNum.Natural n, Trans.C a,
    Marshal.C a, MultiValue.RationalConstant a, MultiValue.Transcendental a) =>
   Proxy n -> Passband ->
   Causal.T (MultiValue.T a, MultiValue.T a) (Cascade.ParameterValue n a)
parameterCausal n kind =
   Causal.map
      (\((psine, ps), (ratio, freq)) ->
         parameterCore n kind psine ps ratio freq)
   $<
   Sig.zipWith (curry return) Sig.alloca Sig.alloca

parameter ::
   (TypeNum.Positive (n :*: LLVM.SizeOf (Marshal.Struct a)),
    TypeNum.Natural (n :*: LLVM.UnknownSize),
    TypeNum.Natural n, Trans.C a,
    Marshal.C a, MultiValue.RationalConstant a, MultiValue.Transcendental a) =>
   Proxy n -> Passband -> MultiValue.T a -> MultiValue.T a ->
   LLVM.CodeGenFunction r (Cascade.ParameterValue n a)
parameter n kind ratio freq = do
   psine <- LLVM.malloc
   ps <- LLVM.malloc
   pv <- parameterCore n kind psine ps ratio freq
   LLVM.free ps
   LLVM.free psine
   return pv

parameterCore ::
   (TypeNum.Positive (n :*: LLVM.SizeOf (Marshal.Struct a)),
    TypeNum.Natural (n :*: LLVM.UnknownSize),
    TypeNum.Natural n, Trans.C a,
    Marshal.C a, MultiValue.RationalConstant a, MultiValue.Transcendental a) =>
   Proxy n -> Passband ->
   LLVM.Value (LLVM.Ptr (Marshal.Struct (MultiValue.Array n a))) ->
   LLVM.Value (LLVM.Ptr (Cascade.ParameterStruct n a)) ->
   MultiValue.T a -> MultiValue.T a ->
   LLVM.CodeGenFunction r (Cascade.ParameterValue n a)
parameterCore n kind psine ps ratio freq = do
   let order = 2 * TypeNum.integralFromProxy n
   partialRatio <- Expr.unliftM1 (Butterworth.partialRatio order) ratio
   let evalSines :: (Trans.C a) => mv a -> Int -> [a]
       evalSines _ = Butterworth.makeSines
   let sines = Cascade.constArray n $ evalSines freq order
   Memory.store sines psine
   s <- LLVM.getElementPtr0 psine (LLVM.valueOf (0::Word), ())
   p <- LLVM.getElementPtr0 ps (LLVM.valueOf (0::Word), ())
   let len = LLVM.valueOf (TypeNum.integralFromProxy n :: Word)
   _ <- U.arrayLoop len p s $ \ptri si -> do
      sinw <- Memory.load si
      flip Memory.store ptri =<<
         Filt2.composeParameterMV =<<
         Expr.unliftM3 (Butterworth.partialParameter kind)
            partialRatio sinw freq
      A.advanceArrayElementPtr si
   fmap Cascade.ParameterValue $ Memory.load ps
