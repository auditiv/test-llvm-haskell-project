{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Generator.Extra where

import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import Synthesizer.Causal.Class (($*))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue

import Data.Word (Word)

import NumericPrelude.Numeric



ramp,
 parabolaFadeIn, parabolaFadeOut,
 parabolaFadeInMap, parabolaFadeOutMap ::
   (Marshal.C a, MultiValue.Field a, MultiValue.IntegerConstant a,
    MultiValue.NativeFloating a ar) =>
   Exp Word -> Sig.MV a

ramp dur =
   Causal.take dur $* Sig.rampInf (Expr.fromIntegral dur)

parabolaFadeIn dur =
   Causal.take dur $* Sig.parabolaFadeInInf (Expr.fromIntegral dur)

parabolaFadeOut dur =
   Causal.take dur $* Sig.parabolaFadeOutInf (Expr.fromIntegral dur)

parabolaFadeInMap dur = Causal.map (\t -> t*(2-t)) $* ramp dur
parabolaFadeOutMap dur = Causal.map (\t -> 1-t*t) $* ramp dur
