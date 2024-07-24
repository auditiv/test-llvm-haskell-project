{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Synthesizer.LLVM.Frame.Binary (
   toCanonical,
   ) where

import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Core as LLVM

import qualified Algebra.ToInteger as ToInteger
import NumericPrelude.Numeric
import NumericPrelude.Base

import Prelude ()



toCanonical ::
   (LLVM.ShapeOf real ~ LLVM.ShapeOf int,
    LLVM.IsFloating real, SoV.IntegerConstant real,
    LLVM.IsInteger int, Bounded int, ToInteger.C int) =>
   LLVM.Value int -> LLVM.CodeGenFunction r (LLVM.Value real)
toCanonical i = do
   numer <- LLVM.inttofp i
   A.fdiv numer (A.fromInteger' (toInteger (maxBoundOf i)))

maxBoundOf :: (Bounded i) => LLVM.Value i -> i
maxBoundOf _ = maxBound
