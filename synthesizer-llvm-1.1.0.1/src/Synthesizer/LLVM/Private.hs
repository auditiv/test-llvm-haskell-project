module Synthesizer.LLVM.Private where

import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Multi.Value as MultiValue

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Control.Applicative (liftA2)


unbool :: MultiValue.T Bool -> LLVM.Value Bool
unbool (MultiValue.Cons b) = b

noLocalPtr :: f -> (LLVM.Value (LLVM.Ptr (LLVM.Struct ())) -> f)
noLocalPtr = const

getPairPtrs ::
   (LLVM.IsSized a, LLVM.IsSized b) =>
   LLVM.Value (LLVM.Ptr (LLVM.Struct (a, (b, ())))) ->
   MaybeCont.T r c (LLVM.Value (LLVM.Ptr a), LLVM.Value (LLVM.Ptr b))
getPairPtrs ptr =
   MaybeCont.lift $
   liftA2 (,)
      (LLVM.getElementPtr0 ptr (TypeNum.d0, ()))
      (LLVM.getElementPtr0 ptr (TypeNum.d1, ()))
