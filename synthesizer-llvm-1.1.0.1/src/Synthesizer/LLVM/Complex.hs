{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Synthesizer.LLVM.Complex (
   Complex.T(Complex.real, Complex.imag),
   Struct,
   (+:),
   Complex.cis,
   Complex.scale,
   constOf, unfold,
   ) where

import qualified Synthesizer.LLVM.Value as Value

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM
import LLVM.Core (Value, ConstValue, IsConst)

import qualified Type.Data.Num.Decimal as TypeNum

import Control.Applicative (liftA2)

import qualified Number.Complex as Complex
import Number.Complex ((+:))


type Struct a = LLVM.Struct (a, (a, ()))

constOf :: IsConst a =>
   Complex.T a -> ConstValue (Struct a)
constOf x =
   LLVM.constStruct
      (LLVM.constOf $ Complex.real x,
        (LLVM.constOf $ Complex.imag x,
          ()))

unfold ::
   Value (Struct a) -> Complex.T (Value.T (Value a))
unfold x =
   Value.lift0 (LLVM.extractvalue x TypeNum.d0)
   +:
   Value.lift0 (LLVM.extractvalue x TypeNum.d1)


instance (Tuple.Undefined a) => Tuple.Undefined (Complex.T a) where
   undef = Tuple.undef +: Tuple.undef

instance (Tuple.Phi a) => Tuple.Phi (Complex.T a) where
   phi bb v =
      liftA2 (+:)
         (Tuple.phi bb (Complex.real v))
         (Tuple.phi bb (Complex.imag v))
   addPhi bb x y = do
      Tuple.addPhi bb (Complex.real x) (Complex.real y)
      Tuple.addPhi bb (Complex.imag x) (Complex.imag y)


memory ::
   (Memory.C l) =>
   Memory.Record r (Struct (Memory.Struct l)) (Complex.T l)
memory =
   liftA2 (+:)
      (Memory.element Complex.real TypeNum.d0)
      (Memory.element Complex.imag TypeNum.d1)

instance (Memory.C l) => Memory.C (Complex.T l) where
   type Struct (Complex.T l) = Struct (Memory.Struct l)
   load = Memory.loadRecord memory
   store = Memory.storeRecord memory
   decompose = Memory.decomposeRecord memory
   compose = Memory.composeRecord memory



instance (MultiValue.C a) => MultiValue.C (Complex.T a) where
   type Repr (Complex.T a) = Complex.T (MultiValue.Repr a)
   cons x =
      consMV
         (MultiValue.cons $ Complex.real x)
         (MultiValue.cons $ Complex.imag x)
   undef = consMV MultiValue.undef MultiValue.undef
   zero = consMV MultiValue.zero MultiValue.zero
   phi bb a =
      case deconsMV a of
         (a0,a1) -> liftA2 consMV (MultiValue.phi bb a0) (MultiValue.phi bb a1)
   addPhi bb a b =
      case (deconsMV a, deconsMV b) of
         ((a0,a1), (b0,b1)) ->
            MultiValue.addPhi bb a0 b0 >> MultiValue.addPhi bb a1 b1

consMV :: MultiValue.T a -> MultiValue.T a -> MultiValue.T (Complex.T a)
consMV (MultiValue.Cons a) (MultiValue.Cons b) = MultiValue.Cons (a+:b)

deconsMV :: MultiValue.T (Complex.T a) -> (MultiValue.T a, MultiValue.T a)
deconsMV (MultiValue.Cons x) =
   (MultiValue.Cons $ Complex.real x, MultiValue.Cons $ Complex.imag x)


instance (Marshal.C a) => Marshal.C (Complex.T a) where
   pack x =
      LLVM.consStruct
         (Marshal.pack $ Complex.real x)
         (Marshal.pack $ Complex.imag x)
   unpack = LLVM.uncurryStruct $ \a b -> Marshal.unpack a +: Marshal.unpack b
