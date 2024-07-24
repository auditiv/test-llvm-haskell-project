{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Synthesizer.LLVM.Filter.SecondOrderCascade (
   causal, causalPacked,
   Parameter,
   ParameterValue(..),
   ParameterStruct,
   fixSize, constArray,
   ) where

import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2

import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Private as Sig

import qualified Synthesizer.LLVM.Frame.SerialVector.Class as Serial
import Synthesizer.Causal.Class (($<))

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Memory as Memory

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal.Number ((:*:))
import Type.Base.Proxy (Proxy)

import Data.Word (Word)

import Control.Arrow ((<<<), (^<<), (&&&), arr)

import NumericPrelude.Base


type Parameter n a = MultiValue.Array n (Filt2.Parameter a)
type ParameterStruct n a = Marshal.Struct (Parameter n a)

newtype ParameterValue n a =
   ParameterValue {parameterValue :: MultiValue.T (Parameter n a)}
{-
Automatic deriving is not allowed even with GeneralizedNewtypeDeriving
because of IsSized constraint
and it would also be wrong for Functor and friends.
      deriving
         (Tuple.Phi, Tuple.Undefined, Tuple.Zero,
          Functor, App.Applicative, Fold.Foldable, Trav.Traversable)
-}

instance (TypeNum.Natural n, Marshal.C a) =>
      Tuple.Phi (ParameterValue n a) where
   phi bb (ParameterValue r) = fmap ParameterValue $ MultiValue.phi bb r
   addPhi bb (ParameterValue r) (ParameterValue r') = MultiValue.addPhi bb r r'

instance (TypeNum.Natural n, Marshal.C a) =>
      Tuple.Undefined (ParameterValue n a) where
   undef = ParameterValue MultiValue.undef

instance (TypeNum.Natural n, Marshal.C a) =>
      Tuple.Zero (ParameterValue n a) where
   zero = ParameterValue MultiValue.zero

instance (TypeNum.Natural n, Marshal.C a,
          TypeNum.Positive (n :*: LLVM.UnknownSize)) =>
      Memory.C (ParameterValue n a) where
   type Struct (ParameterValue n a) = ParameterStruct n a
   load = Memory.loadNewtype ParameterValue
   store = Memory.storeNewtype (\(ParameterValue k) -> k)
   decompose = Memory.decomposeNewtype ParameterValue
   compose = Memory.composeNewtype (\(ParameterValue k) -> k)

type instance Func.Arguments f (ParameterValue n a) = f (ParameterValue n a)
instance Func.MakeArguments (ParameterValue n a) where
   makeArgs = id


withSize ::
   (TypeNum.Natural n) =>
   (TypeNum.Singleton n -> process (ParameterValue n a, x) y) ->
   process (ParameterValue n a, x) y
withSize f = f TypeNum.singleton

fixSize ::
   Proxy n ->
   process (ParameterValue n a, x) y ->
   process (ParameterValue n a, x) y
fixSize _n = id

constArray ::
   (TypeNum.Natural n, Marshal.C a) =>
   Proxy n -> [a] -> MultiValue.T (MultiValue.Array n a)
constArray _n = MultiValue.cons . MultiValue.Array


causal ::
   (A.PseudoModule v, Memory.C v, A.Scalar v ~ MultiValue.T a,
    Marshal.C a, MultiValue.IntegerConstant a,
    TypeNum.Natural n, TypeNum.Positive (n :*: LLVM.UnknownSize)) =>
   Causal.T (ParameterValue n a, v) v
causal = causalGen Filt2.causal

causalPacked ::
   (Marshal.C a, MultiValue.PseudoRing a, MultiValue.IntegerConstant a,
    Serial.Write v, Serial.Element v ~ MultiValue.T a,
    Memory.C v, A.PseudoRing v, A.IntegerConstant v,
    TypeNum.Natural n, TypeNum.Positive (n :*: LLVM.UnknownSize)) =>
   Causal.T (ParameterValue n a, v) v
causalPacked = causalGen Filt2.causalPacked

causalGen ::
   (Marshal.C a, Tuple.Phi v, Tuple.Undefined v,
    TypeNum.Natural n, TypeNum.Positive (n :*: LLVM.UnknownSize)) =>
   Causal.T (Filt2.Parameter (MultiValue.T a), v) v ->
   Causal.T (ParameterValue n a, v) v
causalGen stage =
   withSize $ \n ->
      snd
      ^<<
      Causal.replicateControlled
         (TypeNum.integralFromSingleton n)
         (paramStage stage)
      <<<
      Causal.map
         (\(ptr, (p,v)) -> do
            Memory.store (parameterValue p) ptr
            return (ptr, (A.zero, v)))
      $<
      Sig.alloca

paramStage ::
   (TypeNum.Natural n, Marshal.C a) =>
   Causal.T (Filt2.Parameter (MultiValue.T a), v) v ->
   Causal.T
      (LLVM.Value (LLVM.Ptr (ParameterStruct n a)), (LLVM.Value Word, v))
      (LLVM.Value Word, v)
paramStage stage =
   let p = arr fst
       i = arr (fst.snd)
       v = arr (snd.snd)
   in  (Causal.map A.inc <<< i)
       &&&
       (stage <<<
           (Causal.zipWith getStageParameterGEP <<< p &&& i)
           &&&
           v)

getStageParameterGEP ::
   (TypeNum.Natural n,  Marshal.C a) =>
   LLVM.Value (LLVM.Ptr (ParameterStruct n a)) ->
   LLVM.Value Word ->
   LLVM.CodeGenFunction r (Filt2.Parameter (MultiValue.T a))
getStageParameterGEP ptr k =
   Filt2.decomposeParameterMV
    =<< LLVM.load
    =<< LLVM.getElementPtr0 ptr (k, ())
