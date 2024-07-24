{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Exponential curve with controllable delay.
-}
module Synthesizer.LLVM.Causal.Exponential2 (
   Parameter,
   parameter,
   parameterPlain,
   multiValueParameter,
   unMultiValueParameter,
   causal,

   ParameterPacked,
   parameterPacked,
   parameterPackedExp,
   parameterPackedPlain,
   multiValueParameterPacked,
   unMultiValueParameterPacked,
   causalPacked,
   ) where

import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Functional as F
import qualified Synthesizer.LLVM.Frame.SerialVector.Plain as SerialPlain
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as SerialCode
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as SerialOld
import qualified Synthesizer.LLVM.Value as Value

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as MarshalMV
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.ScalarOrVector as SoV
import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction, Value, IsFloating)

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Foreign.Storable.Traversable as Store
import qualified Foreign.Storable
import Foreign.Storable (Storable)

import qualified Control.Applicative as App
import Control.Applicative (liftA2, pure, (<*>))
import Control.Arrow (arr, (&&&))

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import Data.Traversable (traverse)

import qualified Algebra.Transcendental as Trans

import NumericPrelude.Numeric
import NumericPrelude.Base


newtype Parameter a = Parameter a
   deriving (Show, Storable)


instance Functor Parameter where
   {-# INLINE fmap #-}
   fmap f (Parameter k) = Parameter (f k)

instance App.Applicative Parameter where
   {-# INLINE pure #-}
   pure x = Parameter x
   {-# INLINE (<*>) #-}
   Parameter f <*> Parameter k = Parameter (f k)

instance Fold.Foldable Parameter where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable Parameter where
   {-# INLINE sequenceA #-}
   sequenceA (Parameter k) = fmap Parameter k


instance (Tuple.Phi a) => Tuple.Phi (Parameter a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Parameter a) where
   undef = Tuple.undefPointed

instance Tuple.Zero a => Tuple.Zero (Parameter a) where
   zero = Tuple.zeroPointed

instance (Memory.C a) => Memory.C (Parameter a) where
   type Struct (Parameter a) = Memory.Struct a
   load = Memory.loadNewtype Parameter
   store = Memory.storeNewtype (\(Parameter k) -> k)
   decompose = Memory.decomposeNewtype Parameter
   compose = Memory.composeNewtype (\(Parameter k) -> k)

instance (Marshal.C a) => Marshal.C (Parameter a) where
   pack (Parameter k) = Marshal.pack k
   unpack = Parameter . Marshal.unpack

instance (MarshalMV.C a) => MarshalMV.C (Parameter a) where
   pack (Parameter k) = MarshalMV.pack k
   unpack = Parameter . MarshalMV.unpack

instance (Storable.C a) => Storable.C (Parameter a) where
   load = Storable.loadNewtype Parameter Parameter
   store = Storable.storeNewtype Parameter (\(Parameter k) -> k)

instance (Tuple.Value a) => Tuple.Value (Parameter a) where
   type ValueOf (Parameter a) = Parameter (Tuple.ValueOf a)
   valueOf = Tuple.valueOfFunctor

instance (MultiValue.C a) => MultiValue.C (Parameter a) where
   type Repr (Parameter a) = Parameter (MultiValue.Repr a)
   cons = multiValueParameter . fmap MultiValue.cons
   undef = multiValueParameter $ pure MultiValue.undef
   zero = multiValueParameter $ pure MultiValue.zero
   phi bb =
      fmap multiValueParameter .
      traverse (MultiValue.phi bb) . unMultiValueParameter
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb)
         (unMultiValueParameter a) (unMultiValueParameter b)

multiValueParameter ::
   Parameter (MultiValue.T a) -> MultiValue.T (Parameter a)
multiValueParameter = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValueParameter ::
   MultiValue.T (Parameter a) -> Parameter (MultiValue.T a)
unMultiValueParameter (MultiValue.Cons x) = fmap MultiValue.Cons x


instance (Value.Flatten a) => Value.Flatten (Parameter a) where
   type Registers (Parameter a) = Parameter (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable


instance (Vector.Simple v) => Vector.Simple (Parameter v) where
   type Element (Parameter v) = Parameter (Vector.Element v)
   type Size (Parameter v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (Parameter v) where
   insert  = Vector.insertTraversable


instance
   (Expr.Aggregate exp mv) =>
      Expr.Aggregate (Parameter exp) (Parameter mv) where
   type MultiValuesOf (Parameter exp) = Parameter (Expr.MultiValuesOf exp)
   type ExpressionsOf (Parameter mv) = Parameter (Expr.ExpressionsOf mv)
   bundle (Parameter p) = fmap Parameter $ Expr.bundle p
   dissect (Parameter p) = Parameter $ Expr.dissect p


parameter ::
   (Trans.C a, SoV.TranscendentalConstant a, IsFloating a) =>
   Value a ->
   CodeGenFunction r (Parameter (Value a))
parameter = Value.unlift1 parameterPlain

parameterPlain ::
   (Trans.C a) =>
   a -> Parameter a
parameterPlain halfLife =
   Parameter $ 0.5 ^? recip halfLife


causal ::
   (MarshalMV.C a, MultiValue.T a ~ am, MultiValue.PseudoRing a) =>
   Exp a -> Causal.T (Parameter am) am
causal initial =
   Causal.loop initial
      (arr snd &&& CausalPriv.zipWith (\(Parameter a) -> A.mul a))


data ParameterPacked a =
   ParameterPacked {ppFeedback, ppCurrent :: a}


instance Functor ParameterPacked where
   {-# INLINE fmap #-}
   fmap f p = ParameterPacked
      (f $ ppFeedback p) (f $ ppCurrent p)

instance App.Applicative ParameterPacked where
   {-# INLINE pure #-}
   pure x = ParameterPacked x x
   {-# INLINE (<*>) #-}
   f <*> p = ParameterPacked
      (ppFeedback f $ ppFeedback p)
      (ppCurrent f $ ppCurrent p)

instance Fold.Foldable ParameterPacked where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

instance Trav.Traversable ParameterPacked where
   {-# INLINE sequenceA #-}
   sequenceA p =
      liftA2 ParameterPacked
         (ppFeedback p) (ppCurrent p)


instance (Tuple.Phi a) => Tuple.Phi (ParameterPacked a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (ParameterPacked a) where
   undef = Tuple.undefPointed

instance Tuple.Zero a => Tuple.Zero (ParameterPacked a) where
   zero = Tuple.zeroPointed


{-
storeParameter ::
   Storable a => Store.Dictionary (ParameterPacked a)
storeParameter =
   Store.run $
   liftA2 ParameterPacked
      (Store.element ppFeedback)
      (Store.element ppCurrent)

instance Storable a => Storable (ParameterPacked a) where
   sizeOf    = Store.sizeOf storeParameter
   alignment = Store.alignment storeParameter
   peek      = Store.peek storeParameter
   poke      = Store.poke storeParameter
-}

instance Storable a => Storable (ParameterPacked a) where
   sizeOf    = Store.sizeOf
   alignment = Store.alignment
   peek      = Store.peekApplicative
   poke      = Store.poke


type ParameterPackedStruct a = LLVM.Struct (a, (a, ()))

memory ::
   (Memory.C a) =>
   Memory.Record r (ParameterPackedStruct (Memory.Struct a)) (ParameterPacked a)
memory =
   liftA2 ParameterPacked
      (Memory.element ppFeedback TypeNum.d0)
      (Memory.element ppCurrent  TypeNum.d1)

instance (Memory.C a) => Memory.C (ParameterPacked a) where
   type Struct (ParameterPacked a) = ParameterPackedStruct (Memory.Struct a)
   load = Memory.loadRecord memory
   store = Memory.storeRecord memory
   decompose = Memory.decomposeRecord memory
   compose = Memory.composeRecord memory

instance (Marshal.C a) => Marshal.C (ParameterPacked a) where
   pack (ParameterPacked bend depth) = Marshal.pack (bend, depth)
   unpack = uncurry ParameterPacked . Marshal.unpack

instance (MarshalMV.C a) => MarshalMV.C (ParameterPacked a) where
   pack (ParameterPacked bend depth) = MarshalMV.pack (bend, depth)
   unpack = uncurry ParameterPacked . MarshalMV.unpack

instance (Storable.C a) => Storable.C (ParameterPacked a) where
   load = Storable.loadApplicative
   store = Storable.storeFoldable


instance (Tuple.Value a) => Tuple.Value (ParameterPacked a) where
   type ValueOf (ParameterPacked a) = ParameterPacked (Tuple.ValueOf a)
   valueOf = Tuple.valueOfFunctor

instance (MultiValue.C a) => MultiValue.C (ParameterPacked a) where
   type Repr (ParameterPacked a) = ParameterPacked (MultiValue.Repr a)
   cons = multiValueParameterPacked . fmap MultiValue.cons
   undef = multiValueParameterPacked $ pure MultiValue.undef
   zero = multiValueParameterPacked $ pure MultiValue.zero
   phi bb =
      fmap multiValueParameterPacked .
      traverse (MultiValue.phi bb) . unMultiValueParameterPacked
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb)
         (unMultiValueParameterPacked a) (unMultiValueParameterPacked b)

multiValueParameterPacked ::
   ParameterPacked (MultiValue.T a) -> MultiValue.T (ParameterPacked a)
multiValueParameterPacked = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValueParameterPacked ::
   MultiValue.T (ParameterPacked a) -> ParameterPacked (MultiValue.T a)
unMultiValueParameterPacked (MultiValue.Cons x) = fmap MultiValue.Cons x


instance (Value.Flatten a) => Value.Flatten (ParameterPacked a) where
   type Registers (ParameterPacked a) = ParameterPacked (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable

instance
   (Expr.Aggregate exp mv) =>
      Expr.Aggregate (ParameterPacked exp) (ParameterPacked mv) where
   type MultiValuesOf (ParameterPacked exp) =
            ParameterPacked (Expr.MultiValuesOf exp)
   type ExpressionsOf (ParameterPacked mv) =
            ParameterPacked (Expr.ExpressionsOf mv)
   bundle p =
      liftA2 ParameterPacked
         (Expr.bundle $ ppFeedback p) (Expr.bundle $ ppCurrent p)
   dissect p =
      ParameterPacked
         (Expr.dissect $ ppFeedback p) (Expr.dissect $ ppCurrent p)


type instance F.Arguments f (ParameterPacked a) = f (ParameterPacked a)
instance F.MakeArguments (ParameterPacked a) where
   makeArgs = id



withSize ::
   (TypeNum.Natural n) =>
   (SerialOld.Write v, SerialOld.Size v ~ n, TypeNum.Positive n) =>
   (TypeNum.Singleton n -> m (param v)) ->
   m (param v)
withSize f = f TypeNum.singleton

parameterPacked ::
   (SerialOld.Write v, SerialOld.Element v ~ a,
    A.PseudoRing v, A.RationalConstant v,
    A.Transcendental a, A.RationalConstant a) =>
   a -> CodeGenFunction r (ParameterPacked v)
parameterPacked halfLife = withSize $ \n -> do
   feedback <-
      SerialOld.upsample =<<
      A.pow (A.fromRational' 0.5) =<<
      A.fdiv (A.fromInteger' $ TypeNum.integralFromSingleton n) halfLife
   k <-
      A.pow (A.fromRational' 0.5) =<<
      A.fdiv (A.fromInteger' 1) halfLife
   current <-
      SerialOld.iterate (A.mul k) (A.fromInteger' 1)
   return $ ParameterPacked feedback current
{-
   Value.unlift1 parameterPackedPlain
-}

withSizePlain ::
   (TypeNum.Positive n) =>
   (TypeNum.Singleton n -> param (Serial.T n a)) ->
   param (Serial.T n a)
withSizePlain f = f TypeNum.singleton

parameterPackedPlain ::
   (TypeNum.Positive n, Trans.C a) =>
   a -> ParameterPacked (Serial.T n a)
parameterPackedPlain halfLife =
   withSizePlain $ \n ->
   ParameterPacked
      (SerialPlain.replicate
         (0.5 ^? (fromInteger (TypeNum.integerFromSingleton n) / halfLife)))
      (SerialPlain.iterate (0.5 ^? recip halfLife *) one)

withSizeExp ::
   (TypeNum.Positive n) =>
   (TypeNum.Singleton n -> param (exp (Serial.T n a))) ->
   param (exp (Serial.T n a))
withSizeExp f = f TypeNum.singleton

parameterPackedExp ::
   (TypeNum.Positive n) =>
   (MultiValue.Transcendental a, MultiValue.RationalConstant a) =>
   (MultiVector.C a) =>
   Exp a -> ParameterPacked (Exp (Serial.T n a))
parameterPackedExp halfLife =
   withSizeExp $ \n ->
   ParameterPacked
      (Serial.upsample
         (0.5 ^? (fromInteger (TypeNum.integerFromSingleton n) / halfLife)))
      (Serial.iterate (0.5 ^? recip halfLife *) one)


causalPacked ::
   (MultiVector.PseudoRing a, MultiValue.IntegerConstant a,
    TypeNum.Positive n, MarshalMV.Vector n a, MarshalMV.C a) =>
   Exp a ->
   Causal.T (ParameterPacked (SerialCode.Value n a)) (SerialCode.Value n a)
causalPacked initial =
   Causal.loop
      (Serial.upsample initial)
      (CausalPriv.map $
       \(p, s0) -> liftA2 (,)
          (A.mul (ppCurrent p) s0)
          (A.mul (ppFeedback p) s0))
