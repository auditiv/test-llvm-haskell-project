{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Synthesizer.LLVM.Frame.SerialVector.Code (
   T(Cons), Value, size,
   fromOrdinary, toOrdinary,
   fromMultiVector, toMultiVector,
   extract, insert, modify,
   assemble, dissect,
   assemble1, dissect1,
   upsample, subsample, last,
   reverse, shiftUp, shiftUpMultiZero, shiftDown,
   cumulate, iterate,
   scale,
   ) where

import qualified LLVM.Extra.Multi.Vector.Instance as MultiVectorInst
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value.Vector as MultiValueVec
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Foreign.Storable as Store
import Foreign.Storable (Storable)
import Foreign.Ptr (castPtr)

import Control.Applicative ((<$>))

import qualified Data.NonEmpty as NonEmpty
import Data.Word (Word32)
import Data.Tuple.HT (mapSnd)

import Prelude as P hiding (last, reverse, iterate)


newtype T n a = Cons (LLVM.Vector n a)
   deriving (Eq, Num)

type Value n a = MultiValue.T (T n a)

instance (TypeNum.Positive n, MultiVector.C a) => MultiValue.C (T n a) where
   type Repr (T n a) = MultiVector.Repr n a
   cons (Cons v) = fromOrdinary $ MultiValue.cons v
   undef = fromOrdinary MultiValue.undef
   zero = fromOrdinary MultiValue.zero
   phi bb = fmap fromOrdinary . MultiValue.phi bb . toOrdinary
   addPhi bb a b = MultiValue.addPhi bb (toOrdinary a) (toOrdinary b)

instance (Marshal.Vector n a) => Marshal.C (T n a) where
   pack (Cons v) = Marshal.pack v
   unpack = Cons . Marshal.unpack

instance (TypeNum.Positive n, Storable a) => Storable (T n a) where
   sizeOf (Cons v) = Store.sizeOf v
   alignment (Cons v) = Store.alignment v
   poke ptr (Cons v) = Store.poke (castPtr ptr) v
   peek ptr = Cons <$> Store.peek (castPtr ptr)

instance
   (TypeNum.Positive n, Storable.Vector a, MultiVector.C a) =>
      Storable.C (T n a) where
   load ptr = fmap fromOrdinary $ Storable.load =<< LLVM.bitcast ptr
   store v ptr = Storable.store (toOrdinary v) =<< LLVM.bitcast ptr

instance
   (TypeNum.Positive n, MultiVector.IntegerConstant a) =>
      MultiValue.IntegerConstant (T n a) where
   fromInteger' = fromMultiVector . MultiVector.fromInteger'

instance
   (TypeNum.Positive n, MultiVector.RationalConstant a) =>
      MultiValue.RationalConstant (T n a) where
   fromRational' = fromMultiVector . MultiVector.fromRational'

instance
   (TypeNum.Positive n, MultiVector.Additive a) =>
      MultiValue.Additive (T n a) where
   add = lift2 MultiVector.add
   sub = lift2 MultiVector.sub
   neg = lift1 MultiVector.neg

instance
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
      MultiValue.PseudoRing (T n a) where
   mul = lift2 MultiVector.mul

scale ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   MultiValue.T a -> Value n a -> LLVM.CodeGenFunction r (Value n a)
scale = lift1 . MultiVector.scale

instance
   (TypeNum.Positive n, MultiVector.Real a) =>
      MultiValue.Real (T n a) where
   min = lift2 MultiVector.min
   max = lift2 MultiVector.max
   abs = lift1 MultiVector.abs
   signum = lift1 MultiVector.signum

instance
   (TypeNum.Positive n, MultiVector.Fraction a) =>
      MultiValue.Fraction (T n a) where
   truncate = lift1 MultiVector.truncate
   fraction = lift1 MultiVector.fraction

instance
   (TypeNum.Positive n, MultiVector.Field a) =>
      MultiValue.Field (T n a) where
   fdiv = lift2 MultiVector.fdiv

instance
   (TypeNum.Positive n, MultiVector.Algebraic a) =>
      MultiValue.Algebraic (T n a) where
   sqrt = lift1 MultiVector.sqrt

instance
   (TypeNum.Positive n, MultiVector.Transcendental a) =>
      MultiValue.Transcendental (T n a) where
   pi  = fmap fromMultiVector MultiVector.pi
   sin = lift1 MultiVector.sin
   log = lift1 MultiVector.log
   exp = lift1 MultiVector.exp
   cos = lift1 MultiVector.cos
   pow = lift2 MultiVector.pow

instance
   (TypeNum.Positive n, n ~ m,
    MultiVector.NativeInteger n a ar,
    MultiValue.NativeInteger a ar) =>
      MultiValueVec.NativeInteger (T n a) (LLVM.Vector m ar) where

instance
   (TypeNum.Positive n, n ~ m,
    MultiVector.NativeFloating n a ar,
    MultiValue.NativeFloating a ar) =>
      MultiValueVec.NativeFloating (T n a) (LLVM.Vector m ar) where

lift1 ::
   (Functor f) =>
   (MultiVector.T n a -> f (MultiVector.T m b)) ->
   (Value n a -> f (Value m b))
lift1 f a = fromMultiVector <$> f (toMultiVector a)

lift2 ::
   (Functor f) =>
   (MultiVector.T n a -> MultiVector.T m b -> f (MultiVector.T k c)) ->
   (Value n a -> Value m b -> f (Value k c))
lift2 f a b = fromMultiVector <$> f (toMultiVector a) (toMultiVector b)


extract ::
   (TypeNum.Positive n,
    MultiVector.C x, MultiValue.T x ~ a, Value n x ~ v) =>
   LLVM.Value Word32 -> v -> LLVM.CodeGenFunction r a
extract i v = MultiVector.extract i (toMultiVector v)

insert ::
   (TypeNum.Positive n,
    MultiVector.C x, MultiValue.T x ~ a, Value n x ~ v) =>
   LLVM.Value Word32 -> a -> v -> LLVM.CodeGenFunction r v
insert i a v =
    fromMultiVector <$> MultiVector.insert i a (toMultiVector v)

modify ::
   (TypeNum.Positive n,
    MultiVector.C x, MultiValue.T x ~ a, Value n x ~ v) =>
   LLVM.Value Word32 ->
   (a -> LLVM.CodeGenFunction r a) ->
   v -> LLVM.CodeGenFunction r v
modify k f v = flip (insert k) v =<< f =<< extract k v


assemble ::
   (TypeNum.Positive n, MultiVector.C a) =>
   [MultiValue.T a] ->
   LLVM.CodeGenFunction r (Value n a)
assemble = fmap fromMultiVector . MultiVector.assemble

dissect ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a ->
   LLVM.CodeGenFunction r [MultiValue.T a]
dissect = MultiVector.dissect . toMultiVector

assemble1 ::
   (TypeNum.Positive n, MultiVector.C a) =>
   NonEmpty.T [] (MultiValue.T a) ->
   LLVM.CodeGenFunction r (Value n a)
assemble1 = fmap fromMultiVector . MultiVector.assemble1

dissect1 ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a ->
   LLVM.CodeGenFunction r (NonEmpty.T [] (MultiValue.T a))
dissect1 = MultiVector.dissect1 . toMultiVector


sizeS :: TypeNum.Positive n => Value n a -> TypeNum.Singleton n
sizeS _ = TypeNum.singleton

size :: (TypeNum.Positive n, P.Integral i) => Value n a -> i
size = TypeNum.integralFromSingleton . sizeS


last ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a -> LLVM.CodeGenFunction r (MultiValue.T a)
last v = extract (LLVM.valueOf (size v - 1 :: Word32)) v

subsample ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a -> LLVM.CodeGenFunction r (MultiValue.T a)
subsample = extract (A.zero :: LLVM.Value Word32)

upsample ::
   (TypeNum.Positive n, MultiVector.C a) =>
   MultiValue.T a -> LLVM.CodeGenFunction r (Value n a)
upsample = fmap fromOrdinary . MultiValueVec.replicate


reverse ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a -> LLVM.CodeGenFunction r (Value n a)
reverse =
   fmap fromMultiVector . MultiVector.reverse . toMultiVector

shiftUp ::
   (TypeNum.Positive n, MultiVector.C x,
    MultiValue.T x ~ a, Value n x ~ v) =>
   a -> v -> LLVM.CodeGenFunction r (a, v)
shiftUp a v =
   mapSnd fromMultiVector <$> MultiVector.shiftUp a (toMultiVector v)

shiftUpMultiZero ::
   (TypeNum.Positive n, MultiVector.C x, Value n x ~ v) =>
   Int -> v -> LLVM.CodeGenFunction r v
shiftUpMultiZero k v =
   fromMultiVector <$> MultiVector.shiftUpMultiZero k (toMultiVector v)

shiftDown ::
   (TypeNum.Positive n, MultiVector.C x,
    MultiValue.T x ~ a, Value n x ~ v) =>
   a -> v -> LLVM.CodeGenFunction r (a, v)
shiftDown a v =
   mapSnd fromMultiVector <$> MultiVector.shiftDown a (toMultiVector v)


iterate ::
   (TypeNum.Positive n, MultiVector.C a) =>
   (MultiValue.T a -> LLVM.CodeGenFunction r (MultiValue.T a)) ->
   MultiValue.T a -> LLVM.CodeGenFunction r (Value n a)
iterate f = fmap fromOrdinary . MultiValueVec.iterate f

cumulate ::
   (TypeNum.Positive n, MultiVector.Additive a) =>
   MultiValue.T a -> Value n a ->
   LLVM.CodeGenFunction r (MultiValue.T a, Value n a)
cumulate a =
   fmap (mapSnd fromMultiVector) . MultiVector.cumulate a . toMultiVector


fromOrdinary :: MultiValue.T (LLVM.Vector n a) -> Value n a
fromOrdinary = MultiValue.cast

toOrdinary :: Value n a -> MultiValue.T (LLVM.Vector n a)
toOrdinary = MultiValue.cast

fromMultiVector :: MultiVector.T n a -> Value n a
fromMultiVector = fromOrdinary . MultiVectorInst.toMultiValue

toMultiVector :: Value n a -> MultiVector.T n a
toMultiVector = MultiVectorInst.fromMultiValue . toOrdinary
