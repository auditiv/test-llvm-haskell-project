{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Represent a vector of Stereo values in two vectors
that store the values in an interleaved way.
That is:

> vector0[0] = left[0]
> vector0[1] = right[0]
> vector0[2] = left[1]
> vector0[3] = right[1]
> vector1[0] = left[2]
> vector1[1] = right[2]
> vector1[2] = left[3]
> vector1[3] = right[3]

This representation is not very useful for computation,
but necessary as intermediate representation for interfacing with memory.
SSE/SSE2 have the instructions UNPACK(L|H)P(S|D) that interleave efficiently.
-}
module Synthesizer.LLVM.Frame.StereoInterleavedCode (
   T,
   Value,
   interleave,
   deinterleave,
   fromMono,
   assemble, dissect,
   zero,
   scale,
   amplify,
   envelope,
   ) where

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Core as LLVM
import LLVM.Core (Vector)

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Foreign.Storable as St
import Foreign.Ptr (Ptr, castPtr)

import qualified Control.Applicative.HT as AppHT
import Control.Applicative (liftA2, pure)

import qualified Data.Foldable as Fold
import Data.Tuple.HT (mapPair)

import qualified Algebra.Additive as Additive


data T n a = Cons (Vector n a) (Vector n a)

type Value n a = MultiValue.T (T n a)


withSize :: (TypeNum.Natural n) => (Int -> m (Value n a)) -> m (Value n a)
withSize =
   let sz ::
          (TypeNum.Natural n) =>
          TypeNum.Singleton n -> (Int -> m (Value n a)) -> m (Value n a)
       sz n f = f (TypeNum.integralFromSingleton n)
   in  sz TypeNum.singleton


interleave ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Stereo.T (Serial.Value n a) ->
   LLVM.CodeGenFunction r (Value n a)
interleave x =
   assemble . map Stereo.unMultiValue
      =<< Serial.dissect (Stereo.multiValueSerial x)

deinterleave ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a ->
   LLVM.CodeGenFunction r (Stereo.T (Serial.Value n a))
deinterleave v =
   Stereo.unMultiValueSerial <$>
      (Serial.assemble . map Stereo.multiValue =<< dissect v)

fromMono ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Serial.Value n a ->
   LLVM.CodeGenFunction r (Value n a)
fromMono x =
   assemble . map pure =<< Serial.dissect x

assemble ::
   (TypeNum.Positive n, MultiVector.C a) =>
   [Stereo.T (MultiValue.T a)] -> LLVM.CodeGenFunction r (Value n a)
assemble x =
   withSize $ \n ->
      uncurry (liftA2 merge) .
      mapPair (MultiVector.assemble, MultiVector.assemble) .
      splitAt n .
      concatMap Fold.toList $ x

dissect ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Value n a -> LLVM.CodeGenFunction r [Stereo.T (MultiValue.T a)]
dissect v =
   let (v0,v1) = split v in
   fmap
      (let aux (l:r:xs) = Stereo.cons l r : aux xs
           aux [] = []
           aux _ = error "odd number of stereo elements"
       in  aux) $
   liftA2 (++)
      (MultiVector.dissect v0)
      (MultiVector.dissect v1)


merge :: MultiVector.T n a -> MultiVector.T n a -> MultiValue.T (T n a)
merge (MultiVector.Cons a) (MultiVector.Cons b) = MultiValue.Cons (a,b)

split :: MultiValue.T (T n a) -> (MultiVector.T n a, MultiVector.T n a)
split (MultiValue.Cons (a,b)) = (MultiVector.Cons a, MultiVector.Cons b)

merge_ ::
   MultiValue.T (Vector n a) -> MultiValue.T (Vector n a) ->
   MultiValue.T (T n a)
merge_ (MultiValue.Cons a) (MultiValue.Cons b) = MultiValue.Cons (a,b)

split_ ::
   MultiValue.T (T n a) ->
   (MultiValue.T (Vector n a), MultiValue.T (Vector n a))
split_ (MultiValue.Cons (a,b)) = (MultiValue.Cons a, MultiValue.Cons b)

instance (TypeNum.Positive n, MultiVector.C a) => MultiValue.C (T n a) where
   type Repr (T n a) = (MultiVector.Repr n a, MultiVector.Repr n a)
   cons (Cons v0 v1) = merge (MultiVector.cons v0) (MultiVector.cons v1)
   undef = merge MultiVector.undef MultiVector.undef
   zero = merge MultiVector.zero MultiVector.zero
   phi bb =
      fmap (uncurry merge) .
      AppHT.mapPair (MultiVector.phi bb, MultiVector.phi bb) . split
   addPhi bb a b =
      case (split a, split b) of
         ((a0,a1), (b0,b1)) -> do
            MultiVector.addPhi bb a0 b0
            MultiVector.addPhi bb a1 b1

instance (Marshal.Vector n a) => Marshal.C (T n a) where
   pack (Cons v0 v1) = Marshal.pack (v0,v1)
   unpack = uncurry Cons . Marshal.unpack

instance
   (TypeNum.Positive n, MultiVector.C a, St.Storable a) =>
      St.Storable (T n a) where
   sizeOf ~(Cons v0 v1) = St.sizeOf v0 + St.sizeOf v1
   alignment ~(Cons v _) = St.alignment v
   peek ptr =
      let p = castPtr ptr
      in  liftA2 Cons
             (St.peekElemOff p 0)
             (St.peekElemOff p 1)
   poke ptr (Cons v0 v1) =
      let p = castPtr ptr
      in  St.pokeElemOff p 0 v0 >>
          St.pokeElemOff p 1 v1

instance (TypeNum.Positive n, Storable.Vector a) => Storable.C (T n a) where
   load ptrV = do
      ptr <- castHalfPtr ptrV
      liftA2 merge_
         (Storable.load ptr)
         (Storable.load =<< Storable.incrementPtr ptr)
   store v ptrV = do
      let (v0,v1) = split_ v
      ptr <- castHalfPtr ptrV
      Storable.storeNext v0 ptr >>= Storable.store v1

castHalfPtr ::
   LLVM.Value (Ptr (T n a)) ->
   LLVM.CodeGenFunction r (LLVM.Value (Ptr (Vector n a)))
castHalfPtr = LLVM.bitcast


{- |
This instance allows to run @arrange@ on interleaved stereo vectors.
-}
instance
   (TypeNum.Positive n, MultiVector.Additive a) =>
      MultiValue.Additive (T n a) where
   add = zipV merge A.add
   sub = zipV merge A.sub
   neg = mapV A.neg


zero :: (TypeNum.Positive n, Additive.C a) => T n a
zero = Cons (pure Additive.zero) (pure Additive.zero)


scale ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   MultiValue.T a -> Value n a -> LLVM.CodeGenFunction r (Value n a)
scale a v = do
   av <- MultiVector.replicate a
   mapV (A.mul av) v

amplify ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   a -> Value n a -> LLVM.CodeGenFunction r (Value n a)
amplify a = scale (MultiValue.cons a)

envelope ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   Serial.Value n a -> Value n a -> LLVM.CodeGenFunction r (Value n a)
envelope e a =
   zipV merge (flip A.mul) a =<< fromMono e


mapV :: (Applicative m) =>
   (MultiVector.T n a -> m (MultiVector.T n a)) ->
   Value n a -> m (Value n a)
mapV f x =
   case split x of
      (x0,x1) -> uncurry merge <$> liftA2 (,) (f x0) (f x1)

zipV :: (Applicative m) =>
   (c -> c -> d) ->
   (MultiVector.T n a ->
    MultiVector.T n b ->
    m c) ->
   Value n a ->
   Value n b ->
   m d
zipV g f x y =
   case (split x, split y) of
      ((x0,x1), (y0,y1)) -> liftA2 g (f x0 y0) (f x1 y1)
