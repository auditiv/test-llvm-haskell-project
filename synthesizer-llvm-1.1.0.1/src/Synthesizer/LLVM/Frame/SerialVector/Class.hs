{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{- |
A special vector type that represents a time-sequence of samples.
This way we can distinguish safely between LLVM vectors
used for parallel signals and pipelines and
those used for chunky processing of scalar signals.
For the chunky processing this data type allows us
to derive the factor from the type
that time constants have to be multiplied with.
-}
module Synthesizer.LLVM.Frame.SerialVector.Class (
   Constant(Constant), constant,

   Read, Element, ReadIt, extract, readStart, readNext,
   Write, WriteIt, insert, writeStart, writeNext, writeStop,
   Zero, writeZero,
   Iterator(Iterator), ReadIterator, WriteIterator, ReadMode, WriteMode,

   Sized, Size, size, sizeOfIterator, withSize,

   insertTraversable, extractTraversable,
   readStartTraversable, readNextTraversable,
   writeStartTraversable, writeNextTraversable, writeStopTraversable,
   writeZeroTraversable,

   dissect, assemble, modify,
   upsample, subsample, last,
   iterate, reverse,
   shiftUp, shiftUpMultiZero, shiftDownMultiZero,
   ) where

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as SerialCode
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Data.Word (Word32)

import qualified Control.Monad.Trans.State as MS
import qualified Control.Applicative as App
import Control.Monad (foldM, replicateM, (<=<))
import Control.Applicative (liftA2, liftA3, (<$>))

import qualified Data.Traversable as Trav
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapSnd, fst3, snd3, thd3)

import Prelude hiding (Read, replicate, reverse, iterate, last)



newtype Constant n a = Constant a

constant :: (TypeNum.Positive n) => a -> Constant n a
constant = Constant

instance Functor (Constant n) where
   fmap f (Constant a) = Constant (f a)

instance App.Applicative (Constant n) where
   pure = Constant
   Constant f <*> Constant a = Constant (f a)

instance (Tuple.Phi a) => Tuple.Phi (Constant n a) where
   phi bb (Constant a) = Constant <$> Tuple.phi bb a
   addPhi bb (Constant a) (Constant b) = Tuple.addPhi bb a b

instance (Tuple.Undefined a) => Tuple.Undefined (Constant n a) where
   undef = Tuple.undefPointed



instance (TypeNum.Positive n) => Sized (Constant n a) where
   type Size (Constant n a) = n

instance
   (TypeNum.Positive n, Tuple.Phi a, Tuple.Undefined a) =>
      Read (Constant n a) where

   type Element (Constant n a) = a
   type ReadIt (Constant n a) = a

   extract _k (Constant a) = return a

   readStart (Constant a) = return $ Iterator a
   readNext it@(Iterator a) = return (a, it)



newtype Iterator mode it v = Iterator {unIterator :: it}
   deriving (Tuple.Undefined)

instance Tuple.Phi it => Tuple.Phi (Iterator mode it v) where
   phi bb (Iterator x) = fmap Iterator $ Tuple.phi bb x
   addPhi bb (Iterator x) (Iterator y) = Tuple.addPhi bb x y


type ReadIterator = Iterator ReadMode
type WriteIterator = Iterator WriteMode

data ReadMode
data WriteMode


instance (Memory.C it) => Memory.C (Iterator mode it v) where
   type Struct (Iterator mode it v) = Memory.Struct it
   load = Memory.loadNewtype Iterator
   store = Memory.storeNewtype (\(Iterator v) -> v)
   decompose = Memory.decomposeNewtype Iterator
   compose = Memory.composeNewtype (\(Iterator v) -> v)


fmapIt ::
   (ita -> itb) -> (va -> vb) ->
   Iterator mode ita va -> Iterator mode itb vb
fmapIt f _ (Iterator a) = Iterator (f a)


combineIt2 ::
   Iterator mode xa va -> Iterator mode xb vb ->
   Iterator mode (xa,xb) (va,vb)
combineIt2 (Iterator va) (Iterator vb) = Iterator (va,vb)

combineIt3 ::
   Iterator mode xa va -> Iterator mode xb vb -> Iterator mode xc vc ->
   Iterator mode (xa,xb,xc) (va,vb,vc)
combineIt3 (Iterator va) (Iterator vb) (Iterator vc) = Iterator (va,vb,vc)

combineItFunctor ::
   (Functor f) => f (Iterator mode x v) -> Iterator mode (f x) (f v)
combineItFunctor = Iterator . fmap unIterator

sequenceItFunctor ::
   (Functor f) => Iterator mode (f it) (f v) -> f (Iterator mode it v)
sequenceItFunctor = fmap Iterator . unIterator


withSize :: Sized v => (Int -> m v) -> m v
withSize =
   let sz :: (Sized v) => TypeNum.Singleton (Size v) -> (Int -> m v) -> m v
       sz n f = f (TypeNum.integralFromSingleton n)
   in  sz TypeNum.singleton

size :: (Sized v, Integral i) => v -> i
size =
   let sz :: (Sized v, Integral i) => TypeNum.Singleton (Size v) -> v -> i
       sz n _ = TypeNum.integralFromSingleton n
   in  sz TypeNum.singleton

sizeOfIterator :: (Sized v, Integral i) => Iterator mode it v -> i
sizeOfIterator =
   let sz :: (Sized v, Integral i) =>
               TypeNum.Singleton (Size v) -> Iterator mode it v -> i
       sz n _ = TypeNum.integralFromSingleton n
   in  sz TypeNum.singleton


{- |
The type parameter @v@ shall be a @MultiVector@ or @MultiValue Serial@
or a wrapper around one or more such things sharing the same size.
-}
class (TypeNum.Positive (Size v)) => Sized v where
   type Size v

class
   (Sized v,
    Tuple.Phi (ReadIt v), Tuple.Undefined (ReadIt v),
    Tuple.Phi v, Tuple.Undefined v) =>
      Read v where

   type Element v
   type ReadIt v

   extract :: LLVM.Value Word32 -> v -> LLVM.CodeGenFunction r (Element v)

   dissect :: v -> LLVM.CodeGenFunction r [Element v]
   dissect x = mapM (flip extract x . LLVM.valueOf) (take (size x) [0..])

   readStart :: v -> LLVM.CodeGenFunction r (ReadIterator (ReadIt v) v)
   readNext ::
      ReadIterator (ReadIt v) v ->
      LLVM.CodeGenFunction r (Element v, ReadIterator (ReadIt v) v)

class
   (Read v, Tuple.Phi (WriteIt v), Tuple.Undefined (WriteIt v)) =>
      Write v where
   type WriteIt v

   insert :: LLVM.Value Word32 -> Element v -> v -> LLVM.CodeGenFunction r v

   assemble :: [Element v] -> LLVM.CodeGenFunction r v
   assemble =
      foldM (\v (k,x) -> insert (LLVM.valueOf k) x v) Tuple.undef . zip [0..]

   writeStart :: LLVM.CodeGenFunction r (WriteIterator (WriteIt v) v)
   writeNext ::
      Element v -> WriteIterator (WriteIt v) v ->
      LLVM.CodeGenFunction r (WriteIterator (WriteIt v) v)
   writeStop :: WriteIterator (WriteIt v) v -> LLVM.CodeGenFunction r v

class (Write v, Tuple.Phi (WriteIt v), Tuple.Zero (WriteIt v)) => Zero v where
   -- initializes the target with zeros
   -- you may only call 'writeStop' on the result of 'writeZero'
   writeZero :: LLVM.CodeGenFunction r (WriteIterator (WriteIt v) v)



instance (TypeNum.Positive n) => Sized (MultiVector.T n a) where
   type Size (MultiVector.T n a) = n

instance (TypeNum.Positive n, MultiVector.C a) => Read (MultiVector.T n a) where

   type Element (MultiVector.T n a) = MultiValue.T a
   type ReadIt (MultiVector.T n a) = MultiVector.T n a

   extract = MultiVector.extract

   readStart v = return $ Iterator v
   readNext (Iterator v) =
      mapSnd Iterator <$> MultiVector.shiftDown MultiValue.undef v

instance
      (TypeNum.Positive n, MultiVector.C a) => Write (MultiVector.T n a) where

   type WriteIt (MultiVector.T n a) = MultiVector.T n a

   insert = MultiVector.insert

   writeStart = return (Iterator MultiVector.undef)
   writeNext x (Iterator v) = Iterator . snd <$> MultiVector.shiftDown x v
   writeStop (Iterator v) = return v

instance (TypeNum.Positive n, MultiVector.C a) => Zero (MultiVector.T n a) where
   writeZero = return (Iterator Tuple.zero)



type Serial n a = SerialCode.Value n a

instance (TypeNum.Positive n) => Sized (Serial n a) where
   type Size (Serial n a) = n

instance (TypeNum.Positive n, MultiVector.C a) => Read (Serial n a) where

   type Element (Serial n a) = MultiValue.T a
   type ReadIt (Serial n a) = Serial n a

   extract = SerialCode.extract

   readStart v = return $ Iterator v
   readNext (Iterator v) =
      mapSnd Iterator <$> SerialCode.shiftDown MultiValue.undef v

instance (TypeNum.Positive n, MultiVector.C a) => Write (Serial n a) where

   type WriteIt (Serial n a) = Serial n a

   insert = SerialCode.insert

   writeStart = return (Iterator Tuple.undef)
   writeNext x (Iterator v) = Iterator . snd <$> SerialCode.shiftDown x v
   writeStop (Iterator v) = return v

instance (TypeNum.Positive n, MultiVector.C a) => Zero (Serial n a) where
   writeZero = return (Iterator Tuple.zero)



instance (Sized va, Sized vb, Size va ~ Size vb) => Sized (va, vb) where
   type Size (va, vb) = Size va

instance (Read va, Read vb, Size va ~ Size vb) => Read (va, vb) where

   type Element (va, vb) = (Element va, Element vb)
   type ReadIt (va, vb) = (ReadIt va, ReadIt vb)

   extract k (va,vb) = liftA2 (,) (extract k va) (extract k vb)

   readStart (va,vb) = liftA2 combineIt2 (readStart va) (readStart vb)
   readNext it = do
      (a, ita) <- readNext $ fmapIt fst fst it
      (b, itb) <- readNext $ fmapIt snd snd it
      return ((a,b), combineIt2 ita itb)

instance (Write va, Write vb, Size va ~ Size vb) => Write (va, vb) where

   type WriteIt (va, vb) = (WriteIt va, WriteIt vb)

   insert k (a,b) (va,vb) =
      liftA2 (,)
         (insert k a va)
         (insert k b vb)

   writeStart = liftA2 combineIt2 writeStart writeStart
   writeNext (a,b) it =
      liftA2 combineIt2
         (writeNext a $ fmapIt fst fst it)
         (writeNext b $ fmapIt snd snd it)
   writeStop it =
      liftA2 (,)
         (writeStop (fmapIt fst fst it))
         (writeStop (fmapIt snd snd it))

instance (Zero va, Zero vb, Size va ~ Size vb) => Zero (va, vb) where
   writeZero = liftA2 combineIt2 writeZero writeZero


instance
   (Sized va, Sized vb, Sized vc, Size va ~ Size vb, Size vb ~ Size vc) =>
      Sized (va, vb, vc) where
   type Size (va, vb, vc) = Size va

instance
   (Read va, Read vb, Read vc, Size va ~ Size vb, Size vb ~ Size vc) =>
      Read (va, vb, vc) where

   type Element (va, vb, vc) = (Element va, Element vb, Element vc)
   type ReadIt (va, vb, vc) = (ReadIt va, ReadIt vb, ReadIt vc)

   extract k (va,vb,vc) =
      liftA3 (,,)
         (extract k va)
         (extract k vb)
         (extract k vc)

   readStart (va,vb,vc) =
      liftA3 combineIt3 (readStart va) (readStart vb) (readStart vc)
   readNext it = do
      (a, ita) <- readNext $ fmapIt fst3 fst3 it
      (b, itb) <- readNext $ fmapIt snd3 snd3 it
      (c, itc) <- readNext $ fmapIt thd3 thd3 it
      return ((a,b,c), combineIt3 ita itb itc)


instance
   (Write va, Write vb, Write vc, Size va ~ Size vb, Size vb ~ Size vc) =>
      Write (va, vb, vc) where

   type WriteIt (va, vb, vc) = (WriteIt va, WriteIt vb, WriteIt vc)

   insert k (a,b,c) (va,vb,vc) =
      liftA3 (,,)
         (insert k a va)
         (insert k b vb)
         (insert k c vc)

   writeStart = liftA3 combineIt3 writeStart writeStart writeStart
   writeNext (a,b,c) it =
      liftA3 combineIt3
         (writeNext a $ fmapIt fst3 fst3 it)
         (writeNext b $ fmapIt snd3 snd3 it)
         (writeNext c $ fmapIt thd3 thd3 it)
   writeStop it =
      liftA3 (,,)
         (writeStop (fmapIt fst3 fst3 it))
         (writeStop (fmapIt snd3 snd3 it))
         (writeStop (fmapIt thd3 thd3 it))

instance
   (Zero va, Zero vb, Zero vc, Size va ~ Size vb, Size vb ~ Size vc) =>
      Zero (va, vb, vc) where

   writeZero = liftA3 combineIt3 writeZero writeZero writeZero


instance (Sized value) => Sized (Stereo.T value) where
   type Size (Stereo.T value) = Size value

instance (Read v) => Read (Stereo.T v) where

   type Element (Stereo.T v) = Stereo.T (Element v)
   type ReadIt (Stereo.T v) = Stereo.T (ReadIt v)

   extract = extractTraversable

   readStart = readStartTraversable
   readNext = readNextTraversable

instance (Write v) => Write (Stereo.T v) where

   type WriteIt (Stereo.T v) = Stereo.T (WriteIt v)

   insert = insertTraversable

   writeStart = writeStartTraversable
   writeNext = writeNextTraversable
   writeStop = writeStopTraversable

instance (Zero v) => Zero (Stereo.T v) where

   writeZero = writeZeroTraversable


insertTraversable ::
   (Write v, Trav.Traversable f, App.Applicative f) =>
   LLVM.Value Word32 -> f (Element v) -> f v -> LLVM.CodeGenFunction r (f v)
insertTraversable n a v =
   Trav.sequence (liftA2 (insert n) a v)

extractTraversable ::
   (Read v, Trav.Traversable f) =>
   LLVM.Value Word32 -> f v -> LLVM.CodeGenFunction r (f (Element v))
extractTraversable n v =
   Trav.mapM (extract n) v


readStartTraversable ::
   (Trav.Traversable f, App.Applicative f, Read v) =>
   f v -> LLVM.CodeGenFunction r (ReadIterator (f (ReadIt v)) (f v))
readNextTraversable ::
   (Trav.Traversable f, App.Applicative f, Read v) =>
   ReadIterator (f (ReadIt v)) (f v) ->
   LLVM.CodeGenFunction r (f (Element v), ReadIterator (f (ReadIt v)) (f v))

readStartTraversable v =
   fmap combineItFunctor $ Trav.mapM readStart v

readNextTraversable it = do
   st <- Trav.mapM readNext $ sequenceItFunctor it
   return (fmap fst st, combineItFunctor $ fmap snd st)


writeStartTraversable ::
   (Trav.Traversable f, App.Applicative f, Write v) =>
   LLVM.CodeGenFunction r (WriteIterator (f (WriteIt v)) (f v))
writeNextTraversable ::
   (Trav.Traversable f, App.Applicative f, Write v) =>
   f (Element v) -> WriteIterator (f (WriteIt v)) (f v) ->
   LLVM.CodeGenFunction r (WriteIterator (f (WriteIt v)) (f v))
writeStopTraversable ::
   (Trav.Traversable f, App.Applicative f, Write v) =>
   WriteIterator (f (WriteIt v)) (f v) -> LLVM.CodeGenFunction r (f v)
writeZeroTraversable ::
   (Trav.Traversable f, App.Applicative f, Zero v) =>
   LLVM.CodeGenFunction r (WriteIterator (f (WriteIt v)) (f v))

writeStartTraversable =
   fmap combineItFunctor $ Trav.sequence $ App.pure writeStart

writeNextTraversable x it =
   fmap combineItFunctor $ Trav.sequence $
   liftA2 writeNext x $ sequenceItFunctor it

writeStopTraversable = Trav.mapM writeStop . sequenceItFunctor

writeZeroTraversable =
   fmap combineItFunctor $ Trav.sequence $ App.pure writeZero


modify ::
   (Write v, Element v ~ a) =>
   LLVM.Value Word32 ->
   (a -> LLVM.CodeGenFunction r a) ->
   v -> LLVM.CodeGenFunction r v
modify k f v = flip (insert k) v =<< f =<< extract k v


last :: (Read v) => v -> LLVM.CodeGenFunction r (Element v)
last v = extract (LLVM.valueOf (size v - 1 :: Word32)) v

subsample :: (Read v) => v -> LLVM.CodeGenFunction r (Element v)
subsample v = extract (A.zero :: LLVM.Value Word32) v

-- this will be translated to an efficient pshufd
upsample :: (Write v) => Element v -> LLVM.CodeGenFunction r v
upsample x = withSize $ \n -> assemble $ List.replicate n x


iterate ::
   (Write v) =>
   (Element v -> LLVM.CodeGenFunction r (Element v)) ->
   Element v -> LLVM.CodeGenFunction r v
iterate f x =
   withSize $ \n ->
      assemble =<<
      (flip MS.evalStateT x $
       replicateM n $
       MS.StateT $ \x0 -> do x1 <- f x0; return (x0,x1))

reverse ::
   (Write v) =>
   v -> LLVM.CodeGenFunction r v
reverse =
   assemble . List.reverse <=< dissect

shiftUp ::
   (Write v) =>
   Element v -> v -> LLVM.CodeGenFunction r (Element v, v)
shiftUp x v =
   ListHT.switchR
      (return (x,v))
      (\ys0 y -> fmap ((,) y) $ assemble (x:ys0))
   =<<
   dissect v


shiftUpMultiZero ::
   (Write v, A.Additive (Element v)) =>
   Int -> v -> LLVM.CodeGenFunction r v
shiftUpMultiZero n v =
   assemble . take (size v) . (List.replicate n A.zero ++) =<< dissect v

shiftDownMultiZero ::
   (Write v, A.Additive (Element v)) =>
   Int -> v -> LLVM.CodeGenFunction r v
shiftDownMultiZero n v =
   assemble . take (size v) . (++ List.repeat A.zero) . List.drop n
      =<< dissect v
