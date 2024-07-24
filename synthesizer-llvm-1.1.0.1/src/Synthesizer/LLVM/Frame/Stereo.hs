{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Re-export functions from "Sound.Frame.Stereo"
and add (orphan) instances for various LLVM type classes.
If you want to use the Stereo datatype with synthesizer-llvm
we recommend to import this module instead of
"Sound.Frame.Stereo" or "Sound.Frame.NumericPrelude.Stereo".
-}
module Synthesizer.LLVM.Frame.Stereo (
   Stereo.T, Stereo.cons, Stereo.left, Stereo.right,
   Stereo.Channel(Stereo.Left, Stereo.Right), Stereo.select,
   Stereo.swap,
   multiValue, unMultiValue, consMultiValue, unExpression,
   multiVector, unMultiVector,
   multiValueSerial, unMultiValueSerial,
   Stereo.arrowFromMono,
   Stereo.arrowFromMonoControlled,
   Stereo.arrowFromChannels,
   Stereo.interleave,
   Stereo.sequence,
   Stereo.liftApplicative,
   ) where

import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.Frame.Stereo as Stereo

import qualified LLVM.DSL.Expression as Expr
import qualified LLVM.DSL.Value as Value

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Storable as StorableMV
import qualified LLVM.Extra.Multi.Value.Marshal as MarshalMV
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Control as C
import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Core as LLVM

import Type.Data.Num.Decimal (d0, d1)

import Control.Applicative (liftA2, pure, (<$>))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import Prelude hiding (Either(Left, Right), sequence)


instance (Tuple.Zero a) => Tuple.Zero (Stereo.T a) where
   zero = Stereo.cons Tuple.zero Tuple.zero

instance (Tuple.Undefined a) => Tuple.Undefined (Stereo.T a) where
   undef = Stereo.cons Tuple.undef Tuple.undef

instance (C.Select a) => C.Select (Stereo.T a) where
   select = C.selectTraversable

instance (Tuple.Value h) => Tuple.Value (Stereo.T h) where
   type ValueOf (Stereo.T h) = Stereo.T (Tuple.ValueOf h)
   valueOf = fmap Tuple.valueOf

instance (Tuple.Phi a) => Tuple.Phi (Stereo.T a) where
   phi bb v =
      liftA2 Stereo.cons
         (Tuple.phi bb (Stereo.left v))
         (Tuple.phi bb (Stereo.right v))
   addPhi bb x y = do
      Tuple.addPhi bb (Stereo.left  x) (Stereo.left  y)
      Tuple.addPhi bb (Stereo.right x) (Stereo.right y)

instance (MultiValue.C a) => MultiValue.C (Stereo.T a) where
   type Repr (Stereo.T a) = Stereo.T (MultiValue.Repr a)
   cons = multiValue . fmap MultiValue.cons
   undef = multiValue $ pure MultiValue.undef
   zero = multiValue $ pure MultiValue.zero
   phi bb = fmap multiValue . Trav.traverse (MultiValue.phi bb) . unMultiValue
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb) (unMultiValue a) (unMultiValue b)

instance (MultiValue.Compose a) => MultiValue.Compose (Stereo.T a) where
   type Composed (Stereo.T a) = Stereo.T (MultiValue.Composed a)
   compose = multiValue . fmap MultiValue.compose

instance (MultiValue.Decompose p) => MultiValue.Decompose (Stereo.T p) where
   decompose p = liftA2 MultiValue.decompose p . unMultiValue

type instance MultiValue.Decomposed f (Stereo.T pa) =
                  Stereo.T (MultiValue.Decomposed f pa)
type instance MultiValue.PatternTuple (Stereo.T pa) =
                  Stereo.T (MultiValue.PatternTuple pa)

multiValue :: Stereo.T (MultiValue.T a) -> MultiValue.T (Stereo.T a)
multiValue = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValue :: MultiValue.T (Stereo.T a) -> Stereo.T (MultiValue.T a)
unMultiValue (MultiValue.Cons x) = fmap MultiValue.Cons x

consMultiValue :: MultiValue.T a -> MultiValue.T a -> MultiValue.T (Stereo.T a)
consMultiValue l r = multiValue $ Stereo.cons l r


unExpression :: Expr.Exp (Stereo.T a) -> Stereo.T (Expr.Exp a)
unExpression x =
   Stereo.cons
      (Expr.lift1 (MultiValue.lift1 Stereo.left) x)
      (Expr.lift1 (MultiValue.lift1 Stereo.right) x)


instance (MultiVector.C a) => MultiVector.C (Stereo.T a) where
   type Repr n (Stereo.T a) = Stereo.T (MultiVector.Repr n a)
   cons = multiVector . fmap MultiVector.cons . Stereo.sequence
   undef = multiVector $ pure MultiVector.undef
   zero = multiVector $ pure MultiVector.zero
   phi bb =
      fmap multiVector . Trav.traverse (MultiVector.phi bb) . unMultiVector
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiVector.addPhi bb) (unMultiVector a) (unMultiVector b)

   shuffle is u v =
      multiVector <$>
      traverse2 (MultiVector.shuffle is) (unMultiVector u) (unMultiVector v)
   extract k =
      fmap multiValue . Trav.traverse (MultiVector.extract k) . unMultiVector
   insert k a v =
      multiVector <$>
      traverse2 (MultiVector.insert k) (unMultiValue a) (unMultiVector v)

multiVector :: Stereo.T (MultiVector.T n a) -> MultiVector.T n (Stereo.T a)
multiVector = MultiVector.Cons . fmap (\(MultiVector.Cons a) -> a)

unMultiVector :: MultiVector.T n (Stereo.T a) -> Stereo.T (MultiVector.T n a)
unMultiVector (MultiVector.Cons x) = fmap MultiVector.Cons x


multiValueSerial ::
   Stereo.T (MultiValue.T (Serial.T n a)) ->
   MultiValue.T (Serial.T n (Stereo.T a))
multiValueSerial = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValueSerial ::
   MultiValue.T (Serial.T n (Stereo.T a)) ->
   Stereo.T (MultiValue.T (Serial.T n a))
unMultiValueSerial (MultiValue.Cons x) = fmap MultiValue.Cons x


instance
      (Expr.Aggregate e mv) => Expr.Aggregate (Stereo.T e) (Stereo.T mv) where
   type MultiValuesOf (Stereo.T e) = Stereo.T (Expr.MultiValuesOf e)
   type ExpressionsOf (Stereo.T mv) = Stereo.T (Expr.ExpressionsOf mv)
   bundle = Trav.traverse Expr.bundle
   dissect = fmap Expr.dissect


instance (Vector.Simple v) => Vector.Simple (Stereo.T v) where
   type Element (Stereo.T v) = Stereo.T (Vector.Element v)
   type Size (Stereo.T v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (Stereo.T v) where
   insert = Vector.insertTraversable


type Struct a = LLVM.Struct (a, (a, ()))

memory ::
   (Memory.C l) =>
   Memory.Record r (Struct (Memory.Struct l)) (Stereo.T l)
memory =
   liftA2 Stereo.cons
      (Memory.element Stereo.left  d0)
      (Memory.element Stereo.right d1)

instance (Memory.C l) => Memory.C (Stereo.T l) where
   type Struct (Stereo.T l) = Struct (Memory.Struct l)
   load = Memory.loadRecord memory
   store = Memory.storeRecord memory
   decompose = Memory.decomposeRecord memory
   compose = Memory.composeRecord memory

instance (Marshal.C l) => Marshal.C (Stereo.T l) where
   pack x = Marshal.pack (Stereo.left x, Stereo.right x)
   unpack = uncurry Stereo.cons . Marshal.unpack

instance (Storable.C l) => Storable.C (Stereo.T l) where
   load = Storable.loadApplicative
   store = Storable.storeFoldable

instance (MarshalMV.C l) => MarshalMV.C (Stereo.T l) where
   pack x = MarshalMV.pack (Stereo.left x, Stereo.right x)
   unpack = uncurry Stereo.cons . MarshalMV.unpack

instance (StorableMV.C l) => StorableMV.C (Stereo.T l) where
   load = StorableMV.loadApplicative
   store = StorableMV.storeFoldable

instance
   (StorableMV.Vector l, MultiVector.C l) =>
      StorableMV.Vector (Stereo.T l) where
   assembleVector p =
      Trav.traverse (StorableMV.assembleVector (Stereo.left<$>p)) .
      Stereo.sequence
   disassembleVector p =
      fmap (\x -> liftA2 Stereo.cons (Stereo.left x) (Stereo.right x)) .
      Trav.traverse (StorableMV.disassembleVector (Stereo.left<$>p))


{-
instance
      (Memory l s) =>
      Memory (Stereo.T l) (LLVM.Struct (s, (s, ()))) where
   load ptr =
      liftA2 Stereo.cons
         (load =<< getElementPtr0 ptr (d0, ()))
         (load =<< getElementPtr0 ptr (d1, ()))
   store y ptr = do
      store (Stereo.left  y) =<< getElementPtr0 ptr (d0, ())
      store (Stereo.right y) =<< getElementPtr0 ptr (d1, ())
-}

instance (A.Additive a) => A.Additive (Stereo.T a) where
   zero = Stereo.cons A.zero A.zero
   add x y = traverse2 A.add x y
   sub x y = traverse2 A.sub x y
   neg x   = Trav.traverse A.neg x

type instance A.Scalar (Stereo.T a) = A.Scalar a

instance (A.PseudoModule a) => A.PseudoModule (Stereo.T a) where
   scale a = Trav.traverse (A.scale a)



instance (MultiValue.Additive a) => MultiValue.Additive (Stereo.T a) where
   add x y =
      multiValue <$> traverse2 MultiValue.add (unMultiValue x) (unMultiValue y)
   sub x y =
      multiValue <$> traverse2 MultiValue.sub (unMultiValue x) (unMultiValue y)
   neg x = multiValue <$> Trav.traverse MultiValue.neg (unMultiValue x)


traverse2 ::
   (Monad m, Applicative t, Traversable t) =>
   (a -> b -> m c) -> t a -> t b -> m (t c)
traverse2 f x y = Trav.sequence $ liftA2 f x y



instance Value.Flatten a => Value.Flatten (Stereo.T a) where
   type Registers (Stereo.T a) = Stereo.T (Value.Registers a)
   flattenCode = Value.flattenCodeTraversable
   unfoldCode = Value.unfoldCodeTraversable
