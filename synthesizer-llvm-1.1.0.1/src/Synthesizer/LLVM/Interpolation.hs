{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Synthesizer.LLVM.Interpolation (
   C(margin),
   loadNodes,
   indexNodes,
   loadNodesExp,
   indexNodesExp,

   Margin(..),
   zipMargin,
   unzipMargin,
   toMargin,
   marginNumberExp,
   marginOffsetExp,

   T,

   Nodes02(..),
   linear,
   linearVector,

   Nodes13(..),
   cubic,
   cubicVector,
   ) where

import qualified Synthesizer.LLVM.Value as Value

import qualified Synthesizer.LLVM.Frame.SerialVector.Class as Serial
import qualified Synthesizer.Interpolation.Core as Interpolation

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Scalar as Scalar
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Core as LLVM

import LLVM.Core (CodeGenFunction, Value)

import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import Data.Word (Word)

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Control.Monad.Trans.State as MS
import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Traversable (Traversable, traverse, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)


data Margin nodes = Margin { marginNumber, marginOffset :: Int }
   deriving (Show, Eq)

singletonMargin :: MultiValue.T Int -> MultiValue.T (Margin nodes)
singletonMargin n = zipMargin n n

unzipMargin ::
   MultiValue.T (Margin nodes) -> (MultiValue.T Int, MultiValue.T Int)
unzipMargin (MultiValue.Cons (from, to)) =
   (MultiValue.Cons from, MultiValue.Cons to)

zipMargin :: MultiValue.T Int -> MultiValue.T Int -> MultiValue.T (Margin nodes)
zipMargin (MultiValue.Cons from) (MultiValue.Cons to) =
   MultiValue.Cons (from, to)

marginNumberExp :: (Expr.Value val) => val (Margin nodes) -> val Int
marginNumberExp = Expr.lift1 (fst . unzipMargin)

marginOffsetExp :: (Expr.Value val) => val (Margin nodes) -> val Int
marginOffsetExp = Expr.lift1 (snd . unzipMargin)

instance MultiValue.C (Margin nodes) where
   type Repr (Margin nodes) = (LLVM.Value Int, LLVM.Value Int)
   cons (Margin start len) =
      zipMargin (MultiValue.cons start) (MultiValue.cons len)
   undef = singletonMargin MultiValue.undef
   zero = singletonMargin MultiValue.zero
   phi bb a =
      case unzipMargin a of
         (a0,a1) ->
            liftA2 zipMargin (MultiValue.phi bb a0) (MultiValue.phi bb a1)
   addPhi bb a b =
      case (unzipMargin a, unzipMargin b) of
         ((a0,a1), (b0,b1)) -> do
            MultiValue.addPhi bb a0 b0
            MultiValue.addPhi bb a1 b1


class (Applicative nodes, Traversable nodes) => C nodes where
   margin :: Margin (nodes a)

type T r nodes a v = a -> nodes v -> CodeGenFunction r v


toMargin ::
   (C nodes) =>
   (forall r. T r nodes a v) ->
   Margin (nodes v)
toMargin _ = margin


{- |
Zero nodes before index 0 and two nodes starting from index 0.
-}
data Nodes02 a = Nodes02 {nodes02_0, nodes02_1 :: a}

instance C Nodes02 where
   margin = Margin { marginNumber = 2, marginOffset = 0 }


instance Functor Nodes02 where
   fmap f (Nodes02 x0 x1) = Nodes02 (f x0) (f x1)

instance Applicative Nodes02 where
   pure x = Nodes02 x x
   (Nodes02 f0 f1) <*> (Nodes02 x0 x1) = Nodes02 (f0 x0) (f1 x1)

instance Foldable Nodes02 where
   foldMap = foldMapDefault

instance Traversable Nodes02 where
   traverse f (Nodes02 x0 x1) = liftA2 Nodes02 (f x0) (f x1)


instance (Serial.Sized value) => Serial.Sized (Nodes02 value) where
   type Size (Nodes02 value) = Serial.Size value

instance (Serial.Read v) => Serial.Read (Nodes02 v) where
   type Element (Nodes02 v) = Nodes02 (Serial.Element v)
   type ReadIt (Nodes02 v) = Nodes02 (Serial.ReadIt v)

   extract = Serial.extractTraversable

   readStart = Serial.readStartTraversable
   readNext = Serial.readNextTraversable

instance (Serial.Write v) => Serial.Write (Nodes02 v) where
   type WriteIt (Nodes02 v) = Nodes02 (Serial.WriteIt v)

   insert = Serial.insertTraversable

   writeStart = Serial.writeStartTraversable
   writeNext = Serial.writeNextTraversable
   writeStop = Serial.writeStopTraversable


instance (Tuple.Undefined a) => Tuple.Undefined (Nodes02 a) where
   undef = Tuple.undefPointed

instance (Tuple.Phi a) => Tuple.Phi (Nodes02 a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable


type Struct02 a = LLVM.Struct (a, (a, ()))

memory02 ::
   (Memory.C l) =>
   Memory.Record r (Struct02 (Memory.Struct l)) (Nodes02 l)
memory02 =
   liftA2 Nodes02
      (Memory.element nodes02_0 TypeNum.d0)
      (Memory.element nodes02_1 TypeNum.d1)

instance (Memory.C l) => Memory.C (Nodes02 l) where
   type Struct (Nodes02 l) = Struct02 (Memory.Struct l)
   load = Memory.loadRecord memory02
   store = Memory.storeRecord memory02
   decompose = Memory.decomposeRecord memory02
   compose = Memory.composeRecord memory02


linear ::
   (A.PseudoRing a, A.IntegerConstant a) =>
   T r Nodes02 a a
linear r (Nodes02 a b) =
   Scalar.unliftM3 (Value.unlift3 Interpolation.linear) a b r

linearVector ::
   (A.PseudoModule v, A.Scalar v ~ a, A.IntegerConstant a) =>
   T r Nodes02 a v
linearVector r (Nodes02 a b) =
   Value.unlift3 Interpolation.linear a b r




{- |
One node before index 0 and three nodes starting from index 0.
-}
data Nodes13 a = Nodes13 {nodes13_0, nodes13_1, nodes13_2, nodes13_3 :: a}

instance C Nodes13 where
   margin = Margin { marginNumber = 4, marginOffset = 1 }

instance Functor Nodes13 where
   fmap f (Nodes13 x0 x1 x2 x3) = Nodes13 (f x0) (f x1) (f x2) (f x3)

instance Applicative Nodes13 where
   pure x = Nodes13 x x x x
   (Nodes13 f0 f1 f2 f3) <*> (Nodes13 x0 x1 x2 x3) =
      Nodes13 (f0 x0) (f1 x1) (f2 x2) (f3 x3)

instance Foldable Nodes13 where
   foldMap = foldMapDefault

instance Traversable Nodes13 where
   traverse f (Nodes13 x0 x1 x2 x3) =
      pure Nodes13 <*> f x0 <*> f x1 <*> f x2 <*> f x3


instance (Serial.Sized value) => Serial.Sized (Nodes13 value) where
   type Size (Nodes13 value) = Serial.Size value

instance (Serial.Read v) => Serial.Read (Nodes13 v) where
   type Element (Nodes13 v) = Nodes13 (Serial.Element v)
   type ReadIt (Nodes13 v) = Nodes13 (Serial.ReadIt v)

   extract = Serial.extractTraversable

   readStart = Serial.readStartTraversable
   readNext = Serial.readNextTraversable

instance (Serial.Write v) => Serial.Write (Nodes13 v) where
   type WriteIt (Nodes13 v) = Nodes13 (Serial.WriteIt v)

   insert = Serial.insertTraversable

   writeStart = Serial.writeStartTraversable
   writeNext = Serial.writeNextTraversable
   writeStop = Serial.writeStopTraversable


instance (Tuple.Undefined a) => Tuple.Undefined (Nodes13 a) where
   undef = Tuple.undefPointed

instance (Tuple.Phi a) => Tuple.Phi (Nodes13 a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable


type Struct13 a = LLVM.Struct (a, (a, (a, (a, ()))))

memory13 ::
   (Memory.C l) =>
   Memory.Record r (Struct13 (Memory.Struct l)) (Nodes13 l)
memory13 =
   pure Nodes13
      <*> Memory.element nodes13_0 TypeNum.d0
      <*> Memory.element nodes13_1 TypeNum.d1
      <*> Memory.element nodes13_2 TypeNum.d2
      <*> Memory.element nodes13_3 TypeNum.d3

instance (Memory.C l) => Memory.C (Nodes13 l) where
   type Struct (Nodes13 l) = Struct13 (Memory.Struct l)
   load = Memory.loadRecord memory13
   store = Memory.storeRecord memory13
   decompose = Memory.decomposeRecord memory13
   compose = Memory.composeRecord memory13


cubic ::
   (A.Field a, A.RationalConstant a) =>
   T r Nodes13 a a
cubic r (Nodes13 a b c d) =
   Scalar.unliftM5 (Value.unlift5 Interpolation.cubic) a b c d r

cubicVector ::
   (A.PseudoModule v, A.Scalar v ~ a, A.Field a, A.RationalConstant a) =>
   T r Nodes13 a v
cubicVector r (Nodes13 a b c d) =
   Value.unlift5 Interpolation.cubic a b c d r


loadNodesExp ::
   (C nodes, Storable am) =>
   (Value (Ptr am) -> CodeGenFunction r a) ->
   MultiValue.T Int ->
   Value (Ptr am) -> CodeGenFunction r (nodes a)
loadNodesExp loadNode (MultiValue.Cons step) =
   MS.evalStateT $ sequenceA $ pure $ loadNext loadNode step

loadNodes ::
   (C nodes, Storable am) =>
   (Value (Ptr am) -> CodeGenFunction r a) ->
   Value Int ->
   Value (Ptr am) -> CodeGenFunction r (nodes a)
loadNodes loadNode step =
   MS.evalStateT $ sequenceA $ pure $ loadNext loadNode step

loadNext ::
   (Storable am) =>
   (Value (Ptr am) -> CodeGenFunction r a) ->
   Value Int ->
   MS.StateT (Value (Ptr am)) (CodeGenFunction r) a
loadNext loadNode step =
   MS.StateT $ \ptr -> liftA2 (,) (loadNode ptr) (Storable.advancePtr step ptr)



indexNodesExp ::
   (C nodes) =>
   (MultiValue.T Word -> CodeGenFunction r v) ->
   MultiValue.T Word ->
   MultiValue.T Word -> CodeGenFunction r (nodes v)
indexNodesExp indexNode (MultiValue.Cons step) (MultiValue.Cons offset) =
   indexNodes (indexNode . MultiValue.Cons) step offset

indexNodes ::
   (C nodes) =>
   (Value Word -> CodeGenFunction r v) ->
   Value Word ->
   Value Word -> CodeGenFunction r (nodes v)
indexNodes indexNode step =
   MS.evalStateT $ sequenceA $ pure $ indexNext indexNode step

indexNext ::
   (Value Word -> CodeGenFunction r v) ->
   Value Word ->
   MS.StateT (Value Word) (CodeGenFunction r) v
indexNext indexNode step =
   MS.StateT $ \i -> liftA2 (,) (indexNode i) (A.add i step)
