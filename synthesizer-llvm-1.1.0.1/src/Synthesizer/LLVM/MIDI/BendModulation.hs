{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Various LLVM related instances of the BM.T type.
I have setup a separate module since these instances are orphan
and need several language extensions.
-}
module Synthesizer.LLVM.MIDI.BendModulation (
   BM.T(..),
   BM.deflt,
   BM.shift,
   multiValue,
   unMultiValue,
   ) where

import qualified Synthesizer.MIDI.Value.BendModulation as BM
import qualified Synthesizer.LLVM.Causal.Functional as F

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value.Storable as StorableMV
import qualified LLVM.Extra.Multi.Value.Marshal as MarshalMV
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Control as C
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import Control.Applicative (liftA2)


instance (Tuple.Zero a) => Tuple.Zero (BM.T a) where
   zero = Tuple.zeroPointed

instance (Tuple.Undefined a) => Tuple.Undefined (BM.T a) where
   undef = Tuple.undefPointed

instance (C.Select a) => C.Select (BM.T a) where
   select = C.selectTraversable

instance Tuple.Value h => Tuple.Value (BM.T h) where
   type ValueOf (BM.T h) = BM.T (Tuple.ValueOf h)
   valueOf = Tuple.valueOfFunctor


instance (Expr.Aggregate e mv) => Expr.Aggregate (BM.T e) (BM.T mv) where
   type MultiValuesOf (BM.T e) = BM.T (Expr.MultiValuesOf e)
   type ExpressionsOf (BM.T mv) = BM.T (Expr.ExpressionsOf mv)
   bundle = Trav.traverse Expr.bundle
   dissect = fmap Expr.dissect

instance (MultiValue.C a) => MultiValue.C (BM.T a) where
   type Repr (BM.T a) = BM.T (MultiValue.Repr a)
   cons = multiValue . fmap MultiValue.cons
   undef = multiValue $ pure MultiValue.undef
   zero = multiValue $ pure MultiValue.zero
   phi bb = fmap multiValue . Trav.traverse (MultiValue.phi bb) . unMultiValue
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb) (unMultiValue a) (unMultiValue b)

instance (MarshalMV.C l) => MarshalMV.C (BM.T l) where
   pack (BM.Cons bend depth) = MarshalMV.pack (bend, depth)
   unpack = uncurry BM.Cons . MarshalMV.unpack

instance (StorableMV.C l) => StorableMV.C (BM.T l) where
   load = StorableMV.loadApplicative
   store = StorableMV.storeFoldable

multiValue :: BM.T (MultiValue.T a) -> MultiValue.T (BM.T a)
multiValue = MultiValue.Cons . fmap (\(MultiValue.Cons a) -> a)

unMultiValue :: MultiValue.T (BM.T a) -> BM.T (MultiValue.T a)
unMultiValue (MultiValue.Cons x) = fmap MultiValue.Cons x


type Struct a = LLVM.Struct (a, (a, ()))

memory :: (Memory.C l) => Memory.Record r (Struct (Memory.Struct l)) (BM.T l)
memory =
   liftA2 BM.Cons
      (Memory.element BM.bend  TypeNum.d0)
      (Memory.element BM.depth TypeNum.d1)

instance (Memory.C l) => Memory.C (BM.T l) where
   type Struct (BM.T l) = Struct (Memory.Struct l)
   load = Memory.loadRecord memory
   store = Memory.storeRecord memory
   decompose = Memory.decomposeRecord memory
   compose = Memory.composeRecord memory

instance (Marshal.C l) => Marshal.C (BM.T l) where
   pack (BM.Cons bend depth) = Marshal.pack (bend, depth)
   unpack = uncurry BM.Cons . Marshal.unpack

instance (Storable.C l) => Storable.C (BM.T l) where
   load = Storable.loadApplicative
   store = Storable.storeFoldable

instance (Tuple.Phi a) => Tuple.Phi (BM.T a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable


instance (Vector.Simple v) => Vector.Simple (BM.T v) where
   type Element (BM.T v) = BM.T (Vector.Element v)
   type Size (BM.T v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (BM.T v) where
   insert  = Vector.insertTraversable


type instance F.Arguments f (BM.T a) = f (BM.T a)
instance F.MakeArguments (BM.T a) where
   makeArgs = id
