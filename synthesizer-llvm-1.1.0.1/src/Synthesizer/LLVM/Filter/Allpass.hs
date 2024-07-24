{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Synthesizer.LLVM.Filter.Allpass (
   Parameter, Allpass.parameter,
   CascadeParameter(CascadeParameter), flangerParameter,
   cascadeParameterMultiValue, cascadeParameterUnMultiValue,
   causal, cascade, phaser,
   cascadePipeline, phaserPipeline,
   causalPacked, cascadePacked, phaserPacked,
   ) where

import Synthesizer.Plain.Filter.Recursive.Allpass (Parameter(Parameter))
import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1

import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1L

import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Functional as F
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as Serial

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value.Marshal as MarshalMV
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Scalar as Scalar
import qualified LLVM.Extra.Storable as Storable
import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Arithmetic as A

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Base.Proxy (Proxy(Proxy))

import Foreign.Storable (Storable)

import qualified Control.Category as Cat
import qualified Control.Applicative as App
import Control.Arrow ((<<<), (^<<), (<<^), (&&&), arr, first, second)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import Data.Tuple.HT (mapFst)

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module as Module

import NumericPrelude.Numeric
import NumericPrelude.Base


instance (Tuple.Phi a) => Tuple.Phi (Parameter a) where
   phi = Tuple.phiTraversable
   addPhi = Tuple.addPhiFoldable

instance Tuple.Undefined a => Tuple.Undefined (Parameter a) where
   undef = Tuple.undefPointed

instance Tuple.Zero a => Tuple.Zero (Parameter a) where
   zero = Tuple.zeroPointed

instance
   (Expr.Aggregate e mv) =>
      Expr.Aggregate (Parameter e) (Parameter mv) where
   type MultiValuesOf (Parameter e) = Parameter (Expr.MultiValuesOf e)
   type ExpressionsOf (Parameter mv) = Parameter (Expr.ExpressionsOf mv)
   bundle = Trav.traverse Expr.bundle
   dissect = fmap Expr.dissect

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

instance (Tuple.VectorValue n a) => Tuple.VectorValue n (Parameter a) where
   type VectorValueOf n (Parameter a) = Parameter (Tuple.VectorValueOf n a)
   vectorValueOf = fmap Tuple.vectorValueOf . Trav.sequenceA

instance (MultiValue.C a) => MultiValue.C (Allpass.Parameter a) where
   type Repr (Parameter a) = Parameter (MultiValue.Repr a)
   cons = paramFromPlainValue . MultiValue.cons . Allpass.getParameter

   undef = paramFromPlainValue MultiValue.undef
   zero = paramFromPlainValue MultiValue.zero

   phi bb =
      fmap paramFromPlainValue .
      MultiValue.phi bb .
      plainFromParamValue
   addPhi bb a b =
      MultiValue.addPhi bb
         (plainFromParamValue a)
         (plainFromParamValue b)

instance (MultiVector.C a) => MultiVector.C (Allpass.Parameter a) where
   type Repr n (Parameter a) = Parameter (MultiVector.Repr n a)
   cons = paramFromPlainVector . MultiVector.cons . fmap Allpass.getParameter
   undef = paramFromPlainVector MultiVector.undef
   zero = paramFromPlainVector MultiVector.zero

   phi bb =
      fmap paramFromPlainVector .
      MultiVector.phi bb .
      plainFromParamVector
   addPhi bb a b =
      MultiVector.addPhi bb
         (plainFromParamVector a)
         (plainFromParamVector b)

   shuffle is a b =
      fmap paramFromPlainVector $
      MultiVector.shuffle is (plainFromParamVector a) (plainFromParamVector b)
   extract i v =
      fmap paramFromPlainValue $
      MultiVector.extract i $
      plainFromParamVector v
   insert i a v =
      fmap paramFromPlainVector $
      MultiVector.insert i (plainFromParamValue a) $
      plainFromParamVector v

paramFromPlainVector ::
   MultiVector.T n a ->
   MultiVector.T n (Allpass.Parameter a)
paramFromPlainVector =
   MultiVector.lift1 Allpass.Parameter

plainFromParamVector ::
   MultiVector.T n (Allpass.Parameter a) ->
   MultiVector.T n a
plainFromParamVector =
   MultiVector.lift1 Allpass.getParameter

paramFromPlainValue ::
   MultiValue.T a ->
   MultiValue.T (Allpass.Parameter a)
paramFromPlainValue =
   MultiValue.lift1 Allpass.Parameter

plainFromParamValue ::
   MultiValue.T (Allpass.Parameter a) ->
   MultiValue.T a
plainFromParamValue =
   MultiValue.lift1 Allpass.getParameter


instance (Vector.Simple v) => Vector.Simple (Parameter v) where
   type Element (Parameter v) = Parameter (Vector.Element v)
   type Size (Parameter v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (Parameter v) where
   insert = Vector.insertTraversable

type instance F.Arguments f (Parameter a) = f (Parameter a)
instance F.MakeArguments (Parameter a) where
   makeArgs = id


newtype CascadeParameter n a =
   CascadeParameter (Allpass.Parameter a)
      deriving
         (Tuple.Undefined, Tuple.Zero, Storable,
          Functor, App.Applicative, Fold.Foldable, Trav.Traversable)

instance (Tuple.Phi a) => Tuple.Phi (CascadeParameter n a) where
   phi bb (CascadeParameter v) = fmap CascadeParameter $ Tuple.phi bb v
   addPhi bb (CascadeParameter x) (CascadeParameter y) = Tuple.addPhi bb x y


instance (Memory.C a) => Memory.C (CascadeParameter n a) where
   type Struct (CascadeParameter n a) = Memory.Struct a
   load = Memory.loadNewtype CascadeParameter
   store = Memory.storeNewtype (\(CascadeParameter k) -> k)
   decompose = Memory.decomposeNewtype CascadeParameter
   compose = Memory.composeNewtype (\(CascadeParameter k) -> k)

instance (Marshal.C a) => Marshal.C (CascadeParameter n a) where
   pack (CascadeParameter k) = Marshal.pack k
   unpack = CascadeParameter . Marshal.unpack

instance (MarshalMV.C a) => MarshalMV.C (CascadeParameter n a) where
   pack (CascadeParameter k) = MarshalMV.pack k
   unpack = CascadeParameter . MarshalMV.unpack

instance (Storable.C a) => Storable.C (CascadeParameter n a) where
   load = Storable.loadNewtype CascadeParameter id
   store = Storable.storeNewtype CascadeParameter id


instance (Tuple.Value a) => Tuple.Value (CascadeParameter n a) where
   type ValueOf (CascadeParameter n a) = Parameter (Tuple.ValueOf a)
   valueOf (CascadeParameter a) = Tuple.valueOf a

instance
   (Tuple.VectorValue n a) =>
      Tuple.VectorValue n (CascadeParameter m a) where
   type VectorValueOf n (CascadeParameter m a) =
            Parameter (Tuple.VectorValueOf n a)
   vectorValueOf =
      fmap Tuple.vectorValueOf . Trav.traverse (\(CascadeParameter k) -> k)

instance (MultiValue.C a) => MultiValue.C (CascadeParameter n a) where
   type Repr (CascadeParameter n a) = Parameter (MultiValue.Repr a)
   cons (CascadeParameter a) = cascadeFromParamValue $ MultiValue.cons a

   undef = cascadeFromParamValue MultiValue.undef
   zero = cascadeFromParamValue MultiValue.zero

   phi bb =
      fmap cascadeFromParamValue .
      MultiValue.phi bb .
      paramFromCascadeValue
   addPhi bb a b =
      MultiValue.addPhi bb
         (paramFromCascadeValue a)
         (paramFromCascadeValue b)

instance (MultiVector.C a) => MultiVector.C (CascadeParameter m a) where
   type Repr n (CascadeParameter m a) = Parameter (MultiVector.Repr n a)
   cons =
      cascadeFromParamVector . MultiVector.cons .
      fmap (\(CascadeParameter a) -> a)
   undef = cascadeFromParamVector MultiVector.undef
   zero = cascadeFromParamVector MultiVector.zero

   phi bb =
      fmap cascadeFromParamVector .
      MultiVector.phi bb .
      paramFromCascadeVector
   addPhi bb a b =
      MultiVector.addPhi bb
         (paramFromCascadeVector a)
         (paramFromCascadeVector b)

   shuffle is a b =
      fmap cascadeFromParamVector $
      MultiVector.shuffle is
         (paramFromCascadeVector a) (paramFromCascadeVector b)
   extract i v =
      fmap cascadeFromParamValue $
      MultiVector.extract i $
      paramFromCascadeVector v
   insert i a v =
      fmap cascadeFromParamVector $
      MultiVector.insert i (paramFromCascadeValue a) $
      paramFromCascadeVector v

cascadeFromParamVector ::
   MultiVector.T n (Allpass.Parameter a) ->
   MultiVector.T n (CascadeParameter m a)
cascadeFromParamVector = MultiVector.lift1 id

paramFromCascadeVector ::
   MultiVector.T n (CascadeParameter m a) ->
   MultiVector.T n (Allpass.Parameter a)
paramFromCascadeVector = MultiVector.lift1 id

cascadeFromParamValue ::
   MultiValue.T (Allpass.Parameter a) ->
   MultiValue.T (CascadeParameter m a)
cascadeFromParamValue = MultiValue.lift1 id

paramFromCascadeValue ::
   MultiValue.T (CascadeParameter m a) ->
   MultiValue.T (Allpass.Parameter a)
paramFromCascadeValue = MultiValue.lift1 id


instance (Vector.Simple v) => Vector.Simple (CascadeParameter n v) where
   type Element (CascadeParameter n v) = CascadeParameter n (Vector.Element v)
   type Size (CascadeParameter n v) = Vector.Size v
   shuffleMatch = Vector.shuffleMatchTraversable
   extract = Vector.extractTraversable

instance (Vector.C v) => Vector.C (CascadeParameter n v) where
   insert  = Vector.insertTraversable

type instance F.Arguments f (CascadeParameter n a) = f (CascadeParameter n a)
instance F.MakeArguments (CascadeParameter n a) where
   makeArgs = id


instance
   (Expr.Aggregate e mv, n ~ m) =>
      Expr.Aggregate (CascadeParameter n e) (CascadeParameter m mv) where
   type MultiValuesOf (CascadeParameter n e) =
            CascadeParameter n (Expr.MultiValuesOf e)
   type ExpressionsOf (CascadeParameter m mv) =
            CascadeParameter m (Expr.ExpressionsOf mv)
   bundle = Trav.traverse Expr.bundle
   dissect = fmap Expr.dissect


flangerParameter ::
   (Trans.C a, TypeNum.Natural n) =>
   Proxy n -> a -> CascadeParameter n a
flangerParameter order freq =
   CascadeParameter $
   Allpass.flangerParameter (TypeNum.integralFromProxy order) freq


causal ::
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v, Memory.C v) =>
   Causal.T (Parameter a, v) v
causal = Causal.fromModifier Allpass.firstOrderModifier


replicateStage ::
   (TypeNum.Natural n) =>
   (Tuple.Phi a, Tuple.Undefined a) =>
   (Tuple.Phi b, Tuple.Undefined b) =>
   Proxy n ->
   Causal.T (Parameter a, b) b ->
   Causal.T (CascadeParameter n a, b) b
replicateStage order stg =
   Causal.replicateControlled
      (TypeNum.integralFromProxy order)
      (stg <<< first (arr (\(CascadeParameter p) -> p)))

cascade ::
   (TypeNum.Natural n) =>
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v, Memory.C v) =>
   (Tuple.Phi a, Tuple.Undefined a) =>
   (Tuple.Phi v, Tuple.Undefined v) =>
   Causal.T (CascadeParameter n a, v) v
cascade = replicateStage Proxy causal

halfVector ::
   (A.RationalConstant a, a ~ A.Scalar v, A.PseudoModule v) =>
   Causal.T v v
halfVector = CausalPriv.map (A.scale $ A.fromRational' 0.5)

phaser ::
   (TypeNum.Natural n) =>
   (Module.C ae ve, Expr.Aggregate ae a, Expr.Aggregate ve v, Memory.C v) =>
   (Tuple.Phi a, Tuple.Undefined a) =>
   (Tuple.Phi v, Tuple.Undefined v) =>
   (A.RationalConstant a, a ~ A.Scalar v, A.PseudoModule v) =>
   Causal.T (CascadeParameter n a, v) v
phaser = (cascade + arr snd) <<< second halfVector


paramFromCascadeParam ::
   MultiValue.T (CascadeParameter n a) ->
   Allpass.Parameter (MultiValue.T a)
paramFromCascadeParam (MultiValue.Cons a) =
   fmap MultiValue.Cons a

{-
It shouldn't be too hard to use vector operations for the code we generate,
but LLVM-2.6 does not yet do it.
-}
stage ::
   (TypeNum.Positive n, MultiVector.C a,
    MultiVector.T n (CascadeParameter n a, a) ~ v,
    MultiValue.PseudoRing a, MultiValue.IntegerConstant a,
    MarshalMV.C a) =>
   Proxy n -> Causal.T v v
stage _ =
   Causal.vectorize $
      uncurry MultiValue.zip
      ^<<
      (arr fst &&&
       (Scalar.decons
        ^<<
        causal
        <<^
        (\(p, v) ->
           (fmap Scalar.Cons $ paramFromCascadeParam p, Scalar.Cons v))))
      <<^
      MultiValue.unzip

withSize ::
   (Proxy n -> Causal.T (mv (CascadeParameter n a), b) c) ->
   Causal.T (mv (CascadeParameter n a), b) c
withSize f = f Proxy

{- |
Fast implementation of 'cascade' using vector instructions.
However, there must be at least one pipeline stage,
primitive element types
and we get a delay by the number of pipeline stages.
-}
cascadePipeline ::
   (TypeNum.Positive n, MultiVector.C a,
    Tuple.ValueOf a ~ ar,
    MultiValue.PseudoRing a, MultiValue.IntegerConstant a,
    MarshalMV.C a, MarshalMV.Vector n a) =>
   Causal.T
      (MultiValue.T (CascadeParameter n a), MultiValue.T a)
      (MultiValue.T a)
cascadePipeline = withSize $ \order ->
   MultiValue.snd
   ^<<
   Causal.pipeline (stage order)
   <<^
   uncurry MultiValue.zip

vectorId ::
   Proxy n -> Causal.T (MultiVector.T n a) (MultiVector.T n a)
vectorId _ = Cat.id

half ::
   (A.RationalConstant a, A.PseudoRing a) =>
   Causal.T a a
half = CausalPriv.map (A.mul (A.fromRational' 0.5))


causalPacked,
  causalNonRecursivePacked ::
   (Serial.Write v, Serial.Element v ~ a,
    A.PseudoRing a, A.IntegerConstant a, Memory.C a,
    A.PseudoRing v, A.IntegerConstant v) =>
   Causal.T (Parameter a, v) v

causalPacked =
   Filt1L.causalRecursivePacked <<<
   (CausalPriv.map (\(Parameter k, _) -> fmap Filt1.Parameter $ A.neg k) &&&
    causalNonRecursivePacked)

causalNonRecursivePacked =
   CausalPriv.mapAccum
      (\(Parameter k, v0) x1 -> do
         (_,v1) <- Serial.shiftUp x1 v0
         y <- A.add v1 =<< A.mul v0 =<< Serial.upsample k
         u0 <- Serial.last v0
         return (y, u0))
      (return A.zero)

cascadePacked, phaserPacked ::
   (TypeNum.Natural n,
    Serial.Write v, Serial.Element v ~ a,
    A.PseudoRing a, A.IntegerConstant a, Memory.C a,
    A.PseudoRing v, A.RationalConstant v) =>
   Causal.T (CascadeParameter n a, v) v
cascadePacked = replicateStage Proxy causalPacked

phaserPacked =
   (cascadePacked + arr snd) <<<
   second (CausalPriv.map (A.mul (A.fromRational' 0.5)))


-- ToDo: consistent naming with Exponential2
cascadeParameterMultiValue ::
   CascadeParameter n (MultiValue.T a) ->
   MultiValue.T (CascadeParameter n a)
cascadeParameterMultiValue (CascadeParameter k) =
   MultiValue.Cons $ fmap (\(MultiValue.Cons a) -> a) k

cascadeParameterUnMultiValue ::
   MultiValue.T (CascadeParameter n a) ->
   CascadeParameter n (MultiValue.T a)
cascadeParameterUnMultiValue (MultiValue.Cons k) =
   CascadeParameter $ fmap MultiValue.Cons k


phaserPipelineMV ::
   (TypeNum.Positive n,
    MultiValue.PseudoRing a, MultiValue.RationalConstant a,
    Marshal.C a, MarshalMV.Vector n a) =>
   Causal.T
      (MultiValue.T (CascadeParameter n a), MultiValue.T a)
      (MultiValue.T a)
phaserPipelineMV = withSize $ \order ->
   Causal.mix <<<
   cascadePipeline &&&
   (Causal.pipeline (vectorId order) <<^ snd) <<<
--   (Causal.delay (const zero) (const $ TypeNum.integralFromProxy order) <<^ snd) <<<
   second half

phaserPipeline ::
   (TypeNum.Positive n,
    MultiValue.PseudoRing a, MultiValue.RationalConstant a,
    Marshal.C a, MarshalMV.Vector n a) =>
   Causal.T
      (CascadeParameter n (MultiValue.T a), MultiValue.T a)
      (MultiValue.T a)
phaserPipeline = phaserPipelineMV <<^ mapFst cascadeParameterMultiValue
