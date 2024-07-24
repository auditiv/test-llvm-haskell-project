{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.LLVM.Frame where

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified LLVM.Extra.Vector as Vector
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM
import LLVM.Core
          (CodeGenFunction, Value, Vector,
           IsPrimitive, IsArithmetic)

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (D2, D4)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import NumericPrelude.Numeric hiding (zero, one, div, signum)
import NumericPrelude.Base


{- |
Copy mono signal to both stereo channels.
-}
stereoFromMono ::
   a -> CodeGenFunction r (Stereo.T a)
stereoFromMono x =
   return $ Stereo.cons x x

mixMonoFromStereo ::
   (A.Additive a) =>
   Stereo.T a -> CodeGenFunction r a
mixMonoFromStereo s =
   mix (Stereo.left s) (Stereo.right s)


stereoFromVector ::
   (IsPrimitive a) =>
   Value (Vector D2 a) ->
   CodeGenFunction r (Stereo.T (Value a))
stereoFromVector x =
   Trav.mapM (LLVM.extractelement x . LLVM.valueOf) $ Stereo.cons 0 1

vectorFromStereo ::
   (IsPrimitive a) =>
   Stereo.T (Value a) ->
   CodeGenFunction r (Value (Vector D2 a))
vectorFromStereo =
   Vector.assemble . Fold.toList


quadroFromVector ::
   (IsPrimitive a) =>
   Value (Vector D4 a) ->
   CodeGenFunction r (Stereo.T (Stereo.T (Value a)))
quadroFromVector x =
   Trav.mapM (Trav.mapM (LLVM.extractelement x . LLVM.valueOf)) $
   Stereo.cons (Stereo.cons 0 1) (Stereo.cons 2 3)

vectorFromQuadro ::
   (IsPrimitive a) =>
   Stereo.T (Stereo.T (Value a)) ->
   CodeGenFunction r (Value (Vector D4 a))
vectorFromQuadro =
   Vector.assemble .
   concatMap Fold.toList . Fold.toList


mix ::
   (A.Additive a) =>
   a -> a -> CodeGenFunction r a
mix = A.add


{- |
This may mean more shuffling and is not necessarily better than mixStereo.
-}
mixStereoV ::
   (IsArithmetic a, IsPrimitive a) =>
   Stereo.T (Value a) -> Stereo.T (Value a) ->
   CodeGenFunction r (Stereo.T (Value a))
mixStereoV x y =
   do xv <- vectorFromStereo x
      yv <- vectorFromStereo y
      stereoFromVector =<< A.add xv yv

mixVector ::
   (Vector.Arithmetic a, TypeNum.Positive n) =>
   Value (Vector n a) ->
   CodeGenFunction r (Value a)
mixVector = Vector.sum

mixVectorToStereo ::
   (Vector.Arithmetic a, TypeNum.Positive n) =>
   Value (Vector n a) ->
   CodeGenFunction r (Stereo.T (Value a))
mixVectorToStereo =
   fmap (uncurry Stereo.cons) .
   Vector.sumInterleavedToPair

{- |
Mix components with even index to the left channel
and components with odd index to the right channel.
-}
mixInterleavedVectorToStereo ::
   (Vector.Arithmetic a, TypeNum.Positive n) =>
   Value (Vector n a) ->
   CodeGenFunction r (Stereo.T (Value a))
mixInterleavedVectorToStereo =
   fmap (uncurry Stereo.cons) .
   Vector.sumInterleavedToPair


amplifyMono ::
   (A.PseudoRing a) =>
   a -> a -> CodeGenFunction r a
amplifyMono = A.mul

amplifyStereo ::
   (A.PseudoRing a) =>
   a -> Stereo.T a -> CodeGenFunction r (Stereo.T a)
amplifyStereo x =
   Trav.mapM (A.mul x)
