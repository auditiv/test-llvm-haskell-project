module Synthesizer.LLVM.Server.CommonPacked where

import Synthesizer.LLVM.Server.Common

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial

import qualified Data.NonEmpty as NonEmpty

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Algebra.Field as Field
import qualified Algebra.Additive as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


sumNested :: (Additive.C a) => [a] -> a
sumNested =
   maybe Additive.zero (NonEmpty.foldBalanced (+)) . NonEmpty.fetch


-- maybe this can be merged into a PCS.controllerDiscrete
stair :: Real -> Real
stair i =
   let n = fromIntegral (round i :: Int)
       r = i - n
   in  n + 0.01*r


type Vector = Serial.T VectorSize Real
type VectorValue = Serial.Value VectorSize Real
type VectorSize = TypeNum.D4


-- ToDo: generalize to Integral class
vectorSize :: Int
vectorSize =
   TypeNum.integralFromSingleton
      (TypeNum.singleton :: TypeNum.Singleton VectorSize)

vectorRate :: (Field.C a) => SampleRate a -> a
vectorRate (SampleRate sr) = sr / fromIntegral vectorSize

vectorTime :: (Field.C a) => SampleRate a -> a -> a
vectorTime (SampleRate sr) param = param * sr / fromIntegral vectorSize
