{-# LANGUAGE TypeFamilies #-}
{- |
A special vector type that represents a time-sequence of samples.
This way we can distinguish safely between LLVM vectors
used for parallel signals and pipelines and
those used for chunky processing of scalar signals.
For the chunky processing this data type allows us
to derive the factor from the type
that time constants have to be multiplied with.
-}
module Synthesizer.LLVM.Frame.SerialVector.Plain (
   T(Cons),
   fromList,
   replicate,
   iterate,
   ) where

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Code
import Synthesizer.LLVM.Frame.SerialVector.Code (T)

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty

import Prelude as P hiding (zip, unzip, last, reverse, iterate, replicate)


fromList :: (TypeNum.Positive n) => NonEmpty.T [] a -> T n a
fromList = Code.Cons . LLVM.cyclicVector

replicate :: (TypeNum.Positive n) => a -> T n a
replicate = Code.Cons . pure

iterate :: (TypeNum.Positive n) => (a -> a) -> a -> T n a
iterate f x = fromList $ NonEmptyC.iterate f x
