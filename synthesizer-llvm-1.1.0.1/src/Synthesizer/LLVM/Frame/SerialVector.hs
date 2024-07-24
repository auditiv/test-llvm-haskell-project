{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
A special vector type that represents a time-sequence of samples.
This way we can distinguish safely between LLVM vectors
used for parallel signals and pipelines and
those used for chunky processing of scalar signals.
For the chunky processing this data type allows us
to derive the factor from the type
that time constants have to be multiplied with.
-}
module Synthesizer.LLVM.Frame.SerialVector (
   T(Cons),
   fromFixedList,
   upsample, subsample,
   shiftUp,
   reverse, iterate, cumulate,
   limit,
   select, cmp,
   ) where

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Code
import Synthesizer.LLVM.Frame.SerialVector.Code
         (T, fromMultiVector, toMultiVector)

import qualified LLVM.DSL.Expression.Vector as ExprVec
import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Vector as MultiValueVec
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Data.Word (Word32)

import Prelude hiding (replicate, reverse, iterate)


fromFixedList ::
   (TypeNum.Positive n, MultiVector.C a) =>
   LLVM.FixedList (TypeNum.ToUnary n) a -> Exp (T n a)
fromFixedList = fromOrdinary . Expr.cons . LLVM.vector



subsample :: (TypeNum.Positive n, MultiVector.C a) => Exp (T n a) -> Exp a
subsample =
   Expr.liftM (MultiValueVec.extract (A.zero :: LLVM.Value Word32)) . toOrdinary

upsample :: (TypeNum.Positive n, MultiVector.C a) => Exp a -> Exp (T n a)
upsample = fromOrdinary . ExprVec.replicate


shiftUp ::
   (TypeNum.Positive n, MultiVector.C x, Exp x ~ a, Exp (T n x) ~ v) =>
   a -> v -> (a, v)
shiftUp a v =
   (Expr.liftM2 ((fmap fst .) . Code.shiftUp) a v,
    Expr.liftM2 ((fmap snd .) . Code.shiftUp) a v)


iterate ::
   (TypeNum.Positive n, MultiVector.C a) =>
   (Exp a -> Exp a) -> Exp a -> Exp (T n a)
iterate f = fromOrdinary . ExprVec.iterate f

reverse ::
   (TypeNum.Positive n, MultiVector.C a) =>
   Exp (T n a) -> Exp (T n a)
reverse =
   Expr.liftM (fmap fromMultiVector . MultiVector.reverse . toMultiVector)


cumulate ::
   (TypeNum.Positive n, MultiVector.Additive a) =>
   Exp a -> Exp (T n a) -> (Exp a, Exp (T n a))
cumulate a v =
   (Expr.liftM2 ((fmap fst .) . Code.cumulate) a v,
    Expr.liftM2 ((fmap snd .) . Code.cumulate) a v)

limit ::
   (TypeNum.Positive n, MultiVector.Real a) =>
   (Exp (T n a), Exp (T n a)) -> Exp (T n a) -> Exp (T n a)
limit (l,u) =
   fromOrdinary . ExprVec.limit (toOrdinary l, toOrdinary u) . toOrdinary


cmp ::
   (TypeNum.Positive n, MultiVector.Comparison a) =>
   LLVM.CmpPredicate -> Exp (T n a) -> Exp (T n a) -> Exp (T n Bool)
cmp ord a b = fromOrdinary $ ExprVec.cmp ord (toOrdinary a) (toOrdinary b)

select ::
   (TypeNum.Positive n, MultiVector.Select a) =>
   Exp (T n Bool) -> Exp (T n a) -> Exp (T n a) -> Exp (T n a)
select c a b =
   fromOrdinary $ ExprVec.select (toOrdinary c) (toOrdinary a) (toOrdinary b)


fromOrdinary :: Exp (LLVM.Vector n a) -> Exp (T n a)
fromOrdinary = Expr.lift1 MultiValue.cast

toOrdinary :: Exp (T n a) -> Exp (LLVM.Vector n a)
toOrdinary = Expr.lift1 MultiValue.cast
