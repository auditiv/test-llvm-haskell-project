{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Generator.Private where

import Synthesizer.LLVM.Private (getPairPtrs, noLocalPtr)

import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction)

import Type.Base.Proxy (Proxy(Proxy))

import Control.Applicative (Applicative, liftA2, pure, (<*>), (<$>))

import Data.Semigroup (Semigroup, (<>))
import Data.Tuple.Strict (mapFst, zipPair)

import qualified Number.Ratio as Ratio
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import qualified Prelude as P
import Prelude hiding (iterate, takeWhile, map, zipWith)


data T a =
   forall global local state.
      (Memory.C global, LLVM.IsSized local, Memory.C state) =>
      Cons (forall r c.
            (Tuple.Phi c) =>
            global ->
            -- pointer to loop local storage
            LLVM.Value (LLVM.Ptr local) ->
            state -> MaybeCont.T r c (a, state))
               -- compute next value
           (forall r. CodeGenFunction r (global, state))
               -- initial state
           (forall r. global -> CodeGenFunction r ())
               -- cleanup


noGlobal ::
   (LLVM.IsSized local, Memory.C state) =>
   (forall r c.
    (Tuple.Phi c) =>
    LLVM.Value (LLVM.Ptr local) -> state -> MaybeCont.T r c (a, state)) ->
   (forall r. CodeGenFunction r state) ->
   T a
noGlobal next start = Cons (const next) (fmap ((,) ()) start) return

alloca :: (LLVM.IsSized a) => T (LLVM.Value (LLVM.Ptr a))
alloca =
   noGlobal
      (\ptr () -> return (ptr, ()))
      (return ())


iterate ::
   (Memory.C a) =>
   (forall r. a -> CodeGenFunction r a) ->
   (forall r. CodeGenFunction r a) -> T a
iterate f a =
   noGlobal
      (noLocalPtr $ \s -> fmap ((,) s) $ MaybeCont.lift $ f s)
      a

iterateParam ::
   (Memory.C b, Memory.C a) =>
   (forall r. b -> a -> CodeGenFunction r a) ->
   (forall r. CodeGenFunction r b) ->
   (forall r. CodeGenFunction r a) -> T a
iterateParam f b a =
   fmap snd $ iterate (\(bi,ai) -> (,) bi <$> f bi ai) (liftA2 (,) b a)

takeWhile ::
   (forall r. a -> CodeGenFunction r (LLVM.Value Bool)) -> T a -> T a
takeWhile p (Cons next start stop) = Cons
   (\global local s0 -> do
      (a,s1) <- next global local s0
      MaybeCont.guard =<< MaybeCont.lift (p a)
      return (a,s1))
   start
   stop


empty :: T a
empty = noGlobal (noLocalPtr $ \ _state -> MaybeCont.nothing) (return ())

{- |
Appending many signals is inefficient,
since in cascadingly appended signals the parts are counted in an unary way.
Concatenating infinitely many signals is impossible.
If you want to concatenate a lot of signals,
please render them to lazy storable vectors first.
-}
{-
We might save a little space by using a union
for the states of the first and the second signal generator.
If the concatenated generators allocate memory,
we could also save some memory by calling @startB@
only after the first generator finished.
However, for correct deallocation
we would need to track which of the @start@ blocks
have been executed so far.
This in turn might be difficult in connection with the garbage collector.
-}
append :: (Tuple.Phi a, Tuple.Undefined a) => T a -> T a -> T a
append (Cons nextA startA stopA) (Cons nextB startB stopB) = Cons
   (\(globalA, globalB) local (sa0,sb0,phaseB) -> do
      (localA,localB) <- getPairPtrs local
      MaybeCont.alternative
         (do
            MaybeCont.guard =<< MaybeCont.lift (LLVM.inv phaseB)
            (a,sa1) <- nextA globalA localA sa0
            return (a, (sa1, sb0, LLVM.valueOf False)))
         (do
            (b,sb1) <- nextB globalB localB sb0
            return (b, (sa0, sb1, LLVM.valueOf True))))
   (do
      (globalA,stateA) <- startA
      (globalB,stateB) <- startB
      return ((globalA,globalB), (stateA, stateB, LLVM.valueOf False)))
   (\(globalA,globalB) -> stopB globalB >> stopA globalA)

instance (Tuple.Phi a, Tuple.Undefined a) => Semigroup (T a) where
   (<>) = append

instance (Tuple.Phi a, Tuple.Undefined a) => Monoid (T a) where
   mempty = empty
   mappend = (<>)



instance Functor T where
   fmap f (Cons next start stop) = Cons
      (\global local s -> mapFst f <$> next global local s)
      start stop

instance Applicative T where
   pure a = noGlobal (noLocalPtr $ \() -> return (a, ())) (return ())
   Cons nextF startF stopF <*> Cons nextA startA stopA = Cons
      (\(globalF, globalA) local (sf0,sa0) -> do
         (localF,localA) <- getPairPtrs local
         (f,sf1) <- nextF globalF localF sf0
         (a,sa1) <- nextA globalA localA sa0
         return (f a, (sf1,sa1)))
      (liftA2 zipPair startF startA)
      (\(globalF, globalA) -> stopA globalA >> stopF globalF)


map :: (forall r. a -> CodeGenFunction r b) -> T a -> T b
map f (Cons next start stop) =
   Cons
      (\global local sa0 -> do
         (a,sa1) <- next global local sa0
         b <- MaybeCont.lift $ f a
         return (b, sa1))
      start stop

zipWith :: (forall r. a -> b -> CodeGenFunction r c) -> T a -> T b -> T c
zipWith f as bs = map (uncurry f) $ liftA2 (,) as bs

instance (A.Additive a) => Additive.C (T a) where
   zero = pure A.zero
   negate = map A.neg
   (+) = zipWith A.add
   (-) = zipWith A.sub

instance (A.PseudoRing a, A.IntegerConstant a) => Ring.C (T a) where
   one = pure A.one
   fromInteger n = pure (A.fromInteger' n)
   (*) = zipWith A.mul

instance (A.Field a, A.RationalConstant a) => Field.C (T a) where
   fromRational' x = pure (A.fromRational' $ Ratio.toRational98 x)
   (/) = zipWith A.fdiv


instance (A.PseudoRing a, A.Real a, A.IntegerConstant a) => P.Num (T a) where
   fromInteger n = pure (A.fromInteger' n)
   negate = map A.neg
   (+) = zipWith A.add
   (-) = zipWith A.sub
   (*) = zipWith A.mul
   abs = map A.abs
   signum = map A.signum

instance (A.Field a, A.Real a, A.RationalConstant a) => P.Fractional (T a) where
   fromRational x = pure (A.fromRational' x)
   (/) = zipWith A.fdiv



arraySize :: value (array n a) -> Proxy n
arraySize _ = Proxy
