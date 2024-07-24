{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Causal.Private where

import qualified Synthesizer.LLVM.Generator.Private as Sig
import Synthesizer.LLVM.Private (getPairPtrs, noLocalPtr, unbool)

import qualified Synthesizer.Causal.Class as CausalClass
import qualified Synthesizer.Causal.Utility as ArrowUtil
import Synthesizer.Causal.Class (($>))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Control as C
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM
import LLVM.Core (CodeGenFunction)

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Control.Category as Cat
import Control.Arrow (Arrow, arr, first, (&&&), (<<<))
import Control.Category (Category)
import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$>))

import Data.Tuple.Strict (mapFst, zipPair)
import Data.Word (Word)

import qualified Number.Ratio as Ratio
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import NumericPrelude.Base hiding (map, zip, zipWith, init)

import qualified Prelude as P


data T a b =
   forall global local state.
      (Memory.C global, LLVM.IsSized local, Memory.C state) =>
      Cons (forall r c.
            (Tuple.Phi c) =>
            global -> LLVM.Value (LLVM.Ptr local) ->
            a -> state -> MaybeCont.T r c (b, state))
               -- compute next value
           (forall r. CodeGenFunction r (global, state))
               -- initial state
           (forall r. global -> CodeGenFunction r ())
               -- cleanup


type instance CausalClass.ProcessOf Sig.T = T

instance CausalClass.C T where
   type SignalOf T = Sig.T
   toSignal (Cons next start stop) = Sig.Cons
      (\global local -> next global local ())
      start
      stop
   fromSignal (Sig.Cons next start stop) = Cons
      (\global local _ -> next global local)
      start
      stop


noGlobal ::
   (LLVM.IsSized local, Memory.C state) =>
   (forall r c.
    (Tuple.Phi c) =>
    LLVM.Value (LLVM.Ptr local) -> a -> state -> MaybeCont.T r c (b, state)) ->
   (forall r. CodeGenFunction r state) ->
   T a b
noGlobal next start =
   Cons (const next) (fmap ((,) ()) start) return

simple ::
   (Memory.C state) =>
   (forall r c. (Tuple.Phi c) => a -> state -> MaybeCont.T r c (b, state)) ->
   (forall r. CodeGenFunction r state) ->
   T a b
simple next start = noGlobal (noLocalPtr next) start

mapAccum ::
   (Memory.C state) =>
   (forall r. a -> state -> CodeGenFunction r (b, state)) ->
   (forall r. CodeGenFunction r state) ->
   T a b
mapAccum next =
   simple (\a s -> MaybeCont.lift $ next a s)

map ::
   (forall r. a -> CodeGenFunction r b) ->
   T a b
map f =
   mapAccum (\a s -> fmap (flip (,) s) $ f a) (return ())

zipWith ::
   (forall r. a -> b -> CodeGenFunction r c) ->
   T (a,b) c
zipWith f = map (uncurry f)


instance Category T where
   id = map return
   Cons nextB startB stopB . Cons nextA startA stopA = Cons
      (\(globalA, globalB) local a (sa0,sb0) -> do
         (localA,localB) <- getPairPtrs local
         (b,sa1) <- nextA globalA localA a sa0
         (c,sb1) <- nextB globalB localB b sb0
         return (c, (sa1,sb1)))
      (liftA2 zipPair startA startB)
      (\(globalA, globalB) -> stopA globalA >> stopB globalB)

instance Arrow T where
   arr f = map (return . f)
   first (Cons next start stop) = Cons (firstNext next) start stop

firstNext ::
   (Functor m) =>
   (global -> local -> a -> s -> m (b, s)) ->
   global -> local ->  (a, c) -> s -> m ((b, c), s)
firstNext next global local (b,d) s0 =
   fmap
      (\(c,s1) -> ((c,d), s1))
      (next global local b s0)


instance Functor (T a) where
   fmap = flip (>>^)

instance Applicative (T a) where
   pure = ArrowUtil.pure
   (<*>) = ArrowUtil.apply


infixr 1 >>^, ^>>

(>>^) :: T a b -> (b -> c) -> T a c
Cons next start stop >>^ f =
   Cons
      (\global local a state -> mapFst f <$> next global local a state)
      start stop

(^>>) :: (a -> b) -> T b c -> T a c
f ^>> Cons next start stop =
   Cons
      (\global local -> next global local . f)
      start stop


mapProc ::
   (forall r. b -> CodeGenFunction r c) ->
   T a b -> T a c
mapProc f x = map f <<< x

zipProcWith ::
   (forall r. b -> c -> CodeGenFunction r d) ->
   T a b -> T a c -> T a d
zipProcWith f x y = zipWith f <<< x&&&y


instance (A.Additive b) => Additive.C (T a b) where
   zero = pure A.zero
   negate = mapProc A.neg
   (+) = zipProcWith A.add
   (-) = zipProcWith A.sub

instance (A.PseudoRing b, A.IntegerConstant b) => Ring.C (T a b) where
   one = pure A.one
   fromInteger n = pure (A.fromInteger' n)
   (*) = zipProcWith A.mul

instance (A.Field b, A.RationalConstant b) => Field.C (T a b) where
   fromRational' x = pure (A.fromRational' $ Ratio.toRational98 x)
   (/) = zipProcWith A.fdiv


instance (A.PseudoRing b, A.Real b, A.IntegerConstant b) => P.Num (T a b) where
   fromInteger n = pure (A.fromInteger' n)
   negate = mapProc A.neg
   (+) = zipProcWith A.add
   (-) = zipProcWith A.sub
   (*) = zipProcWith A.mul
   abs = mapProc A.abs
   signum = mapProc A.signum

instance
      (A.Field b, A.Real b, A.RationalConstant b) => P.Fractional (T a b) where
   fromRational x = pure (A.fromRational' x)
   (/) = zipProcWith A.fdiv


{- |
Not quite the loop of ArrowLoop
because we need a delay of one time step
and thus an initialization value.

For a real ArrowLoop.loop, that is a zero-delay loop,
we would formally need a MonadFix instance of CodeGenFunction.
But this will not become reality, since LLVM is not able to re-order code
in a way that allows to access a result before creating the input.
-}
loop ::
   (Memory.C c) =>
   (forall r. CodeGenFunction r c) -> T (a,c) (b,c) -> T a b
loop initial (Cons next start stop) = Cons
   (\global local a0 (c0,s0) -> do
      ((b1,c1), s1) <- next global local (a0,c0) s0
      return (b1,(c1,s1)))
   (liftA2 (\ini (global,s) -> (global,(ini,s))) initial start)
   stop


replicateSerial ::
   (Tuple.Undefined a, Tuple.Phi a) =>
   Exp Word -> T a a -> T a a
replicateSerial n proc =
   (\a -> ((),a)) ^>> replicateControlled n (snd^>>proc)

replicateControlled ::
   (Tuple.Undefined a, Tuple.Phi a) =>
   Exp Word -> T (c,a) a -> T (c,a) a
replicateControlled n (Cons next start stop) = Cons
   (\(len,globalStates) local (c,a) () ->
      MaybeCont.fromMaybe $ fmap (\(_,ms) -> flip (,) () <$> ms) $
         MaybeCont.arrayLoop len globalStates a $
               \globalStatePtr a0 -> do
            (global, s0) <- MaybeCont.lift $ Memory.load globalStatePtr
            (a1,s1) <- next global local (c,a0) s0
            MaybeCont.lift $
               Memory.store s1 =<<
               LLVM.getElementPtr0 globalStatePtr (TypeNum.d1, ())
            return a1)
   (do
      MultiValue.Cons len <- Expr.unExp n
      globalStates <- LLVM.arrayMalloc len
      C.arrayLoop len globalStates () $ \globalStatePtr () ->
         flip Memory.store globalStatePtr =<< start
      return ((len,globalStates), ()))
   (\(len,globalStates) -> do
      C.arrayLoop len globalStates () $ \globalStatePtr () ->
         stop =<< Memory.load
            =<< LLVM.getElementPtr0 globalStatePtr (TypeNum.d0, ())
      LLVM.free globalStates)

{-
We can implement 'replicateControlled' in terms of 'replicateSerial'
but this adds constraints @(Tuple.Undefined c, Tuple.Phi c)@.
-}
replicateControlledAlt ::
   (Tuple.Undefined a, Tuple.Phi a) =>
   (Tuple.Undefined c, Tuple.Phi c) =>
   Exp Word -> T (c,a) a -> T (c,a) a
replicateControlledAlt n proc =
   replicateSerial n (arr fst &&& proc) >>^ snd

replicateParallel ::
   (Tuple.Undefined b, Tuple.Phi b) =>
   Exp Word -> Sig.T b -> T (b,b) b -> T a b -> T a b
replicateParallel n z cum p =
   replicateControlled n (cum <<< first p) $> z


quantizeLift ::
   (Memory.C b, Marshal.C c, MultiValue.IntegerConstant c,
    MultiValue.Additive c, MultiValue.Comparison c) =>
   T a b -> T (MultiValue.T c, a) b
quantizeLift (Cons next start stop) = Cons
   (\global local (k, a0) yState0 -> do
      (yState1, frac1) <-
         MaybeCont.fromBool $
         C.whileLoop
            (LLVM.valueOf True, yState0)
            (\(cont1, (_, frac0)) ->
               LLVM.and cont1 . unbool
                  =<< MultiValue.cmp LLVM.CmpLE frac0 A.zero)
            (\(_,((_,state01), frac0)) ->
               MaybeCont.toBool $ liftA2 (,)
                  (next global local a0 state01)
                  (MaybeCont.lift $ A.add frac0 k))

      frac2 <- MaybeCont.lift $ A.sub frac1 A.one
      return (fst yState1, (yState1, frac2)))
{- using this initialization code we would not need undefined values
   (do (global,s) <- start
       (a,_) <- next s
       return (global, ((a,s), A.zero))
-}
   (do
      (global,s) <- start
      return (global, ((Tuple.undef, s), A.zero)))
   stop
