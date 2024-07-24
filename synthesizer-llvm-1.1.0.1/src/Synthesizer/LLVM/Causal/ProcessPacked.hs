{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Causal.ProcessPacked where

import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as SerialCode
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as SerialClass
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame as Frame

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.MaybeContinuation as Maybe
import qualified LLVM.Extra.Control as C
import qualified LLVM.Extra.Arithmetic as A

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal ((:<:))
import Type.Base.Proxy (Proxy)

import qualified LLVM.Core as LLVM

import qualified Control.Arrow as Arrow
import qualified Control.Category as Cat
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.State as MS
import Control.Arrow ((<<<))

import Data.Tuple.HT (swap)
import Data.Word (Word)

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (map, zipWith, takeWhile)
import Prelude ()


type Serial n a = MultiValue.T (Serial.T n a)


{- |
Run a scalar process on packed data.
If the signal length is not divisible by the chunk size,
then the last chunk is dropped.
-}
pack ::
   (SerialClass.Read  va, n ~ SerialClass.Size va, a ~ SerialClass.Element va,
    SerialClass.Write vb, n ~ SerialClass.Size vb, b ~ SerialClass.Element vb)
   =>
   Causal.T a b -> Causal.T va vb
pack (CausalPriv.Cons next start stop) = CausalPriv.Cons
   (\global local a s -> do
      r <- Maybe.lift $ SerialClass.readStart a
      ((_,w2),(_,s2)) <-
         Maybe.fromBool $
         C.whileLoop
            (LLVM.valueOf True,
             let w = Tuple.undef
             in ((r,w),
                 (LLVM.valueOf (SerialClass.sizeOfIterator w :: Word), s)))
            (\(cont,(_rw0,(i0,_s0))) ->
               A.and cont =<< A.cmp LLVM.CmpGT i0 A.zero)
            (\(_,((r0,w0),(i0,s0))) -> Maybe.toBool $ do
               (ai,r1) <- Maybe.lift $ SerialClass.readNext r0
               (bi,s1) <- next global local ai s0
               Maybe.lift $ do
                  w1 <- SerialClass.writeNext bi w0
                  i1 <- A.dec i0
                  return ((r1,w1),(i1,s1)))
      b <- Maybe.lift $ SerialClass.writeStop w2
      return (b, s2))
   start
   stop

{- |
Like 'pack' but duplicates the code for the scalar process.
That is, for vectors of size n,
the code for the scalar causal process will be written n times.
This is efficient only for simple input processes.
-}
packSmall ::
   (SerialClass.Read  va, n ~ SerialClass.Size va, a ~ SerialClass.Element va,
    SerialClass.Write vb, n ~ SerialClass.Size vb, b ~ SerialClass.Element vb)
   =>
   Causal.T a b -> Causal.T va vb
packSmall (CausalPriv.Cons next start stop) = CausalPriv.Cons
   (\global local a ->
      MS.runStateT $
         MT.lift . Maybe.lift . SerialClass.assemble
         =<<
         mapM (MS.StateT . next global local)
         =<<
         (MT.lift $ Maybe.lift $ SerialClass.dissect a))
   start
   stop


raise ::
   (TypeNum.Positive n, MultiVector.Additive a) =>
   Exp a -> Causal.T (Serial n a) (Serial n a)
raise x =
   CausalPriv.map
      (\y -> Expr.unExp (Serial.upsample x) >>= flip Frame.mix y)

amplify ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   Exp a -> Causal.T (Serial n a) (Serial n a)
amplify x =
   CausalPriv.map
      (\y -> Expr.unExp (Serial.upsample x) >>= flip Frame.amplifyMono y)

amplifyStereo ::
   (TypeNum.Positive n, MultiVector.PseudoRing a) =>
   Exp a -> Causal.T (Stereo.T (Serial n a)) (Stereo.T (Serial n a))
amplifyStereo x =
   CausalPriv.map
      (\y -> Expr.unExp (Serial.upsample x) >>= flip Frame.amplifyStereo y)


delay1 ::
   (LLVM.Positive n, Marshal.C a,
    MultiVector.C a, SerialCode.Value n a ~ v) =>
   Exp a -> Causal.T v v
delay1 initial =
   Causal.loop initial $
   Causal.map (swap . uncurry Serial.shiftUp . swap)

differentiate ::
   (LLVM.Positive n, Marshal.C a,
    MultiVector.Additive a, SerialCode.Value n a ~ v) =>
   Exp a -> Causal.T v v
differentiate initial = Cat.id - delay1 initial

integrate ::
   (LLVM.Positive n, Marshal.C a,
    MultiVector.Additive a, SerialCode.Value n a ~ v) =>
   Exp a -> Causal.T v v
integrate =
   Causal.mapAccum (\a acc0 -> swap $ Serial.cumulate acc0 a)


osciCore ::
   (TypeNum.Positive n, Marshal.C t, MultiVector.Fraction t) =>
   Causal.T (Serial n t, Serial n t) (Serial n t)
osciCore =
   CausalPriv.zipWith A.addToPhase <<<
   Arrow.second
      (Causal.mapAccum
         (\a phase0 ->
            let (phase1,b1) = Serial.cumulate phase0 a
            in (b1, Expr.liftM A.signedFraction phase1))
         Expr.zero)

osci ::
   (TypeNum.Positive n, Marshal.C t, MultiVector.Fraction t) =>
   (forall r. Serial n t -> LLVM.CodeGenFunction r y) ->
   Causal.T (Serial n t, Serial n t) y
osci wave = CausalPriv.map wave <<< osciCore

shapeModOsci ::
   (TypeNum.Positive n, Marshal.C t, MultiVector.Fraction t) =>
   (forall r. c -> Serial n t -> LLVM.CodeGenFunction r y) ->
   Causal.T (c, (Serial n t, Serial n t)) y
shapeModOsci wave = CausalPriv.zipWith wave <<< Arrow.second osciCore


arrayElement ::
   (TypeNum.Positive n,
    MultiVector.C a, Marshal.C a,
    Marshal.Struct a ~ aStruct, LLVM.IsFirstClass aStruct,
    TypeNum.Natural i, TypeNum.Natural d, i :<: d) =>
   Proxy i -> Causal.T (MultiValue.T (MultiValue.Array d a)) (Serial n a)
arrayElement i = Causal.map Serial.upsample <<< Causal.arrayElement i
