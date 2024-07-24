{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Synthesizer.LLVM.Filter.NonRecursive (
   convolve,
   convolvePacked,
   ) where

import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Generator.Source as Source
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.RingBuffer as RingBuffer
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial

import qualified Synthesizer.Causal.Class as CausalClass
import Synthesizer.Causal.Class (($<))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Control as C
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Foreign.Ptr (Ptr)
import Data.Word (Word)

import Control.Arrow ((<<<), (&&&))
import Control.Monad (liftM2)

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{-
This is a brute-force implementation.
No Karatsuba, No Toom-Cook, No Fourier.
-}
convolve ::
   (Storable.C a, Marshal.C a, MultiValue.PseudoRing a, MultiValue.T a ~ am) =>
   Exp (Source.StorableVector a) -> Causal.T am am
convolve mask =
   let len = Source.storableVectorLength mask
   in (CausalPriv.zipWith (\(MultiValue.Cons l) -> scalarProduct l)
         $< Sig.constant len)
      <<<
      Causal.track Expr.zero len &&& provideMask mask

convolvePacked ::
   (Marshal.Vector n a, MultiVector.PseudoRing a) =>
   (Storable.C a, MultiValue.PseudoRing a, Serial.Value n a ~ v) =>
   Exp (Source.StorableVector a) -> Causal.T v v
convolvePacked = convolvePackedAux TypeNum.singleton

convolvePackedAux ::
   (Marshal.Vector n a, MultiVector.PseudoRing a) =>
   (Storable.C a, MultiValue.PseudoRing a, Serial.Value n a ~ v) =>
   TypeNum.Singleton n -> Exp (Source.StorableVector a) -> Causal.T v v
convolvePackedAux vectorSize mask =
   let len = Source.storableVectorLength mask
   in (CausalPriv.zipWith (\(MultiValue.Cons l) -> scalarProductPacked l)
         $< Sig.constant len)
      <<<
      Causal.track Expr.zero
         (divUp (TypeNum.integralFromSingleton vectorSize) len)
      &&&
      provideMask mask

divUp :: Exp Word -> Exp Word -> Exp Word
divUp k n = Expr.idiv (n+(k-1)) k

provideMask ::
   (Storable.C a) =>
   Exp (Source.StorableVector a) -> Causal.T x (LLVM.Value (Ptr a))
provideMask mask =
   CausalClass.fromSignal $
   fmap (\(MultiValue.Cons (ptr,_l)) -> ptr) $
   Sig.constant mask


scalarProduct ::
   (Storable.C a, Marshal.C a, MultiValue.T a ~ am, MultiValue.PseudoRing a) =>
   LLVM.Value Word ->
   (RingBuffer.T am, LLVM.Value (Ptr a)) ->
   LLVM.CodeGenFunction r am
scalarProduct n (rb,mask) =
   fmap snd $
   Storable.arrayLoop n mask (A.zero, A.zero) $ \ptr (k, s) -> do
      a <- RingBuffer.index k rb
      b <- Storable.load ptr
      liftM2 (,) (A.inc k) (A.add s =<< A.mul a b)


scalarProductPacked ::
   (Storable.C a, Marshal.Vector n a, MultiVector.PseudoRing a) =>
   LLVM.Value Word ->
   (RingBuffer.T (Serial.Value n a), LLVM.Value (Ptr a)) ->
   LLVM.CodeGenFunction r (Serial.Value n a)
scalarProductPacked n0 (rb,mask0) = do
   (ax, rx) <- readSerialStart rb
   bx <- Storable.load mask0
   sx <- Serial.scale bx ax
   n1 <- A.dec n0
   mask1 <- Storable.incrementPtr mask0
   fmap snd $ Storable.arrayLoop n1 mask1 (rx, sx) $ \ptr (r1, s1) -> do
      (a,r2) <- readSerialNext rb r1
      b <- Storable.load ptr
      fmap ((,) r2) (A.add s1 =<< Serial.scale b a)


type
   Iterator n a =
      ((Serial.Value n a,
        {-
        I would like to use Serial.Iterator,
        but we need to read in reversed order,
        that is, from high to low indices.
        -}
        Serial.Value n a,
        LLVM.Value Word),
       LLVM.Value Word)

readSerialStart ::
   (TypeNum.Positive n, Marshal.Vector n a) =>
   RingBuffer.T (Serial.Value n a) ->
   LLVM.CodeGenFunction r (Serial.Value n a, Iterator n a)
readSerialStart rb = do
   a <- RingBuffer.index A.zero rb
   return (a, ((a, Tuple.undef, A.zero), A.zero))

readSerialNext ::
   (MultiValue.C a, Marshal.Vector n a) =>
   RingBuffer.T (Serial.Value n a) ->
   Iterator n a ->
   LLVM.CodeGenFunction r (Serial.Value n a, Iterator n a)
readSerialNext rb ((a0,r0,j0), k0) = do
   vectorEnd <- A.cmp LLVM.CmpEQ j0 A.zero
   ((r1,j1), k1) <-
      C.ifThen vectorEnd ((r0,j0), k0) $ do
         k <- A.inc k0
         r <- RingBuffer.index k rb
         return ((r, LLVM.valueOf (Serial.size r :: Word)), k)
   j2 <- A.dec j1
   (ai,r2) <- Serial.shiftUp Tuple.undef r1
   (_, a1) <- Serial.shiftUp ai a0
   return (a1, ((a1,r2,j2), k1))
