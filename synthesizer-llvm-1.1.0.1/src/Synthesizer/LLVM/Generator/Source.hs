{-# LANGUAGE TypeFamilies #-}
module Synthesizer.LLVM.Generator.Source where

import qualified Synthesizer.LLVM.Storable.ChunkIterator as ChunkIt
import qualified Synthesizer.LLVM.Storable.LazySizeIterator as SizeIt
import qualified Synthesizer.LLVM.Generator.Private as Sig
import qualified Synthesizer.LLVM.ConstantPiece as Const
import qualified Synthesizer.LLVM.EventIterator as EventIt
import Synthesizer.LLVM.Private (noLocalPtr)

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Control as C

import qualified LLVM.Core as LLVM

import Foreign.Storable (Storable)
import Foreign.StablePtr (StablePtr)
import Foreign.Ptr (Ptr, nullPtr)

import Control.Applicative (liftA2, (<$>))

import Data.Tuple.HT (mapSnd)
import Data.Word (Word)


type T a = Sig.T (MultiValue.T a)


data StorableVector a = StorableVector (Ptr a) Word

storableVectorLength :: Exp (StorableVector a) -> Exp Word
storableVectorLength = Expr.lift1 (MultiValue.lift1 (\(_ptr,l) -> l))

consStorableVector :: Ptr a -> Int -> StorableVector a
consStorableVector p = StorableVector p . fromIntegral

instance (Storable a) => MultiValue.C (StorableVector a) where
   type Repr (StorableVector a) = (LLVM.Value (Ptr a), LLVM.Value Word)
   cons (StorableVector p l) = MultiValue.Cons (LLVM.valueOf p, LLVM.valueOf l)
   undef = MultiValue.undefTuple
   zero = MultiValue.zeroTuple
   phi = MultiValue.phiTuple
   addPhi = MultiValue.addPhiTuple

instance (Storable a) => Marshal.C (StorableVector a) where
   pack (StorableVector p l) = LLVM.consStruct p l
   unpack = LLVM.uncurryStruct StorableVector

storableVector :: (Storable.C a) => Exp (StorableVector a) -> T a
storableVector vec =
   Sig.noGlobal
      (noLocalPtr $ \(p0,l0) -> do
         cont <- MaybeCont.lift $ A.cmp LLVM.CmpGT l0 A.zero
         MaybeCont.withBool cont $ do
            y1 <- Storable.load p0
            p1 <- Storable.incrementPtr p0
            l1 <- A.dec l0
            return (y1,(p1,l1)))
      (fmap (\(MultiValue.Cons (p,l)) -> (p,l)) (Expr.unExp vec))


{-
This function calls back into the Haskell function 'ChunkIt.next'
that returns a pointer to the data of the next chunk
and advances to the next chunk in the sequence.
-}
storableVectorLazy ::
   (Storable.C a) => Exp (StablePtr (ChunkIt.T a)) -> T a
storableVectorLazy = flattenChunks . storableVectorChunks

type Chunk a = (LLVM.Value (Ptr a), LLVM.Value Word)

storableVectorChunks ::
   (Storable.C a) => Exp (StablePtr (ChunkIt.T a)) -> Sig.T (Chunk a)
storableVectorChunks sig =
   Sig.Cons
      (\stable lenPtr () -> MaybeCont.fromBool $ do
         nextChunkFn <-
            LLVM.staticNamedFunction
               "SignalExp.fromStorableVectorLazy.nextChunk"
               ChunkIt.nextCallBack
         (buffer,len) <-
            liftA2 (,)
               (LLVM.call nextChunkFn stable lenPtr)
               (LLVM.load lenPtr)
         valid <- A.cmp LLVM.CmpNE buffer (LLVM.valueOf nullPtr)
         return (valid, ((buffer,len), ())))
      (fmap (\(MultiValue.Cons it) -> (it, ())) $ Expr.unExp sig)
      (\ _it -> return ())

flattenChunks :: (Storable.C a) => Sig.T (Chunk a) -> T a
flattenChunks (Sig.Cons next start stop) =
   Sig.Cons
      (\global local ((buffer0,length0), state0) -> do
         ((buffer1,length1), state1) <- MaybeCont.fromBool $ do
            needNext <- A.cmp LLVM.CmpEQ length0 A.zero
            C.ifThen needNext
               (LLVM.valueOf True, ((buffer0,length0), state0))
               (MaybeCont.toBool $ next global local state0)
         MaybeCont.lift $ do
            x <- Storable.load buffer1
            buffer2 <- Storable.incrementPtr buffer1
            length2 <- A.dec length1
            return (x, ((buffer2,length2), state1)))
      (mapSnd ((,) (LLVM.valueOf nullPtr, A.zero)) <$> start)
      stop


eventList ::
   (Marshal.C a) =>
   Exp (StablePtr (EventIt.T a)) -> Sig.T (Const.T (MultiValue.T a))
eventList sig =
   Sig.Cons
      -- FixMe: duplicate of ConstantPiece.piecewiseConstant
      (\stable yPtr () -> do
         len <- MaybeCont.lift $ do
            nextFn <-
               LLVM.staticNamedFunction
                  "ConstantPiece.piecewiseConstant.nextChunk"
                  EventIt.nextCallBack
            LLVM.call nextFn stable yPtr
         MaybeCont.guard =<< MaybeCont.lift (A.cmp LLVM.CmpNE len A.zero)
         y <- MaybeCont.lift $ Memory.load yPtr
         return (Const.Cons len y, ()))
      (fmap (\(MultiValue.Cons it) -> (it, ())) $ Expr.unExp sig)
      (\ _it -> return ())

lazySize :: Exp (StablePtr SizeIt.T) -> Sig.T (Const.T ())
lazySize size = Sig.Cons
   (\stable -> noLocalPtr $ \() -> do
      len <- MaybeCont.lift $ do
         nextFn <-
            LLVM.staticNamedFunction
               "ConstantPiece.lazySize.next"
               SizeIt.nextCallBack
         LLVM.call nextFn stable
      MaybeCont.guard =<< MaybeCont.lift (A.cmp LLVM.CmpNE len A.zero)
      return (Const.Cons len (), ()))
   (fmap (\(MultiValue.Cons it) -> (it, ())) $ Expr.unExp size)
   (\ _it -> return ())
