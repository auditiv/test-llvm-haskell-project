{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- |
Functions on storable vectors that are implemented using LLVM.
-}
module Synthesizer.LLVM.Storable.Signal (
   unpackStrict, unpack,
   unpackStereoStrict, unpackStereo,
   makeReversePackedStrict, makeReversePacked,
   continue, continuePacked, continuePackedGeneric,
   fillBuffer, makeMixer,
   makeArranger,
   ) where

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoVector
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Absolute.TimeBody  as AbsEventList
import qualified Number.NonNegative as NonNeg

import qualified LLVM.DSL.Execution as Exec
import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Control.Monad.HT (void)

import Foreign.Marshal.Array (advancePtr)
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)

import qualified System.Unsafe as Unsafe


{- |
This function needs only constant time
in contrast to 'Synthesizer.LLVM.Parameterized.SignalPacked.unpack'.

We cannot provide a 'pack' function
since the array size may not line up.
It would also need copying since the source data may not be aligned properly.
-}
unpackChunk ::
   (Storable.C a, TypeNum.Positive n) =>
   SV.Vector (Serial.T n a) -> SV.Vector a
unpackChunk v =
   let getDim ::
          (TypeNum.Positive n) =>
          SV.Vector (Serial.T n a) -> TypeNum.Singleton n -> Int
       getDim _ = TypeNum.integralFromSingleton
       d = getDim v TypeNum.singleton
       (fptr,s,l) = SVB.toForeignPtr v
   in  SVB.SV (castForeignPtr fptr) (s*d) (l*d)


unpackStrict ::
   (TypeNum.Positive n, Storable.Vector a) =>
   SV.Vector (Serial.T n a) -> SV.Vector a
unpackStrict = unpackChunk

unpack ::
   (TypeNum.Positive n, Storable.Vector a) =>
   SVL.Vector (Serial.T n a) -> SVL.Vector a
unpack = SVL.fromChunks . map unpackChunk . SVL.chunks


unpackStereoStrict ::
   (TypeNum.Positive n, Storable.C a) =>
   SV.Vector (StereoVector.T n a) -> SV.Vector (Stereo.T a)
unpackStereoStrict v =
   let getDim ::
          (TypeNum.Positive n) =>
          SV.Vector (StereoVector.T n a) -> TypeNum.Singleton n -> Int
       getDim _ = TypeNum.integralFromSingleton
       d = getDim v TypeNum.singleton
       (fptr,s,l) = SVB.toForeignPtr v
   in  SVB.SV (castForeignPtr fptr) (s*d) (l*d)

unpackStereo ::
   (TypeNum.Positive n, Storable.C a) =>
   SVL.Vector (StereoVector.T n a) -> SVL.Vector (Stereo.T a)
unpackStereo =
   SVL.fromChunks . map unpackStereoStrict . SVL.chunks


makeReverser ::
   (Storable.C a, MultiValue.T a ~ value) =>
   (value -> LLVM.CodeGenFunction () value) ->
   IO (Word -> Ptr a -> Ptr a -> IO ())
makeReverser rev =
   Exec.compile "reverse" $
   Exec.createFunction derefMixPtr "reverse" $ \ size ptrA ptrB -> do
      sizeInt <- LLVM.bitcast size
      ptrAEnd <- Storable.advancePtr sizeInt ptrA
      void $ Storable.arrayLoop size ptrB ptrAEnd $ \ ptrBi ptrAj0 -> do
         ptrAj1 <- Storable.decrementPtr ptrAj0
         flip Storable.store ptrBi
            =<< rev
            =<< Storable.load ptrAj1
         return ptrAj1

makeReversePackedStrict ::
   (TypeNum.Positive n, Storable.Vector a, v ~ Serial.T n a) =>
   IO (SV.Vector v -> SV.Vector v)
makeReversePackedStrict = do
   rev <- makeReverser Serial.reverse
   return $ \v ->
      Unsafe.performIO $
      SVB.withStartPtr v $ \ptrA len ->
      SVB.create len $ \ptrB ->
      rev (fromIntegral len) ptrA ptrB

makeReversePacked ::
   (TypeNum.Positive n, Storable.Vector a, v ~ Serial.T n a) =>
   IO (SVL.Vector v -> SVL.Vector v)
makeReversePacked =
   fmap (\f -> SVL.fromChunks . reverse . map f . SVL.chunks) $
   makeReversePackedStrict


-- ToDo: move to synthesizer-core or storablevector
{- |
Append two signals where the second signal
gets the last value of the first signal as parameter.
If the first signal is empty
then there is no parameter for the second signal
and thus we simply return an empty signal in that case.
-}
continue ::
   (Storable a) =>
   SVL.Vector a -> (a -> SVL.Vector a) -> SVL.Vector a
continue x y =
   SVL.fromChunks $
   withLast SV.empty
      (SVL.chunks x)
      (SV.switchR [] $ \_ -> SVL.chunks . y)

continuePacked ::
   (TypeNum.Positive n, Storable.Vector a) =>
   SVL.Vector (Serial.T n a) ->
   (a -> SVL.Vector (Serial.T n a)) ->
   SVL.Vector (Serial.T n a)
continuePacked x y =
   SVL.fromChunks $
   withLast SV.empty
      (SVL.chunks x)
      (SV.switchR [] (\_ -> SVL.chunks . y) . unpackStrict)

{-
This function reduces the last chunk to size one, repacks that
and takes the last value.
It would be certainly more efficient to use
a single @Memory.load@, @extractelement@ and @store@
instead of a loop of count 1.
However, this implementation is the simplest one, so far.
-}
{- |
Use this like

> do unpackGeneric <- makeUnpackGenericStrict
>    return (continuePackedGeneric unpackGeneric x y)
-}
continuePackedGeneric ::
   (Storable v, Storable a) =>
   (SV.Vector v -> SV.Vector a) ->
   SVL.Vector v -> (a -> SVL.Vector v) -> SVL.Vector v
continuePackedGeneric unpackGeneric x y =
   SVL.fromChunks $
   withLast SV.empty
      (SVL.chunks x)
      (\lastChunk ->
         SV.switchR [] (\_ -> SVL.chunks . y) $ unpackGeneric $
         SV.drop (SV.length lastChunk - 1) $ lastChunk)


-- ToDo: candidate for utility-ht
withLast :: a -> [a] -> (a -> [a]) -> [a]
withLast deflt x y =
   foldr
      (\a cont _ -> a : cont a)
      y x deflt


foreign import ccall safe "dynamic" derefFillPtr ::
   Exec.Importer (Word -> Ptr a -> IO ())

{- |
'fillBuffer' is not only more general than filling with zeros,
it also simplifies type inference.
-}
fillBuffer ::
   (Storable.C a, MultiValue.T a ~ value) =>
   value -> IO (Word -> Ptr a -> IO ())
fillBuffer x =
   Exec.compile "constant" $
   Exec.createFunction derefFillPtr "constantfill" $ \ size ptr ->
      Storable.arrayLoop size ptr () $ \ ptri () -> Storable.store x ptri


foreign import ccall safe "dynamic" derefMixPtr ::
   Exec.Importer (Word -> Ptr a -> Ptr a -> IO ())

makeMixer ::
   (Storable.C a, MultiValue.T a ~ value) =>
   (value -> value -> LLVM.CodeGenFunction () value) ->
   IO (Word -> Ptr a -> Ptr a -> IO ())
makeMixer add =
   Exec.compile "mixer" $
   Exec.createFunction derefMixPtr "mix" $ \ size srcPtr dstPtr ->
      void $ Storable.arrayLoop2 size srcPtr dstPtr () $
            \srcPtri dstPtri () -> do
         y <- Storable.load srcPtri
         Storable.modify (add y) dstPtri


addToBuffer ::
   (Storable a) =>
   (Word -> Ptr a -> Ptr a -> IO ()) ->
   Int -> Ptr a -> Int -> SVL.Vector a -> IO (Int, SVL.Vector a)
addToBuffer addChunkToBuffer len v start xs =
   let (now,future) = SVL.splitAt (len - start) xs
       go i [] = return i
       go i (c:cs) =
          SVB.withStartPtr c (\ptr l ->
             addChunkToBuffer (fromIntegral l) ptr (advancePtr v i)) >>
          go (i + SV.length c) cs
   in  fmap (flip (,) future) . go start . SVL.chunks $ now


{-
Same algorithm as in Synthesizer.Storable.Cut.arrangeEquidist
-}
makeArranger ::
   (Storable.C a, MultiValue.Additive a) =>
   IO (SVL.ChunkSize ->
       EventList.T NonNeg.Int (SVL.Vector a) ->
       SVL.Vector a)
makeArranger = do
   mixer <- makeMixer MultiValue.add
   fill <- fillBuffer MultiValue.zero
   return $ \ (SVL.ChunkSize sz) ->
      let sznn = NonNeg.fromNumberMsg "arrange" sz
          go acc evs =
             let (now,future) = EventListTM.splitAtTime sznn evs
                 xs =
                    AbsEventList.toPairList $
                    EventList.toAbsoluteEventList 0 $
                    EventListTM.switchTimeR const now
                 (chunk,newAcc) =
                    Unsafe.performIO $
                    SVB.createAndTrim' sz $ \ptr -> do
                       fill (fromIntegral sz) ptr
                       newAcc0 <- flip mapM acc $ addToBuffer mixer sz ptr 0
                       newAcc1 <- flip mapM xs $ \(i,s) ->
                          addToBuffer mixer sz ptr (NonNeg.toNumber i) s
                       let (ends, suffixes) = unzip $ newAcc0++newAcc1
                           {- if there are more events to come,
                              we must pad with zeros -}
                           len =
                              if EventList.null future
                                then foldl max 0 ends
                                else sz
                       return (0, len,
                               filter (not . SVL.null) suffixes)
             in  if SV.null chunk
                   then []
                   else chunk : go newAcc future
      in  SVL.fromChunks . go []
