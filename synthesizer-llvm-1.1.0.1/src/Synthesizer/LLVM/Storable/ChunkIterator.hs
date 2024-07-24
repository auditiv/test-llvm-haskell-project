{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Synthesizer.LLVM.Storable.ChunkIterator where

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB

import qualified LLVM.Core as LLVM

import Data.Word (Word)
import Foreign.Storable (Storable, poke)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)

import Control.Monad (liftM2)

import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)


{-
FFI declarations must not have constraints.
Thus we put them in the iterator datatype.
-}
data T a = (Storable a) => Cons (IORef [SVB.Vector a]) (IORef (SVB.Vector a))


foreign import ccall "&nextChunk"
   nextCallBack :: FunPtr (StablePtr (T a) -> LLVM.Ptr Word -> IO (Ptr a))

foreign export ccall "nextChunk"
   next :: StablePtr (T a) -> Ptr Word -> IO (Ptr a)


new :: (Storable a) => SVL.Vector a -> IO (StablePtr (T a))
new sig =
   newStablePtr =<<
   liftM2 Cons
      (newIORef (SVL.chunks sig))
      (newIORef (error "first chunk must be fetched with nextChunk"))

dispose :: StablePtr (T a) -> IO ()
dispose = freeStablePtr

next :: StablePtr (T a) -> Ptr Word -> IO (Ptr a)
next stable lenPtr =
   deRefStablePtr stable >>= \state ->
   case state of
      Cons listRef chunkRef -> do
         xt <- readIORef listRef
         case xt of
            [] -> return nullPtr
            (x:xs) ->
               {- We have to maintain a pointer to the current chunk
                  in order to protect it against garbage collection -}
               writeIORef chunkRef x >>
               writeIORef listRef xs >>
               SVB.withStartPtr x
                  (\p l -> poke lenPtr (fromIntegral l) >> return p)
