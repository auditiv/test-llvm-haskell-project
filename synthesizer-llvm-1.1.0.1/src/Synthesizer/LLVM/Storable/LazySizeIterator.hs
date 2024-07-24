{-# LANGUAGE ForeignFunctionInterface #-}
module Synthesizer.LLVM.Storable.LazySizeIterator where

import qualified Numeric.NonNegative.Chunky  as Chunky
import qualified Data.StorableVector.Lazy.Pattern as SVP
import qualified Data.StorableVector.Lazy as SVL

import Data.Word (Word)

import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List.HT as ListHT


newtype T = Cons (IORef [SVL.ChunkSize])

{-
For problems about Storable constraint, see ChunkIterator.
-}
foreign import ccall "&nextSize"
   nextCallBack :: FunPtr (StablePtr T -> IO Word)

foreign export ccall "nextSize"
   next :: StablePtr T -> IO Word


new :: SVP.LazySize -> IO (StablePtr T)
new ls =
   newStablePtr . Cons =<< newIORef (Chunky.toChunks (Chunky.normalize ls))

dispose :: StablePtr T -> IO ()
dispose = freeStablePtr

{- |
Zero pieces are filtered out.
If 'next' returns 0 then the end of the lazy size is reached.
-}
next :: StablePtr T -> IO Word
next stable =
   deRefStablePtr stable >>= \state ->
   case state of
      Cons listRef ->
         readIORef listRef >>=
         ListHT.switchL
            (return 0)
            (\(SVL.ChunkSize time) xs ->
               writeIORef listRef xs >>
               return (fromIntegral time))
