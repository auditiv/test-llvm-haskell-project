{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Synthesizer.LLVM.EventIterator where

import qualified Data.EventList.Relative.BodyTime as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Core as LLVM

import Foreign.StablePtr
          (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word)

import Control.Monad ((<=<))

import qualified LLVM.DSL.Debug.StablePtr as DebugStable


{-
For problems on constraints, see ChunkIterator.
-}
data T a = (Marshal.C a) => Cons (IORef (EventList.T NonNeg.Int a))

type MarshalPtr a = LLVM.Ptr (Marshal.Struct a)


foreign import ccall "&nextConstantExp"
   nextCallBack :: FunPtr (StablePtr (T a) -> MarshalPtr a -> IO Word)

foreign export ccall "nextConstantExp"
   next :: StablePtr (T a) -> MarshalPtr a -> IO Word


{- |
Events with subsequent duration 0 are ignored
(and for performance reasons it should not contain too many small values,
say below 100).
-}
new :: (Marshal.C a) => EventList.T NonNeg.Int a -> IO (StablePtr (T a))
new evs =
   DebugStable.trace "new" =<<
   newStablePtr . Cons
    =<< newIORef
      (EventList.fromPairList $
       filter ((/=0) . snd) $
       EventList.toPairList evs)

dispose :: StablePtr (T a) -> IO ()
dispose = freeStablePtr <=< DebugStable.trace "dispose"

next :: StablePtr (T a) -> MarshalPtr a -> IO Word
next stable eventPtr =
   DebugStable.trace "next" stable >>=
   deRefStablePtr >>= \state ->
   case state of
      Cons listRef ->
         readIORef listRef >>=
         EventList.switchL
            (return 0)
            (\body time xs ->
               writeIORef listRef xs >>
               Marshal.poke eventPtr body >>
               return (fromIntegral time))
