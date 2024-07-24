{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
Adding the finalizer to a ForeignPtr seems to be the only way
that warrants execution of the finalizer (not too early and not never).
However, the normal ForeignPtr finalizers must be independent from Haskell runtime.
In contrast to ForeignPtr finalizers,
addFinalizer adds finalizers to boxes, that are optimized away.
Thus finalizers are run too early or not at all.
Concurrent.ForeignPtr and using threaded execution
is the only way to get finalizers in Haskell IO.
-}
module Synthesizer.LLVM.ForeignPtr where

import qualified LLVM.DSL.Execution as Exec
import qualified LLVM.Extra.Multi.Value.Marshal as MarshalMV
import qualified LLVM.Extra.Marshal as Marshal
import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Core as LLVM

import qualified Foreign.ForeignPtr as FPtr
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.StablePtr (newStablePtr, freeStablePtr)
import Foreign.Ptr (nullPtr)


newAux :: IO () -> IO (ForeignPtr ())
newAux = FC.newForeignPtr nullPtr


makeFinalizer :: (EE.ExecutionEngine, IO ()) -> IO (IO ())
makeFinalizer (ee, finalizer) = do
   stable <- newStablePtr ee
   return $ finalizer >> freeStablePtr stable

type MemoryPtr struct = ForeignPtr (EE.Stored struct)

newInit :: Exec.Finalizer a -> IO (LLVM.Ptr a) -> IO (MemoryPtr a)
newInit (ee, stop) start = do
   state <- start
   FC.newForeignPtr (EE.castToStoredPtr state)
      =<< makeFinalizer (ee, stop state)

newParam ::
   (Marshal.C b) =>
   Exec.Finalizer a ->
   (LLVM.Ptr (Marshal.Struct b) -> IO (LLVM.Ptr a)) ->
   b -> IO (MemoryPtr a)
newParam stop start b =
   newInit stop (Marshal.with b start)

newParamMV ::
   (MarshalMV.C b) =>
   Exec.Finalizer a ->
   (LLVM.Ptr (MarshalMV.Struct b) -> IO (LLVM.Ptr a)) ->
   b -> IO (MemoryPtr a)
newParamMV stop start b =
   newInit stop (MarshalMV.with b start)

new ::
   (Marshal.C a, Marshal.Struct a ~ struct) =>
   IO () -> a -> IO (MemoryPtr struct)
new finalizer a = do
   ptr <- FPtr.mallocForeignPtr
   FC.addForeignPtrFinalizer ptr finalizer
   with ptr $ flip Marshal.poke a
   return ptr


with :: MemoryPtr struct -> (LLVM.Ptr struct -> IO a) -> IO a
with fptr act = withForeignPtr fptr $ act . EE.castFromStoredPtr
