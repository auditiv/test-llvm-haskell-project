{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module LLVM.FFI.BitReader where

import qualified LLVM.FFI.Core as LLVM
import LLVM.FFI.Core (MemoryBufferRef, ModuleRef, ContextRef)

import Foreign.C.String(CString)
import Foreign.Ptr(Ptr)


foreign import ccall unsafe "LLVMParseBitcode" parseBitcode
    :: MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMParseBitcodeInContext" parseBitcodeInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMGetBitcodeModule" getBitcodeModule
    :: MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMGetBitcodeModuleInContext" getBitcodeModuleInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO LLVM.Bool
