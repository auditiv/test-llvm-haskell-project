{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module LLVM.FFI.BitWriter where

import LLVM.FFI.Core (ModuleRef)

import qualified Foreign.C.Types as C
import Foreign.C.String(CString)

type CInt = C.CInt


foreign import ccall unsafe "LLVMWriteBitcodeToFile" writeBitcodeToFile
    :: ModuleRef -> CString -> IO CInt
foreign import ccall unsafe "LLVMWriteBitcodeToFileHandle" writeBitcodeToFileHandle
    :: ModuleRef -> CInt -> IO CInt
foreign import ccall unsafe "LLVMWriteBitcodeToFD" writeBitcodeToFD
    :: ModuleRef -> CInt -> CInt -> CInt -> IO CInt
