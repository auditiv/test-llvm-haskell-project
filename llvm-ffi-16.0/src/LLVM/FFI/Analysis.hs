{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module LLVM.FFI.Analysis where

import qualified LLVM.FFI.Core as LLVM
import LLVM.FFI.Core (ModuleRef, ValueRef)

import qualified Foreign.C.Types as C
import Foreign.C.String(CString)
import Foreign.Ptr(Ptr)


type VerifierFailureAction = C.CInt

foreign import ccall unsafe "LLVMVerifyFunction" verifyFunction
    :: ValueRef -> VerifierFailureAction -> IO LLVM.Bool
foreign import ccall unsafe "LLVMVerifyModule" verifyModule
    :: ModuleRef -> VerifierFailureAction -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMViewFunctionCFG" viewFunctionCFG
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMViewFunctionCFGOnly" viewFunctionCFGOnly
    :: ValueRef -> IO ()
