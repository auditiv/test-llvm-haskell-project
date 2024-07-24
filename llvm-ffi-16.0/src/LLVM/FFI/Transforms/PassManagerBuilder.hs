{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.Transforms.PassManagerBuilder where

import qualified LLVM.FFI.Core as LLVM
import LLVM.FFI.Core (PassManagerRef)

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)


type CUInt = C.CUInt


data PassManagerBuilder
    deriving (Typeable)
type PassManagerBuilderRef = Ptr PassManagerBuilder


foreign import ccall unsafe "LLVMPassManagerBuilderCreate" create
    :: IO PassManagerBuilderRef

foreign import ccall unsafe "LLVMPassManagerBuilderDispose" dispose
    :: PassManagerBuilderRef -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetOptLevel" setOptLevel
    :: PassManagerBuilderRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetSizeLevel" setSizeLevel
    :: PassManagerBuilderRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableUnitAtATime" setDisableUnitAtATime
    :: PassManagerBuilderRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableUnrollLoops" setDisableUnrollLoops
    :: PassManagerBuilderRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableSimplifyLibCalls" setDisableSimplifyLibCalls
    :: PassManagerBuilderRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderUseInlinerWithThreshold" useInlinerWithThreshold
    :: PassManagerBuilderRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateFunctionPassManager" populateFunctionPassManager
    :: PassManagerBuilderRef -> PassManagerRef -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateModulePassManager" populateModulePassManager
    :: PassManagerBuilderRef -> PassManagerRef -> IO ()
