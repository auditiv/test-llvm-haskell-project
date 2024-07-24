{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.FFI.Transforms.Vectorize where

import LLVM.FFI.Core (PassManagerRef)


foreign import ccall unsafe "LLVMAddLoopVectorizePass" addLoopPass
    :: PassManagerRef -> IO ()

foreign import ccall unsafe "LLVMAddSLPVectorizePass" addSLPPass
    :: PassManagerRef -> IO ()
