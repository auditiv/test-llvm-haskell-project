{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module LLVM.FFI.Transforms.IPO where

import LLVM.FFI.Core (PassManagerRef)


foreign import ccall unsafe "LLVMAddConstantMergePass" addConstantMergePass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddDeadArgEliminationPass" addDeadArgEliminationPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddFunctionAttrsPass" addFunctionAttrsPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddFunctionInliningPass" addFunctionInliningPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddAlwaysInlinerPass" addAlwaysInlinerPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddGlobalDCEPass" addGlobalDCEPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddGlobalOptimizerPass" addGlobalOptimizerPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddIPSCCPPass" addIPSCCPPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddInternalizePass" addInternalizePass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddStripDeadPrototypesPass" addStripDeadPrototypesPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddStripSymbolsPass" addStripSymbolsPass
    :: PassManagerRef -> IO ()
