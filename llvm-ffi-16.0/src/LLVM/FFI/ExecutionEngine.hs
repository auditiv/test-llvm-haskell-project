{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.ExecutionEngine
    (
    -- * Linking
      linkInInterpreter
    , linkInMCJIT

    -- * Generic values
    , GenericValue
    , GenericValueRef
    , createGenericValueOfInt
    , createGenericValueOfPointer
    , createGenericValueOfFloat
    , genericValueIntWidth
    , genericValueToInt
    , genericValueToPointer
    , genericValueToFloat
    , ptrDisposeGenericValue

    -- * Execution engines
    , ExecutionEngine
    , ExecutionEngineRef
    , EngineKind(..)
    , EngineKindSet
    , kindJIT
    , kindInterpreter
    , kindEither
    , createExecutionEngineKindForModuleCPU
    , createExecutionEngineForModule
    , createExecutionEngineForModuleCPU
    , createInterpreterForModule
    , createInterpreterForModuleCPU
    , createJITCompilerForModule
    , createMCJITCompilerForModule
    , initializeMCJITCompilerOptions
    , ptrDisposeExecutionEngine
    , disposeExecutionEngine
    , runStaticConstructors
    , runStaticDestructors
    , runFunctionAsMain
    , freeMachineCodeForFunction
    , addModule
    , removeModule
    , findFunction
    , recompileAndRelinkFunction
    , runFunction
    , getExecutionEngineTargetData
    , addGlobalMapping
    , addFunctionMapping
    , getPointerToGlobal
    , getPointerToFunction

    ) where

import qualified LLVM.FFI.Core as LLVM
import LLVM.FFI.Core (ModuleRef, TypeRef, ValueRef)
import LLVM.FFI.Target (TargetDataRef)
import LLVM.FFI.Base (FinalizerPtr)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, FunPtr)

import qualified Data.EnumBitSet as EnumSet
import Data.Typeable (Typeable)


type CDouble  = C.CDouble
type CInt     = C.CInt
type CUInt    = C.CUInt
type CULLong  = C.CULLong
type CSize    = C.CSize


data ExecutionEngine
    deriving (Typeable)
type ExecutionEngineRef = Ptr ExecutionEngine

data GenericValue
    deriving (Typeable)
type GenericValueRef = Ptr GenericValue

data MCJITCompilerOptions
    deriving (Typeable)
type MCJITCompilerOptionsRef = Ptr MCJITCompilerOptions

-- ** Linking
foreign import ccall unsafe "LLVMLinkInInterpreter" linkInInterpreter
    :: IO ()
foreign import ccall unsafe "LLVMLinkInMCJIT" linkInMCJIT
    :: IO ()

-- ** Generic values
foreign import ccall unsafe "LLVMCreateGenericValueOfInt"
    createGenericValueOfInt :: TypeRef -> CULLong -> LLVM.Bool
                            -> IO GenericValueRef
foreign import ccall unsafe "LLVMCreateGenericValueOfPointer"
    createGenericValueOfPointer :: Ptr a -> IO GenericValueRef
foreign import ccall unsafe "LLVMCreateGenericValueOfFloat"
    createGenericValueOfFloat :: TypeRef -> CDouble -> IO GenericValueRef
foreign import ccall unsafe "LLVMGenericValueIntWidth" genericValueIntWidth
    :: GenericValueRef -> IO CUInt
foreign import ccall unsafe "LLVMGenericValueToInt" genericValueToInt
    :: GenericValueRef -> LLVM.Bool -> IO CULLong
foreign import ccall unsafe "LLVMGenericValueToPointer" genericValueToPointer
    :: GenericValueRef -> IO (Ptr a)
foreign import ccall unsafe "LLVMGenericValueToFloat" genericValueToFloat
    :: TypeRef -> GenericValueRef -> IO CDouble
foreign import ccall unsafe "&LLVMDisposeGenericValue" ptrDisposeGenericValue
    :: FinalizerPtr GenericValue


data EngineKind
    = JIT
    | Interpreter
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable)

type EngineKindSet = EnumSet.T CUInt EngineKind

kindJIT, kindInterpreter, kindEither :: EngineKindSet
kindJIT = EnumSet.fromEnum JIT
kindInterpreter = EnumSet.fromEnum Interpreter
kindEither = kindJIT EnumSet..|. kindInterpreter

-- ** Execution engines
foreign import ccall unsafe "LLVMCreateExecutionEngineKindForModuleCPU" createExecutionEngineKindForModuleCPU
    :: (Ptr ExecutionEngineRef) -> EngineKindSet -> ModuleRef -> (Ptr CString) -> IO LLVM.Bool
{-# INLINE createExecutionEngineForModuleCPU #-}
createExecutionEngineForModuleCPU
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> (Ptr CString) -> IO LLVM.Bool
createExecutionEngineForModuleCPU ee m outError =
    createExecutionEngineKindForModuleCPU ee kindEither m outError
{-# INLINE createInterpreterForModuleCPU #-}
createInterpreterForModuleCPU
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> (Ptr CString) -> IO LLVM.Bool
createInterpreterForModuleCPU ee m outError =
    createExecutionEngineKindForModuleCPU ee kindInterpreter m outError

foreign import ccall unsafe "LLVMCreateExecutionEngineForModule" createExecutionEngineForModule
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMCreateInterpreterForModule" createInterpreterForModule
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMCreateJITCompilerForModule" createJITCompilerForModule
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> CUInt -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMInitializeMCJITCompilerOptions" initializeMCJITCompilerOptions
    :: MCJITCompilerOptionsRef -> CSize -> IO ()
foreign import ccall unsafe "LLVMCreateMCJITCompilerForModule" createMCJITCompilerForModule
    :: Ptr ExecutionEngineRef -> ModuleRef -> MCJITCompilerOptionsRef -> CSize -> Ptr CString -> IO LLVM.Bool
foreign import ccall unsafe "LLVMDisposeExecutionEngine" disposeExecutionEngine
    :: ExecutionEngineRef -> IO ()
foreign import ccall unsafe "&LLVMDisposeExecutionEngine" ptrDisposeExecutionEngine
    :: FinalizerPtr ExecutionEngine
foreign import ccall unsafe "LLVMRunStaticConstructors" runStaticConstructors
    :: ExecutionEngineRef -> IO ()
foreign import ccall unsafe "LLVMRunStaticDestructors" runStaticDestructors
    :: ExecutionEngineRef -> IO ()
{-
safe call is important, since the running LLVM code may call back into Haskell code

See
http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffise3.html#x6-130003.3 says:

"Optionally, an import declaration can specify,
after the calling  convention,
the safety level that should be used when invoking an external entity.
..."
-}
foreign import ccall safe "LLVMRunFunctionAsMain" runFunctionAsMain
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr CString              -- ^ argv
    -> Ptr CString              -- ^ envp
    -> IO CInt
foreign import ccall safe "LLVMRunFunction" runFunction
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr GenericValueRef -> IO GenericValueRef
foreign import ccall unsafe "LLVMFreeMachineCodeForFunction"
    freeMachineCodeForFunction :: ExecutionEngineRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMAddModule" addModule
    :: ExecutionEngineRef -> ModuleRef -> IO ()
foreign import ccall unsafe "LLVMRemoveModule" removeModule
    :: ExecutionEngineRef -> ModuleRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO LLVM.Bool
foreign import ccall unsafe "LLVMFindFunction" findFunction
    :: ExecutionEngineRef -> CString -> Ptr ValueRef -> IO LLVM.Bool
foreign import ccall unsafe "LLVMRecompileAndRelinkFunction" recompileAndRelinkFunction
    :: ExecutionEngineRef -> ValueRef -> IO (FunPtr a)
foreign import ccall unsafe "LLVMGetExecutionEngineTargetData" getExecutionEngineTargetData
    :: ExecutionEngineRef -> IO TargetDataRef
{- |
disfunctional in LLVM-3.6,
see <https://llvm.org/bugs/show_bug.cgi?id=20656>
-}
foreign import ccall unsafe "LLVMAddGlobalMapping" addGlobalMapping
    :: ExecutionEngineRef -> ValueRef -> Ptr a -> IO ()
foreign import ccall unsafe "LLVMAddGlobalMapping" addFunctionMapping
    :: ExecutionEngineRef -> ValueRef -> FunPtr a -> IO ()
foreign import ccall unsafe "LLVMGetPointerToGlobal" getPointerToGlobal
    :: ExecutionEngineRef -> ValueRef -> IO (Ptr a)
foreign import ccall unsafe "LLVMGetPointerToGlobal" getPointerToFunction
    :: ExecutionEngineRef -> ValueRef -> IO (FunPtr a)
