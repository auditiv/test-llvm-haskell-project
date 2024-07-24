{-# LINE 1 "src/LLVM/FFI/Target.hsc" #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.Target where

import LLVM.FFI.Core (ValueRef, TypeRef, PassManagerRef, ModuleRef)
import LLVM.FFI.Base (FinalizerPtr)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)
import Data.Word (Word32)


type CUInt = C.CUInt
type CULLong = C.CULLong




newtype ByteOrdering = ByteOrdering (Word32)
{-# LINE 26 "src/LLVM/FFI/Target.hsc" #-}
    deriving (Eq)

bigEndian, littleEndian :: ByteOrdering
bigEndian = ByteOrdering (0)
{-# LINE 30 "src/LLVM/FFI/Target.hsc" #-}
littleEndian = ByteOrdering (1)
{-# LINE 31 "src/LLVM/FFI/Target.hsc" #-}


data TargetData
    deriving (Typeable)
type TargetDataRef = Ptr TargetData

data TargetLibraryInfo
    deriving (Typeable)
type TargetLibraryInfoRef = Ptr TargetLibraryInfo

foreign import ccall unsafe "LLVMGetModuleDataLayout" getModuleDataLayout
    :: ModuleRef -> IO TargetDataRef
foreign import ccall unsafe "LLVMSetModuleDataLayout" setModuleDataLayout
    :: ModuleRef -> TargetDataRef -> IO ()
foreign import ccall unsafe "LLVMCreateTargetData" createTargetData
    :: CString -> IO TargetDataRef
foreign import ccall unsafe "LLVMDisposeTargetData" disposeTargetData
    :: TargetDataRef -> IO ()
foreign import ccall unsafe "&LLVMDisposeTargetData" ptrDisposeTargetData
    :: FinalizerPtr TargetData
foreign import ccall unsafe "LLVMAddTargetLibraryInfo" addTargetLibraryInfo
    :: TargetLibraryInfoRef -> PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMCopyStringRepOfTargetData" copyStringRepOfTargetData
    :: TargetDataRef -> IO CString
foreign import ccall unsafe "LLVMByteOrder" byteOrder
    :: TargetDataRef -> IO ByteOrdering
foreign import ccall unsafe "LLVMPointerSize" pointerSize
    :: TargetDataRef -> IO CUInt
foreign import ccall unsafe "LLVMIntPtrType" intPtrType
    :: TargetDataRef -> IO TypeRef
foreign import ccall unsafe "LLVMSizeOfTypeInBits" sizeOfTypeInBits
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMStoreSizeOfType" storeSizeOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMABISizeOfType" abiSizeOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMABIAlignmentOfType" abiAlignmentOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMCallFrameAlignmentOfType" callFrameAlignmentOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMPreferredAlignmentOfType" preferredAlignmentOfType
    :: TargetDataRef -> TypeRef -> IO CULLong
foreign import ccall unsafe "LLVMPreferredAlignmentOfGlobal" preferredAlignmentOfGlobal
    :: TargetDataRef -> ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMElementAtOffset" elementAtOffset
    :: TargetDataRef -> TypeRef -> CULLong -> IO CUInt
foreign import ccall unsafe "LLVMOffsetOfElement" offsetOfElement
    :: TargetDataRef -> TypeRef -> CUInt -> IO CULLong
