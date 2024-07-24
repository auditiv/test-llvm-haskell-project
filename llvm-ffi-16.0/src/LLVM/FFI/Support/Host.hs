{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.FFI.Support.Host (
    FeatureMap,
    FeatureMapRef,
    FeatureIterator,
    FeatureIteratorRef,
    getHostCPUName,
    getHostFeatures,
    freeFeatures,
    getFirstFeature,
    getNextFeature,
    getFeatureName,
    getFeatureSupport,
    ) where

import qualified LLVM.FFI.Core as LLVM
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)


foreign import ccall unsafe "LLVMGetHostCPUName" getHostCPUName :: IO CString


data FeatureMap
    deriving (Typeable)
type FeatureMapRef = Ptr FeatureMap

data FeatureIterator
    deriving (Typeable)
type FeatureIteratorRef = Ptr FeatureIterator

foreign import ccall unsafe "LLVMGetHostFeatures" getHostFeatures
    :: IO FeatureMapRef
foreign import ccall unsafe "LLVMFreeFeatures" freeFeatures
    :: FeatureMapRef -> IO ()

foreign import ccall unsafe "LLVMGetFirstFeature" getFirstFeature
    :: FeatureMapRef -> IO FeatureIteratorRef
foreign import ccall unsafe "LLVMGetNextFeature" getNextFeature
    :: FeatureMapRef -> FeatureIteratorRef -> IO FeatureIteratorRef

foreign import ccall unsafe "LLVMGetFeatureName" getFeatureName
    :: FeatureIteratorRef -> IO CString
foreign import ccall unsafe "LLVMGetFeatureSupport" getFeatureSupport
    :: FeatureIteratorRef -> IO LLVM.Bool
