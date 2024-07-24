module LLVM.FFI.Core14 where

import qualified LLVM.Raw.Core as Raw

import LLVM.Raw.Core (TypeRef, ValueRef, ModuleRef, BasicBlockRef, BuilderRef)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)


type CDouble  = C.CDouble
type CInt     = C.CInt
type CUInt    = C.CUInt
type CLLong   = C.CLLong
type CULLong  = C.CULLong


constGEP2 :: TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef
constGEP2 _typ = Raw.constGEP

constInBoundsGEP2 :: TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef
constInBoundsGEP2 _typ = Raw.constInBoundsGEP

addAlias2 :: ModuleRef -> TypeRef -> CUInt -> ValueRef -> CString -> IO ValueRef
addAlias2 bld typ _addrSpace = Raw.addAlias bld typ

buildInvoke2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef
buildInvoke2 bld _typ = Raw.buildInvoke bld

buildLoad2 :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
buildLoad2 bld _typ = Raw.buildLoad bld

buildGEP2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
buildGEP2 bld _typ = Raw.buildGEP bld

buildInBoundsGEP2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
buildInBoundsGEP2 bld _typ = Raw.buildInBoundsGEP bld

buildStructGEP2 :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef
buildStructGEP2 bld _typ = Raw.buildStructGEP bld

buildCall2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
buildCall2 bld _typ = Raw.buildCall bld

buildPtrDiff2 :: BuilderRef -> TypeRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildPtrDiff2 bld _typ = Raw.buildPtrDiff bld
