{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLVM.Raw.Core where

import qualified LLVM.FFI.Base as LLVM

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, FunPtr)

import Data.Typeable (Typeable)

import Data.Word (Word8, Word32, Word64)


type CDouble  = C.CDouble
type CInt     = C.CInt
type CUInt    = C.CUInt
type CLLong   = C.CLLong
type CULLong  = C.CULLong
type CSize    = C.CSize


#include <llvm/Config/llvm-config.h>
#include <llvm-c/Core.h>


data Module
    deriving (Typeable)
type ModuleRef = Ptr Module

data ModuleProvider
    deriving (Typeable)
type ModuleProviderRef = Ptr ModuleProvider

data Type
    deriving (Typeable)
type TypeRef = Ptr Type

data BasicBlock
    deriving (Typeable)
type BasicBlockRef = Ptr BasicBlock

data Value
    deriving (Typeable)
type ValueRef = Ptr Value

data OpaqueUse
    deriving (Typeable)
type UseRef = Ptr OpaqueUse

data Builder
    deriving (Typeable)
type BuilderRef = Ptr Builder

data MemoryBuffer
    deriving (Typeable)
type MemoryBufferRef = Ptr MemoryBuffer

data PassManager
    deriving (Typeable)
type PassManagerRef = Ptr PassManager

data PassRegistry
    deriving (Typeable)
type PassRegistryRef = Ptr PassRegistry

data Context
    deriving (Typeable)
type ContextRef = Ptr Context

{-
data Attribute
    deriving (Typeable)
-}
-- until 3.9
newtype Attribute = Attribute Word32
type AttributeRef = Ptr Attribute

newtype AttributeIndex = AttributeIndex #{type LLVMAttributeIndex}

attributeReturnIndex, attributeFunctionIndex :: AttributeIndex
attributeReturnIndex   = AttributeIndex (#const LLVMAttributeReturnIndex)
attributeFunctionIndex = AttributeIndex (#const LLVMAttributeFunctionIndex)


data Metadata
    deriving (Typeable)
type MetadataRef = Ptr Metadata

data DiagnosticInfo
    deriving (Typeable)
type DiagnosticInfoRef = Ptr DiagnosticInfo

data NamedMDNode
    deriving (Typeable)
type NamedMDNodeRef = Ptr NamedMDNode


data ModuleFlagEntry
    deriving (Typeable)
data ValueMetadataEntry
    deriving (Typeable)

type DiagnosticHandler = FunPtr (DiagnosticInfoRef -> Ptr () -> IO ())
type YieldCallback = FunPtr (ContextRef -> Ptr () -> IO ())

newtype CallingConvention = CallingConvention {unCallingConvention :: CUInt}

newtype AtomicOrdering = AtomicOrdering #{type LLVMAtomicOrdering}
newtype AtomicRMWBinOp = AtomicRMWBinOp #{type LLVMAtomicRMWBinOp}
newtype DiagnosticSeverity = DiagnosticSeverity #{type LLVMDiagnosticSeverity}
newtype DLLStorageClass = DLLStorageClass #{type LLVMDLLStorageClass}
-- newtype InlineAsmDialect = InlineAsmDialect #{type LLVMInlineAsmDialect}
newtype InlineAsmDialect = InlineAsmDialect Word32
newtype IntPredicate = IntPredicate #{type LLVMIntPredicate}
newtype Linkage = Linkage #{type LLVMLinkage}
-- newtype ModuleFlagBehavior = ModuleFlagBehavior #{type LLVMModuleFlagBehavior}
newtype ModuleFlagBehavior = ModuleFlagBehavior Word32
newtype Opcode = Opcode #{type LLVMOpcode}
newtype RealPredicate = RealPredicate #{type LLVMRealPredicate}
newtype ThreadLocalMode = ThreadLocalMode #{type LLVMThreadLocalMode}
newtype TypeKind = TypeKind {unTypeKind :: #{type LLVMTypeKind}}
-- newtype UnnamedAddr = UnnamedAddr #{type LLVMUnnamedAddr}
newtype UnnamedAddr = UnnamedAddr Word32
newtype ValueKind = ValueKind #{type LLVMValueKind}
newtype Visibility = Visibility #{type LLVMVisibility}


foreign import ccall unsafe "LLVMInitializeCore" initializeCore
    :: PassRegistryRef -> IO ()

foreign import ccall unsafe "LLVMShutdown" shutdown
    :: IO ()

foreign import ccall unsafe "LLVMCreateMessage" createMessage
    :: CString -> IO CString

foreign import ccall unsafe "LLVMDisposeMessage" disposeMessage
    :: CString -> IO ()

foreign import ccall unsafe "LLVMContextCreate" contextCreate
    :: IO ContextRef

foreign import ccall unsafe "LLVMGetGlobalContext" getGlobalContext
    :: IO ContextRef

foreign import ccall unsafe "LLVMContextSetDiagnosticHandler" contextSetDiagnosticHandler
    :: ContextRef -> DiagnosticHandler -> (Ptr ()) -> IO ()

foreign import ccall unsafe "LLVMContextGetDiagnosticHandler" contextGetDiagnosticHandler
    :: ContextRef -> IO DiagnosticHandler

foreign import ccall unsafe "LLVMContextGetDiagnosticContext" contextGetDiagnosticContext
    :: ContextRef -> IO (Ptr ())

foreign import ccall unsafe "LLVMContextSetYieldCallback" contextSetYieldCallback
    :: ContextRef -> YieldCallback -> (Ptr ()) -> IO ()

foreign import ccall unsafe "LLVMContextShouldDiscardValueNames" contextShouldDiscardValueNames
    :: ContextRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMContextSetDiscardValueNames" contextSetDiscardValueNames
    :: ContextRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMContextDispose" contextDispose
    :: ContextRef -> IO ()

foreign import ccall unsafe "LLVMGetDiagInfoDescription" getDiagInfoDescription
    :: DiagnosticInfoRef -> IO CString

foreign import ccall unsafe "LLVMGetDiagInfoSeverity" getDiagInfoSeverity
    :: DiagnosticInfoRef -> IO DiagnosticSeverity

foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext
    :: ContextRef -> CString -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVMGetMDKindID" getMDKindID
    :: CString -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVMGetEnumAttributeKindForName" getEnumAttributeKindForName
    :: CString -> CSize -> IO CUInt

foreign import ccall unsafe "LLVMGetLastEnumAttributeKind" getLastEnumAttributeKind
    :: IO CUInt

foreign import ccall unsafe "LLVMCreateEnumAttribute" createEnumAttribute
    :: ContextRef -> CUInt -> Word64 -> IO AttributeRef

foreign import ccall unsafe "LLVMGetEnumAttributeKind" getEnumAttributeKind
    :: AttributeRef -> IO CUInt

foreign import ccall unsafe "LLVMGetEnumAttributeValue" getEnumAttributeValue
    :: AttributeRef -> IO Word64

foreign import ccall unsafe "LLVMCreateTypeAttribute" createTypeAttribute
    :: ContextRef -> CUInt -> TypeRef -> IO AttributeRef

foreign import ccall unsafe "LLVMGetTypeAttributeValue" getTypeAttributeValue
    :: AttributeRef -> IO TypeRef

foreign import ccall unsafe "LLVMCreateStringAttribute" createStringAttribute
    :: ContextRef -> CString -> CUInt -> CString -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMGetStringAttributeKind" getStringAttributeKind
    :: AttributeRef -> (Ptr CUInt) -> IO CString

foreign import ccall unsafe "LLVMGetStringAttributeValue" getStringAttributeValue
    :: AttributeRef -> (Ptr CUInt) -> IO CString

foreign import ccall unsafe "LLVMIsEnumAttribute" isEnumAttribute
    :: AttributeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsStringAttribute" isStringAttribute
    :: AttributeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsTypeAttribute" isTypeAttribute
    :: AttributeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetTypeByName2" getTypeByName2
    :: ContextRef -> CString -> IO TypeRef

foreign import ccall unsafe "LLVMModuleCreateWithName" moduleCreateWithName
    :: CString -> IO ModuleRef

foreign import ccall unsafe "LLVMModuleCreateWithNameInContext" moduleCreateWithNameInContext
    :: CString -> ContextRef -> IO ModuleRef

foreign import ccall unsafe "LLVMCloneModule" cloneModule
    :: ModuleRef -> IO ModuleRef

foreign import ccall unsafe "LLVMDisposeModule" disposeModule
    :: ModuleRef -> IO ()

foreign import ccall unsafe "LLVMGetModuleIdentifier" getModuleIdentifier
    :: ModuleRef -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMSetModuleIdentifier" setModuleIdentifier
    :: ModuleRef -> CString -> CSize -> IO ()

foreign import ccall unsafe "LLVMGetSourceFileName" getSourceFileName
    :: ModuleRef -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMSetSourceFileName" setSourceFileName
    :: ModuleRef -> CString -> CSize -> IO ()

foreign import ccall unsafe "LLVMGetDataLayoutStr" getDataLayoutStr
    :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMGetDataLayout" getDataLayout
    :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMSetDataLayout" setDataLayout
    :: ModuleRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetTarget" getTarget
    :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMSetTarget" setTarget
    :: ModuleRef -> CString -> IO ()

foreign import ccall unsafe "LLVMCopyModuleFlagsMetadata" copyModuleFlagsMetadata
    :: ModuleRef -> (Ptr CSize) -> IO (Ptr ModuleFlagEntry)

foreign import ccall unsafe "LLVMDisposeModuleFlagsMetadata" disposeModuleFlagsMetadata
    :: (Ptr ModuleFlagEntry) -> IO ()

foreign import ccall unsafe "LLVMModuleFlagEntriesGetFlagBehavior" moduleFlagEntriesGetFlagBehavior
    :: (Ptr ModuleFlagEntry) -> CUInt -> IO ModuleFlagBehavior

foreign import ccall unsafe "LLVMModuleFlagEntriesGetKey" moduleFlagEntriesGetKey
    :: (Ptr ModuleFlagEntry) -> CUInt -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMModuleFlagEntriesGetMetadata" moduleFlagEntriesGetMetadata
    :: (Ptr ModuleFlagEntry) -> CUInt -> IO MetadataRef

foreign import ccall unsafe "LLVMGetModuleFlag" getModuleFlag
    :: ModuleRef -> CString -> CSize -> IO MetadataRef

foreign import ccall unsafe "LLVMAddModuleFlag" addModuleFlag
    :: ModuleRef -> ModuleFlagBehavior -> CString -> CSize -> MetadataRef -> IO ()

foreign import ccall unsafe "LLVMDumpModule" dumpModule
    :: ModuleRef -> IO ()

foreign import ccall unsafe "LLVMPrintModuleToFile" printModuleToFile
    :: ModuleRef -> CString -> (Ptr CString) -> IO LLVM.Bool

foreign import ccall unsafe "LLVMPrintModuleToString" printModuleToString
    :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMGetModuleInlineAsm" getModuleInlineAsm
    :: ModuleRef -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMSetModuleInlineAsm2" setModuleInlineAsm2
    :: ModuleRef -> CString -> CSize -> IO ()

foreign import ccall unsafe "LLVMAppendModuleInlineAsm" appendModuleInlineAsm
    :: ModuleRef -> CString -> CSize -> IO ()

foreign import ccall unsafe "LLVMGetInlineAsm" getInlineAsm
    :: TypeRef -> CString -> CSize -> CString -> CSize -> LLVM.Bool -> LLVM.Bool -> InlineAsmDialect -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMGetModuleContext" getModuleContext
    :: ModuleRef -> IO ContextRef

foreign import ccall unsafe "LLVMGetTypeByName" getTypeByName
    :: ModuleRef -> CString -> IO TypeRef

foreign import ccall unsafe "LLVMGetFirstNamedMetadata" getFirstNamedMetadata
    :: ModuleRef -> IO NamedMDNodeRef

foreign import ccall unsafe "LLVMGetLastNamedMetadata" getLastNamedMetadata
    :: ModuleRef -> IO NamedMDNodeRef

foreign import ccall unsafe "LLVMGetNextNamedMetadata" getNextNamedMetadata
    :: NamedMDNodeRef -> IO NamedMDNodeRef

foreign import ccall unsafe "LLVMGetPreviousNamedMetadata" getPreviousNamedMetadata
    :: NamedMDNodeRef -> IO NamedMDNodeRef

foreign import ccall unsafe "LLVMGetNamedMetadata" getNamedMetadata
    :: ModuleRef -> CString -> CSize -> IO NamedMDNodeRef

foreign import ccall unsafe "LLVMGetOrInsertNamedMetadata" getOrInsertNamedMetadata
    :: ModuleRef -> CString -> CSize -> IO NamedMDNodeRef

foreign import ccall unsafe "LLVMGetNamedMetadataName" getNamedMetadataName
    :: NamedMDNodeRef -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMGetNamedMetadataNumOperands" getNamedMetadataNumOperands
    :: ModuleRef -> CString -> IO CUInt

foreign import ccall unsafe "LLVMGetNamedMetadataOperands" getNamedMetadataOperands
    :: ModuleRef -> CString -> (Ptr ValueRef) -> IO ()

foreign import ccall unsafe "LLVMAddNamedMetadataOperand" addNamedMetadataOperand
    :: ModuleRef -> CString -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetDebugLocDirectory" getDebugLocDirectory
    :: ValueRef -> (Ptr CUInt) -> IO CString

foreign import ccall unsafe "LLVMGetDebugLocFilename" getDebugLocFilename
    :: ValueRef -> (Ptr CUInt) -> IO CString

foreign import ccall unsafe "LLVMGetDebugLocLine" getDebugLocLine
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetDebugLocColumn" getDebugLocColumn
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMAddFunction" addFunction
    :: ModuleRef -> CString -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: ModuleRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstFunction" getFirstFunction
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastFunction" getLastFunction
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextFunction" getNextFunction
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousFunction" getPreviousFunction
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetModuleInlineAsm" setModuleInlineAsm
    :: ModuleRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetTypeKind" getTypeKind
    :: TypeRef -> IO TypeKind

foreign import ccall unsafe "LLVMTypeIsSized" typeIsSized
    :: TypeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetTypeContext" getTypeContext
    :: TypeRef -> IO ContextRef

foreign import ccall unsafe "LLVMPrintTypeToString" printTypeToString
    :: TypeRef -> IO CString

foreign import ccall unsafe "LLVMInt1TypeInContext" int1TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt8TypeInContext" int8TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt16TypeInContext" int16TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt32TypeInContext" int32TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt64TypeInContext" int64TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMInt128TypeInContext" int128TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMIntTypeInContext" intTypeInContext
    :: ContextRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMInt1Type" int1Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMInt8Type" int8Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMInt16Type" int16Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMInt32Type" int32Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMInt64Type" int64Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMInt128Type" int128Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMIntType" intType
    :: CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMGetIntTypeWidth" getIntTypeWidth
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMHalfTypeInContext" halfTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMBFloatTypeInContext" bFloatTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMFloatTypeInContext" floatTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMDoubleTypeInContext" doubleTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMX86FP80TypeInContext" x86FP80TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMFP128TypeInContext" fP128TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMPPCFP128TypeInContext" pPCFP128TypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMHalfType" halfType
    :: IO TypeRef

foreign import ccall unsafe "LLVMBFloatType" bFloatType
    :: IO TypeRef

foreign import ccall unsafe "LLVMFloatType" floatType
    :: IO TypeRef

foreign import ccall unsafe "LLVMDoubleType" doubleType
    :: IO TypeRef

foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMFP128Type" fP128Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMPPCFP128Type" pPCFP128Type
    :: IO TypeRef

foreign import ccall unsafe "LLVMFunctionType" functionType
    :: TypeRef -> (Ptr TypeRef) -> CUInt -> LLVM.Bool -> IO TypeRef

foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg
    :: TypeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetReturnType" getReturnType
    :: TypeRef -> IO TypeRef

foreign import ccall unsafe "LLVMCountParamTypes" countParamTypes
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMGetParamTypes" getParamTypes
    :: TypeRef -> (Ptr TypeRef) -> IO ()

foreign import ccall unsafe "LLVMStructTypeInContext" structTypeInContext
    :: ContextRef -> (Ptr TypeRef) -> CUInt -> LLVM.Bool -> IO TypeRef

foreign import ccall unsafe "LLVMStructType" structType
    :: (Ptr TypeRef) -> CUInt -> LLVM.Bool -> IO TypeRef

foreign import ccall unsafe "LLVMStructCreateNamed" structCreateNamed
    :: ContextRef -> CString -> IO TypeRef

foreign import ccall unsafe "LLVMGetStructName" getStructName
    :: TypeRef -> IO CString

foreign import ccall unsafe "LLVMStructSetBody" structSetBody
    :: TypeRef -> (Ptr TypeRef) -> CUInt -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMCountStructElementTypes" countStructElementTypes
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMGetStructElementTypes" getStructElementTypes
    :: TypeRef -> (Ptr TypeRef) -> IO ()

foreign import ccall unsafe "LLVMStructGetTypeAtIndex" structGetTypeAtIndex
    :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMIsPackedStruct" isPackedStruct
    :: TypeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsOpaqueStruct" isOpaqueStruct
    :: TypeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsLiteralStruct" isLiteralStruct
    :: TypeRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: TypeRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetSubtypes" getSubtypes
    :: TypeRef -> (Ptr TypeRef) -> IO ()

foreign import ccall unsafe "LLVMGetNumContainedTypes" getNumContainedTypes
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMArrayType" arrayType
    :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMGetArrayLength" getArrayLength
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMPointerType" pointerType
    :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMGetPointerAddressSpace" getPointerAddressSpace
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMVectorType" vectorType
    :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMScalableVectorType" scalableVectorType
    :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMGetVectorSize" getVectorSize
    :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMVoidTypeInContext" voidTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMLabelTypeInContext" labelTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMX86MMXTypeInContext" x86MMXTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMX86AMXTypeInContext" x86AMXTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMTokenTypeInContext" tokenTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMMetadataTypeInContext" metadataTypeInContext
    :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMVoidType" voidType
    :: IO TypeRef

foreign import ccall unsafe "LLVMLabelType" labelType
    :: IO TypeRef

foreign import ccall unsafe "LLVMX86MMXType" x86MMXType
    :: IO TypeRef

foreign import ccall unsafe "LLVMX86AMXType" x86AMXType
    :: IO TypeRef

foreign import ccall unsafe "LLVMTypeOf" typeOf
    :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetValueKind" getValueKind
    :: ValueRef -> IO ValueKind

foreign import ccall unsafe "LLVMGetValueName2" getValueName2
    :: ValueRef -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMSetValueName2" setValueName2
    :: ValueRef -> CString -> CSize -> IO ()

foreign import ccall unsafe "LLVMDumpValue" dumpValue
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMPrintValueToString" printValueToString
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMReplaceAllUsesWith" replaceAllUsesWith
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMIsConstant" isConstant
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsUndef" isUndef
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsPoison" isPoison
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMIsAMDNode" isAMDNode
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMIsAMDString" isAMDString
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetValueName" getValueName
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetValueName" setValueName
    :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetFirstUse" getFirstUse
    :: ValueRef -> IO UseRef

foreign import ccall unsafe "LLVMGetNextUse" getNextUse
    :: UseRef -> IO UseRef

foreign import ccall unsafe "LLVMGetUser" getUser
    :: UseRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetUsedValue" getUsedValue
    :: UseRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetOperand" getOperand
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetOperandUse" getOperandUse
    :: ValueRef -> CUInt -> IO UseRef

foreign import ccall unsafe "LLVMSetOperand" setOperand
    :: ValueRef -> CUInt -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetNumOperands" getNumOperands
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMConstNull" constNull
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstAllOnes" constAllOnes
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetUndef" getUndef
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPoison" getPoison
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMIsNull" isNull
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMConstPointerNull" constPointerNull
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstInt" constInt
    :: TypeRef -> CULLong -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntOfArbitraryPrecision" constIntOfArbitraryPrecision
    :: TypeRef -> CUInt -> Ptr Word64 -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntOfString" constIntOfString
    :: TypeRef -> CString -> Word8 -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntOfStringAndSize" constIntOfStringAndSize
    :: TypeRef -> CString -> CUInt -> Word8 -> IO ValueRef

foreign import ccall unsafe "LLVMConstReal" constReal
    :: TypeRef -> CDouble -> IO ValueRef

foreign import ccall unsafe "LLVMConstRealOfString" constRealOfString
    :: TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMConstRealOfStringAndSize" constRealOfStringAndSize
    :: TypeRef -> CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntGetZExtValue" constIntGetZExtValue
    :: ValueRef -> IO CULLong

foreign import ccall unsafe "LLVMConstIntGetSExtValue" constIntGetSExtValue
    :: ValueRef -> IO CLLong

foreign import ccall unsafe "LLVMConstRealGetDouble" constRealGetDouble
    :: ValueRef -> (Ptr LLVM.Bool) -> IO CDouble

foreign import ccall unsafe "LLVMConstStringInContext" constStringInContext
    :: ContextRef -> CString -> CUInt -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMConstString" constString
    :: CString -> CUInt -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMIsConstantString" isConstantString
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetAsString" getAsString
    :: ValueRef -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMConstStructInContext" constStructInContext
    :: ContextRef -> (Ptr ValueRef) -> CUInt -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMConstStruct" constStruct
    :: (Ptr ValueRef) -> CUInt -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMConstArray" constArray
    :: TypeRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstNamedStruct" constNamedStruct
    :: TypeRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetElementAsConstant" getElementAsConstant
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstVector" constVector
    :: (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetConstOpcode" getConstOpcode
    :: ValueRef -> IO Opcode

foreign import ccall unsafe "LLVMAlignOf" alignOf
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMSizeOf" sizeOf
    :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNeg" constNeg
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNSWNeg" constNSWNeg
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWNeg" constNUWNeg
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFNeg" constFNeg
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNot" constNot
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstAdd" constAdd
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNSWAdd" constNSWAdd
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWAdd" constNUWAdd
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFAdd" constFAdd
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSub" constSub
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNSWSub" constNSWSub
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWSub" constNUWSub
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFSub" constFSub
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstMul" constMul
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNSWMul" constNSWMul
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstNUWMul" constNUWMul
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFMul" constFMul
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstUDiv" constUDiv
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstExactUDiv" constExactUDiv
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSDiv" constSDiv
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstExactSDiv" constExactSDiv
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFDiv" constFDiv
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstURem" constURem
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSRem" constSRem
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFRem" constFRem
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstAnd" constAnd
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstOr" constOr
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstXor" constXor
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstICmp" constICmp
    :: IntPredicate -> ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFCmp" constFCmp
    :: RealPredicate -> ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstShl" constShl
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstLShr" constLShr
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstAShr" constAShr
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstGEP" constGEP
    :: ValueRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstInBoundsGEP" constInBoundsGEP
    :: ValueRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstTrunc" constTrunc
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSExt" constSExt
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstZExt" constZExt
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFPTrunc" constFPTrunc
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFPExt" constFPExt
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstUIToFP" constUIToFP
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSIToFP" constSIToFP
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFPToUI" constFPToUI
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstFPToSI" constFPToSI
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstPtrToInt" constPtrToInt
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntToPtr" constIntToPtr
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstBitCast" constBitCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstAddrSpaceCast" constAddrSpaceCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstZExtOrBitCast" constZExtOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSExtOrBitCast" constSExtOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstTruncOrBitCast" constTruncOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstPointerCast" constPointerCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstIntCast" constIntCast
    :: ValueRef -> TypeRef -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMConstFPCast" constFPCast
    :: ValueRef -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstSelect" constSelect
    :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstExtractElement" constExtractElement
    :: ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstInsertElement" constInsertElement
    :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstShuffleVector" constShuffleVector
    :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstExtractValue" constExtractValue
    :: ValueRef -> (Ptr CUInt) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstInsertValue" constInsertValue
    :: ValueRef -> ValueRef -> (Ptr CUInt) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBlockAddress" blockAddress
    :: ValueRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstInlineAsm" constInlineAsm
    :: TypeRef -> CString -> CString -> LLVM.Bool -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMGetGlobalParent" getGlobalParent
    :: ValueRef -> IO ModuleRef

foreign import ccall unsafe "LLVMIsDeclaration" isDeclaration
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetLinkage" getLinkage
    :: ValueRef -> IO Linkage

foreign import ccall unsafe "LLVMSetLinkage" setLinkage
    :: ValueRef -> Linkage -> IO ()

foreign import ccall unsafe "LLVMGetSection" getSection
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetSection" setSection
    :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetVisibility" getVisibility
    :: ValueRef -> IO Visibility

foreign import ccall unsafe "LLVMSetVisibility" setVisibility
    :: ValueRef -> Visibility -> IO ()

foreign import ccall unsafe "LLVMGetDLLStorageClass" getDLLStorageClass
    :: ValueRef -> IO DLLStorageClass

foreign import ccall unsafe "LLVMSetDLLStorageClass" setDLLStorageClass
    :: ValueRef -> DLLStorageClass -> IO ()

foreign import ccall unsafe "LLVMGetUnnamedAddress" getUnnamedAddress
    :: ValueRef -> IO UnnamedAddr

foreign import ccall unsafe "LLVMSetUnnamedAddress" setUnnamedAddress
    :: ValueRef -> UnnamedAddr -> IO ()

foreign import ccall unsafe "LLVMGlobalGetValueType" globalGetValueType
    :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMHasUnnamedAddr" hasUnnamedAddr
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetUnnamedAddr" setUnnamedAddr
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMGetAlignment" getAlignment
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetAlignment" setAlignment
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGlobalSetMetadata" globalSetMetadata
    :: ValueRef -> CUInt -> MetadataRef -> IO ()

foreign import ccall unsafe "LLVMGlobalEraseMetadata" globalEraseMetadata
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGlobalClearMetadata" globalClearMetadata
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMGlobalCopyAllMetadata" globalCopyAllMetadata
    :: ValueRef -> (Ptr CSize) -> IO (Ptr ValueMetadataEntry)

foreign import ccall unsafe "LLVMDisposeValueMetadataEntries" disposeValueMetadataEntries
    :: (Ptr ValueMetadataEntry) -> IO ()

foreign import ccall unsafe "LLVMValueMetadataEntriesGetKind" valueMetadataEntriesGetKind
    :: (Ptr ValueMetadataEntry) -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVMValueMetadataEntriesGetMetadata" valueMetadataEntriesGetMetadata
    :: (Ptr ValueMetadataEntry) -> CUInt -> IO MetadataRef

foreign import ccall unsafe "LLVMAddGlobal" addGlobal
    :: ModuleRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMAddGlobalInAddressSpace" addGlobalInAddressSpace
    :: ModuleRef -> TypeRef -> CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetNamedGlobal" getNamedGlobal
    :: ModuleRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstGlobal" getFirstGlobal
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastGlobal" getLastGlobal
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextGlobal" getNextGlobal
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousGlobal" getPreviousGlobal
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteGlobal" deleteGlobal
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetInitializer" getInitializer
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetInitializer" setInitializer
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMIsThreadLocal" isThreadLocal
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetThreadLocal" setThreadLocal
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMIsGlobalConstant" isGlobalConstant
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetGlobalConstant" setGlobalConstant
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMGetThreadLocalMode" getThreadLocalMode
    :: ValueRef -> IO ThreadLocalMode

foreign import ccall unsafe "LLVMSetThreadLocalMode" setThreadLocalMode
    :: ValueRef -> ThreadLocalMode -> IO ()

foreign import ccall unsafe "LLVMIsExternallyInitialized" isExternallyInitialized
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetExternallyInitialized" setExternallyInitialized
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMAddAlias" addAlias
    :: ModuleRef -> TypeRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetNamedGlobalAlias" getNamedGlobalAlias
    :: ModuleRef -> CString -> CSize -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstGlobalAlias" getFirstGlobalAlias
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastGlobalAlias" getLastGlobalAlias
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextGlobalAlias" getNextGlobalAlias
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousGlobalAlias" getPreviousGlobalAlias
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMAliasGetAliasee" aliasGetAliasee
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMAliasSetAliasee" aliasSetAliasee
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMDeleteFunction" deleteFunction
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMHasPersonalityFn" hasPersonalityFn
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetPersonalityFn" getPersonalityFn
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetPersonalityFn" setPersonalityFn
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMLookupIntrinsicID" lookupIntrinsicID
    :: CString -> CSize -> IO CUInt

foreign import ccall unsafe "LLVMGetIntrinsicID" getIntrinsicID
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetIntrinsicDeclaration" getIntrinsicDeclaration
    :: ModuleRef -> CUInt -> (Ptr TypeRef) -> CSize -> IO ValueRef

foreign import ccall unsafe "LLVMIntrinsicGetType" intrinsicGetType
    :: ContextRef -> CUInt -> (Ptr TypeRef) -> CSize -> IO TypeRef

foreign import ccall unsafe "LLVMIntrinsicGetName" intrinsicGetName
    :: CUInt -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMIntrinsicCopyOverloadedName" intrinsicCopyOverloadedName
    :: CUInt -> (Ptr TypeRef) -> CSize -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMIntrinsicCopyOverloadedName2" intrinsicCopyOverloadedName2
    :: ModuleRef -> CUInt -> (Ptr TypeRef) -> CSize -> (Ptr CSize) -> IO CString

foreign import ccall unsafe "LLVMIntrinsicIsOverloaded" intrinsicIsOverloaded
    :: CUInt -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetFunctionCallConv" getFunctionCallConv
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetFunctionCallConv" setFunctionCallConv
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetGC" getGC
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetGC" setGC
    :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMAddAttributeAtIndex" addAttributeAtIndex
    :: ValueRef -> AttributeIndex -> AttributeRef -> IO ()

foreign import ccall unsafe "LLVMGetAttributeCountAtIndex" getAttributeCountAtIndex
    :: ValueRef -> AttributeIndex -> IO CUInt

foreign import ccall unsafe "LLVMGetAttributesAtIndex" getAttributesAtIndex
    :: ValueRef -> AttributeIndex -> (Ptr AttributeRef) -> IO ()

foreign import ccall unsafe "LLVMGetEnumAttributeAtIndex" getEnumAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMGetStringAttributeAtIndex" getStringAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMRemoveEnumAttributeAtIndex" removeEnumAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CUInt -> IO ()

foreign import ccall unsafe "LLVMRemoveStringAttributeAtIndex" removeStringAttributeAtIndex
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO ()

foreign import ccall unsafe "LLVMAddTargetDependentFunctionAttr" addTargetDependentFunctionAttr
    :: ValueRef -> CString -> CString -> IO ()

foreign import ccall unsafe "LLVMCountParams" countParams
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetParams" getParams
    :: ValueRef -> (Ptr ValueRef) -> IO ()

foreign import ccall unsafe "LLVMGetParam" getParam
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetParamParent" getParamParent
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstParam" getFirstParam
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastParam" getLastParam
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextParam" getNextParam
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousParam" getPreviousParam
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetParamAlignment" setParamAlignment
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMAddGlobalIFunc" addGlobalIFunc
    :: ModuleRef -> CString -> CSize -> TypeRef -> CUInt -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNamedGlobalIFunc" getNamedGlobalIFunc
    :: ModuleRef -> CString -> CSize -> IO ValueRef

foreign import ccall unsafe "LLVMGetFirstGlobalIFunc" getFirstGlobalIFunc
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastGlobalIFunc" getLastGlobalIFunc
    :: ModuleRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNextGlobalIFunc" getNextGlobalIFunc
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousGlobalIFunc" getPreviousGlobalIFunc
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetGlobalIFuncResolver" getGlobalIFuncResolver
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetGlobalIFuncResolver" setGlobalIFuncResolver
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMEraseGlobalIFunc" eraseGlobalIFunc
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMRemoveGlobalIFunc" removeGlobalIFunc
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMMDStringInContext2" mDStringInContext2
    :: ContextRef -> CString -> CSize -> IO MetadataRef

foreign import ccall unsafe "LLVMMDNodeInContext2" mDNodeInContext2
    :: ContextRef -> (Ptr MetadataRef) -> CSize -> IO MetadataRef

foreign import ccall unsafe "LLVMMetadataAsValue" metadataAsValue
    :: ContextRef -> MetadataRef -> IO ValueRef

foreign import ccall unsafe "LLVMValueAsMetadata" valueAsMetadata
    :: ValueRef -> IO MetadataRef

foreign import ccall unsafe "LLVMGetMDString" getMDString
    :: ValueRef -> (Ptr CUInt) -> IO CString

foreign import ccall unsafe "LLVMGetMDNodeNumOperands" getMDNodeNumOperands
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetMDNodeOperands" getMDNodeOperands
    :: ValueRef -> (Ptr ValueRef) -> IO ()

foreign import ccall unsafe "LLVMMDStringInContext" mDStringInContext
    :: ContextRef -> CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMMDString" mDString
    :: CString -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMMDNodeInContext" mDNodeInContext
    :: ContextRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMMDNode" mDNode
    :: (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBasicBlockAsValue" basicBlockAsValue
    :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMValueIsBasicBlock" valueIsBasicBlock
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMValueAsBasicBlock" valueAsBasicBlock
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetBasicBlockName" getBasicBlockName
    :: BasicBlockRef -> IO CString

foreign import ccall unsafe "LLVMGetBasicBlockParent" getBasicBlockParent
    :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetBasicBlockTerminator" getBasicBlockTerminator
    :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMCountBasicBlocks" countBasicBlocks
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetBasicBlocks" getBasicBlocks
    :: ValueRef -> (Ptr BasicBlockRef) -> IO ()

foreign import ccall unsafe "LLVMGetFirstBasicBlock" getFirstBasicBlock
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetLastBasicBlock" getLastBasicBlock
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetNextBasicBlock" getNextBasicBlock
    :: BasicBlockRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetPreviousBasicBlock" getPreviousBasicBlock
    :: BasicBlockRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetEntryBasicBlock" getEntryBasicBlock
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertExistingBasicBlockAfterInsertBlock" insertExistingBasicBlockAfterInsertBlock
    :: BuilderRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMAppendExistingBasicBlock" appendExistingBasicBlock
    :: ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMCreateBasicBlockInContext" createBasicBlockInContext
    :: ContextRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMAppendBasicBlockInContext" appendBasicBlockInContext
    :: ContextRef -> ValueRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMAppendBasicBlock" appendBasicBlock
    :: ValueRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertBasicBlockInContext" insertBasicBlockInContext
    :: ContextRef -> BasicBlockRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertBasicBlock" insertBasicBlock
    :: BasicBlockRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMDeleteBasicBlock" deleteBasicBlock
    :: BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMRemoveBasicBlockFromParent" removeBasicBlockFromParent
    :: BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMMoveBasicBlockBefore" moveBasicBlockBefore
    :: BasicBlockRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMMoveBasicBlockAfter" moveBasicBlockAfter
    :: BasicBlockRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetFirstInstruction" getFirstInstruction
    :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetLastInstruction" getLastInstruction
    :: BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMHasMetadata" hasMetadata
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMGetMetadata" getMetadata
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMSetMetadata" setMetadata
    :: ValueRef -> CUInt -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMInstructionGetAllMetadataOtherThanDebugLoc" instructionGetAllMetadataOtherThanDebugLoc
    :: ValueRef -> (Ptr CSize) -> IO (Ptr ValueMetadataEntry)

foreign import ccall unsafe "LLVMGetInstructionParent" getInstructionParent
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetNextInstruction" getNextInstruction
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetPreviousInstruction" getPreviousInstruction
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMInstructionRemoveFromParent" instructionRemoveFromParent
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMInstructionEraseFromParent" instructionEraseFromParent
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetInstructionOpcode" getInstructionOpcode
    :: ValueRef -> IO Opcode

foreign import ccall unsafe "LLVMGetICmpPredicate" getICmpPredicate
    :: ValueRef -> IO IntPredicate

foreign import ccall unsafe "LLVMGetFCmpPredicate" getFCmpPredicate
    :: ValueRef -> IO RealPredicate

foreign import ccall unsafe "LLVMInstructionClone" instructionClone
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMIsATerminatorInst" isATerminatorInst
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMGetNumArgOperands" getNumArgOperands
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetInstructionCallConv" setInstructionCallConv
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetInstructionCallConv" getInstructionCallConv
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetInstrParamAlignment" setInstrParamAlignment
    :: ValueRef -> AttributeIndex -> CUInt -> IO ()

foreign import ccall unsafe "LLVMAddCallSiteAttribute" addCallSiteAttribute
    :: ValueRef -> AttributeIndex -> AttributeRef -> IO ()

foreign import ccall unsafe "LLVMGetCallSiteAttributeCount" getCallSiteAttributeCount
    :: ValueRef -> AttributeIndex -> IO CUInt

foreign import ccall unsafe "LLVMGetCallSiteAttributes" getCallSiteAttributes
    :: ValueRef -> AttributeIndex -> (Ptr AttributeRef) -> IO ()

foreign import ccall unsafe "LLVMGetCallSiteEnumAttribute" getCallSiteEnumAttribute
    :: ValueRef -> AttributeIndex -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMGetCallSiteStringAttribute" getCallSiteStringAttribute
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO AttributeRef

foreign import ccall unsafe "LLVMRemoveCallSiteEnumAttribute" removeCallSiteEnumAttribute
    :: ValueRef -> AttributeIndex -> CUInt -> IO ()

foreign import ccall unsafe "LLVMRemoveCallSiteStringAttribute" removeCallSiteStringAttribute
    :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetCalledFunctionType" getCalledFunctionType
    :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetCalledValue" getCalledValue
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMIsTailCall" isTailCall
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetTailCall" setTailCall
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMGetNormalDest" getNormalDest
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetUnwindDest" getUnwindDest
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMSetNormalDest" setNormalDest
    :: ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMSetUnwindDest" setUnwindDest
    :: ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetNumSuccessors" getNumSuccessors
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetSuccessor" getSuccessor
    :: ValueRef -> CUInt -> IO BasicBlockRef

foreign import ccall unsafe "LLVMSetSuccessor" setSuccessor
    :: ValueRef -> CUInt -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMIsConditional" isConditional
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMGetCondition" getCondition
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetCondition" setCondition
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetSwitchDefaultDest" getSwitchDefaultDest
    :: ValueRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetAllocatedType" getAllocatedType
    :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMIsInBounds" isInBounds
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetIsInBounds" setIsInBounds
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMAddIncoming" addIncoming
    :: ValueRef -> (Ptr ValueRef) -> (Ptr BasicBlockRef) -> CUInt -> IO ()

foreign import ccall unsafe "LLVMCountIncoming" countIncoming
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetIncomingValue" getIncomingValue
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMGetIncomingBlock" getIncomingBlock
    :: ValueRef -> CUInt -> IO BasicBlockRef

foreign import ccall unsafe "LLVMGetNumIndices" getNumIndices
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetIndices" getIndices
    :: ValueRef -> IO (Ptr CUInt)

foreign import ccall unsafe "LLVMCreateBuilderInContext" createBuilderInContext
    :: ContextRef -> IO BuilderRef

foreign import ccall unsafe "LLVMCreateBuilder" createBuilder
    :: IO BuilderRef

foreign import ccall unsafe "LLVMPositionBuilder" positionBuilder
    :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderBefore" positionBuilderBefore
    :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderAtEnd" positionBuilderAtEnd
    :: BuilderRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetInsertBlock" getInsertBlock
    :: BuilderRef -> IO BasicBlockRef

foreign import ccall unsafe "LLVMClearInsertionPosition" clearInsertionPosition
    :: BuilderRef -> IO ()

foreign import ccall unsafe "LLVMInsertIntoBuilder" insertIntoBuilder
    :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMInsertIntoBuilderWithName" insertIntoBuilderWithName
    :: BuilderRef -> ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMDisposeBuilder" disposeBuilder
    :: BuilderRef -> IO ()

foreign import ccall unsafe "LLVMGetCurrentDebugLocation2" getCurrentDebugLocation2
    :: BuilderRef -> IO MetadataRef

foreign import ccall unsafe "LLVMSetCurrentDebugLocation2" setCurrentDebugLocation2
    :: BuilderRef -> MetadataRef -> IO ()

foreign import ccall unsafe "LLVMSetInstDebugLocation" setInstDebugLocation
    :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMBuilderGetDefaultFPMathTag" builderGetDefaultFPMathTag
    :: BuilderRef -> IO MetadataRef

foreign import ccall unsafe "LLVMBuilderSetDefaultFPMathTag" builderSetDefaultFPMathTag
    :: BuilderRef -> MetadataRef -> IO ()

foreign import ccall unsafe "LLVMSetCurrentDebugLocation" setCurrentDebugLocation
    :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetCurrentDebugLocation" getCurrentDebugLocation
    :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildRetVoid" buildRetVoid
    :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildRet" buildRet
    :: BuilderRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAggregateRet" buildAggregateRet
    :: BuilderRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBr" buildBr
    :: BuilderRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCondBr" buildCondBr
    :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSwitch" buildSwitch
    :: BuilderRef -> ValueRef -> BasicBlockRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIndirectBr" buildIndirectBr
    :: BuilderRef -> ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInvoke" buildInvoke
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInvoke2" buildInvoke2
    :: BuilderRef -> TypeRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildUnreachable" buildUnreachable
    :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildResume" buildResume
    :: BuilderRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLandingPad" buildLandingPad
    :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCleanupRet" buildCleanupRet
    :: BuilderRef -> ValueRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCatchRet" buildCatchRet
    :: BuilderRef -> ValueRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCatchPad" buildCatchPad
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCleanupPad" buildCleanupPad
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCatchSwitch" buildCatchSwitch
    :: BuilderRef -> ValueRef -> BasicBlockRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMAddCase" addCase
    :: ValueRef -> ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMAddDestination" addDestination
    :: ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetNumClauses" getNumClauses
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetClause" getClause
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMAddClause" addClause
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMIsCleanup" isCleanup
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetCleanup" setCleanup
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMAddHandler" addHandler
    :: ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetNumHandlers" getNumHandlers
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetHandlers" getHandlers
    :: ValueRef -> (Ptr BasicBlockRef) -> IO ()

foreign import ccall unsafe "LLVMGetArgOperand" getArgOperand
    :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMSetArgOperand" setArgOperand
    :: ValueRef -> CUInt -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetParentCatchSwitch" getParentCatchSwitch
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMSetParentCatchSwitch" setParentCatchSwitch
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMBuildAdd" buildAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWAdd" buildNSWAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWAdd" buildNUWAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFAdd" buildFAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSub" buildSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWSub" buildNSWSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWSub" buildNUWSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFSub" buildFSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildMul" buildMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWMul" buildNSWMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWMul" buildNUWMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFMul" buildFMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildUDiv" buildUDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExactUDiv" buildExactUDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSDiv" buildSDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExactSDiv" buildExactSDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFDiv" buildFDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildURem" buildURem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSRem" buildSRem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFRem" buildFRem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildShl" buildShl
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLShr" buildLShr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAShr" buildAShr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAnd" buildAnd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildOr" buildOr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildXor" buildXor
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBinOp" buildBinOp
    :: BuilderRef -> Opcode -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNeg" buildNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNSWNeg" buildNSWNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNUWNeg" buildNUWNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFNeg" buildFNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildNot" buildNot
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildMalloc" buildMalloc
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildArrayMalloc" buildArrayMalloc
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildMemSet" buildMemSet
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMBuildMemCpy" buildMemCpy
    :: BuilderRef -> ValueRef -> CUInt -> ValueRef -> CUInt -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildMemMove" buildMemMove
    :: BuilderRef -> ValueRef -> CUInt -> ValueRef -> CUInt -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAlloca" buildAlloca
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildArrayAlloca" buildArrayAlloca
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFree" buildFree
    :: BuilderRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLoad" buildLoad
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildLoad2" buildLoad2
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildStore" buildStore
    :: BuilderRef -> ValueRef -> ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildGEP" buildGEP
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInBoundsGEP" buildInBoundsGEP
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildStructGEP" buildStructGEP
    :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildGEP2" buildGEP2
    :: BuilderRef -> TypeRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInBoundsGEP2" buildInBoundsGEP2
    :: BuilderRef -> TypeRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildStructGEP2" buildStructGEP2
    :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildGlobalString" buildGlobalString
    :: BuilderRef -> CString -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildGlobalStringPtr" buildGlobalStringPtr
    :: BuilderRef -> CString -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetVolatile" getVolatile
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetVolatile" setVolatile
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMGetWeak" getWeak
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetWeak" setWeak
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMGetOrdering" getOrdering
    :: ValueRef -> IO AtomicOrdering

foreign import ccall unsafe "LLVMSetOrdering" setOrdering
    :: ValueRef -> AtomicOrdering -> IO ()

foreign import ccall unsafe "LLVMGetAtomicRMWBinOp" getAtomicRMWBinOp
    :: ValueRef -> IO AtomicRMWBinOp

foreign import ccall unsafe "LLVMSetAtomicRMWBinOp" setAtomicRMWBinOp
    :: ValueRef -> AtomicRMWBinOp -> IO ()

foreign import ccall unsafe "LLVMBuildTrunc" buildTrunc
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildZExt" buildZExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSExt" buildSExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPToUI" buildFPToUI
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPToSI" buildFPToSI
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildUIToFP" buildUIToFP
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSIToFP" buildSIToFP
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPTrunc" buildFPTrunc
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPExt" buildFPExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPtrToInt" buildPtrToInt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIntToPtr" buildIntToPtr
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBitCast" buildBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAddrSpaceCast" buildAddrSpaceCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildZExtOrBitCast" buildZExtOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSExtOrBitCast" buildSExtOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildTruncOrBitCast" buildTruncOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCast" buildCast
    :: BuilderRef -> Opcode -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPointerCast" buildPointerCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIntCast2" buildIntCast2
    :: BuilderRef -> ValueRef -> TypeRef -> LLVM.Bool -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFPCast" buildFPCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildICmp" buildICmp
    :: BuilderRef -> IntPredicate -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFCmp" buildFCmp
    :: BuilderRef -> RealPredicate -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPhi" buildPhi
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCall" buildCall
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCall2" buildCall2
    :: BuilderRef -> TypeRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildSelect" buildSelect
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildVAArg" buildVAArg
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExtractElement" buildExtractElement
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInsertElement" buildInsertElement
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildShuffleVector" buildShuffleVector
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildExtractValue" buildExtractValue
    :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildInsertValue" buildInsertValue
    :: BuilderRef -> ValueRef -> ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFreeze" buildFreeze
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIsNull" buildIsNull
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildIsNotNull" buildIsNotNull
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildPtrDiff" buildPtrDiff
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildFence" buildFence
    :: BuilderRef -> AtomicOrdering -> LLVM.Bool -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAtomicRMW" buildAtomicRMW
    :: BuilderRef -> AtomicRMWBinOp -> ValueRef -> ValueRef -> AtomicOrdering -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAtomicCmpXchg" buildAtomicCmpXchg
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> AtomicOrdering -> AtomicOrdering -> LLVM.Bool -> IO ValueRef

foreign import ccall unsafe "LLVMGetNumMaskElements" getNumMaskElements
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMGetUndefMaskElem" getUndefMaskElem
    :: IO CInt

foreign import ccall unsafe "LLVMGetMaskValue" getMaskValue
    :: ValueRef -> CUInt -> IO CInt

foreign import ccall unsafe "LLVMIsAtomicSingleThread" isAtomicSingleThread
    :: ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMSetAtomicSingleThread" setAtomicSingleThread
    :: ValueRef -> LLVM.Bool -> IO ()

foreign import ccall unsafe "LLVMGetCmpXchgSuccessOrdering" getCmpXchgSuccessOrdering
    :: ValueRef -> IO AtomicOrdering

foreign import ccall unsafe "LLVMSetCmpXchgSuccessOrdering" setCmpXchgSuccessOrdering
    :: ValueRef -> AtomicOrdering -> IO ()

foreign import ccall unsafe "LLVMGetCmpXchgFailureOrdering" getCmpXchgFailureOrdering
    :: ValueRef -> IO AtomicOrdering

foreign import ccall unsafe "LLVMSetCmpXchgFailureOrdering" setCmpXchgFailureOrdering
    :: ValueRef -> AtomicOrdering -> IO ()

foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule" createModuleProviderForExistingModule
    :: ModuleRef -> IO ModuleProviderRef

foreign import ccall unsafe "LLVMDisposeModuleProvider" disposeModuleProvider
    :: ModuleProviderRef -> IO ()

foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile" createMemoryBufferWithContentsOfFile
    :: CString -> (Ptr MemoryBufferRef) -> (Ptr CString) -> IO LLVM.Bool

foreign import ccall unsafe "LLVMCreateMemoryBufferWithSTDIN" createMemoryBufferWithSTDIN
    :: (Ptr MemoryBufferRef) -> (Ptr CString) -> IO LLVM.Bool

foreign import ccall unsafe "LLVMCreateMemoryBufferWithMemoryRange" createMemoryBufferWithMemoryRange
    :: CString -> CSize -> CString -> LLVM.Bool -> IO MemoryBufferRef

foreign import ccall unsafe "LLVMCreateMemoryBufferWithMemoryRangeCopy" createMemoryBufferWithMemoryRangeCopy
    :: CString -> CSize -> CString -> IO MemoryBufferRef

foreign import ccall unsafe "LLVMGetBufferStart" getBufferStart
    :: MemoryBufferRef -> IO CString

foreign import ccall unsafe "LLVMGetBufferSize" getBufferSize
    :: MemoryBufferRef -> IO CSize

foreign import ccall unsafe "LLVMDisposeMemoryBuffer" disposeMemoryBuffer
    :: MemoryBufferRef -> IO ()

foreign import ccall unsafe "LLVMGetGlobalPassRegistry" getGlobalPassRegistry
    :: IO PassRegistryRef

foreign import ccall unsafe "LLVMCreatePassManager" createPassManager
    :: IO PassManagerRef

foreign import ccall unsafe "LLVMCreateFunctionPassManagerForModule" createFunctionPassManagerForModule
    :: ModuleRef -> IO PassManagerRef

foreign import ccall unsafe "LLVMCreateFunctionPassManager" createFunctionPassManager
    :: ModuleProviderRef -> IO PassManagerRef

foreign import ccall unsafe "LLVMRunPassManager" runPassManager
    :: PassManagerRef -> ModuleRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMInitializeFunctionPassManager" initializeFunctionPassManager
    :: PassManagerRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMRunFunctionPassManager" runFunctionPassManager
    :: PassManagerRef -> ValueRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMFinalizeFunctionPassManager" finalizeFunctionPassManager
    :: PassManagerRef -> IO LLVM.Bool

foreign import ccall unsafe "LLVMDisposePassManager" disposePassManager
    :: PassManagerRef -> IO ()

foreign import ccall unsafe "LLVMStartMultithreaded" startMultithreaded
    :: IO LLVM.Bool

foreign import ccall unsafe "LLVMStopMultithreaded" stopMultithreaded
    :: IO ()

foreign import ccall unsafe "LLVMIsMultithreaded" isMultithreaded
    :: IO LLVM.Bool
