{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Raw.Module:      LLVM.FFI.Core
-- Copyright:   Bryan O'Sullivan 2007, 2008
-- License:     BSD-style (see the file LICENSE)
--
-- Maintainer:  bos@serpentine.com
-- Stability:   experimental
-- Portability: requires GHC 6.8, LLVM
--
-- This module provides direct access to the LLVM C bindings.

module LLVM.FFI.Core
    (
      initializeCore
    , Version.version

    -- * Boolean values
    , LLVM.Bool(LLVM.Bool)
    , LLVM.false
    , LLVM.true
    , LLVM.consBool
    , LLVM.deconsBool

    -- * Error handling
    , disposeMessage

    -- * Raw.Context functions
    , Raw.Context
    , ContextRef
    , contextCreate
    , contextDispose
    , getGlobalContext

    , getMDKindID
    , getMDKindIDInContext

      -- * Modules
    , Raw.Module
    , ModuleRef
    , moduleCreateWithName
    , moduleCreateWithNameInContext
    , disposeModule
    , ptrDisposeModule

    , getDataLayout
    , setDataLayout

    , getTarget
    , setTarget

    , defaultTargetTriple
    , hostTriple

    , dumpModule

    , setModuleInlineAsm
    , getModuleContext

    -- * Types
    , Raw.Type
    , TypeRef
    , TypeKind(..)

    , getTypeKind
    , typeIsSized
    , getTypeContext

    -- ** Integer types
    , int1TypeInContext
    , int8TypeInContext
    , int16TypeInContext
    , int32TypeInContext
    , int64TypeInContext
    , intTypeInContext

    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType
    , getIntTypeWidth

    -- ** Real types
    , floatTypeInContext
    , doubleTypeInContext
    , x86FP80TypeInContext
    , fp128TypeInContext
    , ppcFP128TypeInContext

    , floatType
    , doubleType
    , x86FP80Type
    , fp128Type
    , ppcFP128Type

    -- ** Function types
    , functionType
    , isFunctionVarArg
    , getReturnType
    , countParamTypes
    , getParamTypes

    -- ** Struct types
    , structTypeInContext
    , structType
    , structCreateNamed
    , getStructName
    , structSetBody

    , countStructElementTypes
    , getStructElementTypes
    , isPackedStruct
    , isOpaqueStruct

    , getTypeByName

    -- ** Array, pointer, and vector types
    , arrayType
    , pointerType
    , vectorType

    , getElementType
    , getArrayLength
    , getPointerAddressSpace
    , getVectorSize

    -- ** Other types
    , voidTypeInContext
    , labelTypeInContext
    , x86MMXTypeInContext

    , voidType
    , labelType
    , x86MMXType

    -- * Values
    , Raw.Value
    , ValueRef
    , typeOf
    , getValueName
    , setValueName
    , dumpValue
    , replaceAllUsesWith
    , hasMetadata
    , getMetadata
    , setMetadata

    -- ** Uses
    , Raw.OpaqueUse
    , UseRef
    , getFirstUse
    , getNextUse
    , getUser
    , getUsedValue

    -- ** Users
    , getOperand
    , setOperand
    , getNumOperands

    -- ** Constants
    , constNull
    , constAllOnes
    , getUndef
    , isConstant
    , isNull
    , isUndef
    , constPointerNull

    -- ** Metadata
    , mDStringInContext
    , mDString
    , mDNodeInContext
    , mDNode
    , getMDString
--    , getMDNodeNumOperands
--    , getMDNodeOperand
    , getNamedMetadataNumOperands
    , getNamedMetadataOperands

    -- ** Scalar constants
    , constInt
    , constIntOfArbitraryPrecision
    , constIntOfString
    , constIntOfStringAndSize
    , constReal
    , constRealOfString
    , constRealOfStringAndSize
    , constIntGetZExtValue
    , constIntGetSExtValue

    -- ** Composite constants
    , constStringInContext
    , constStructInContext
    , constString
    , constArray
    , constStruct
    , constNamedStruct
    , constVector

    -- ** Constant Expressions
    , getConstOpcode
    , alignOf
    , sizeOf
    , constNeg
    , constNUWNeg
    , constNSWNeg
    , constNot
    , constAdd
    , constNSWAdd
    , constNUWAdd
    , constSub
    , constNSWSub
    , constNUWSub
    , constMul
    , constNSWMul
    , constNUWMul
    , constAnd
    , constOr
    , constXor
    , constICmp
    , constFCmp
    , constShl
    , constLShr
    , constAShr
    , constGEP2
    , constInBoundsGEP2
    , constTrunc
    , constSExt
    , constZExt
    , constFPTrunc
    , constFPExt
    , constUIToFP
    , constSIToFP
    , constFPToUI
    , constFPToSI
    , constPtrToInt
    , constIntToPtr
    , constBitCast
    , constZExtOrBitCast
    , constSExtOrBitCast
    , constTruncOrBitCast
    , constPointerCast
    , constIntCast
    , constFPCast
    , constSelect
    , constExtractElement
    , constInsertElement
    , constShuffleVector
    , constInlineAsm
    , blockAddress

    -- ** Comparison predicates
    , IntPredicate(..)
    , fromIntPredicate
    , toIntPredicate
    , FPPredicate(..)
    , fromRealPredicate
    , toRealPredicate

    -- ** Floating point attributes
    , setFastMath
    , setHasUnsafeAlgebra
    , setHasNoNaNs
    , setHasNoInfs
    , setHasNoSignedZeros
    , setHasAllowReciprocal
    , setHasAllowReassoc
    , setHasApproxFunc

    -- ** Support operations and types
    , Linkage(..)
    , fromLinkage
    , toLinkage

    , Visibility(..)
    , fromVisibility
    , toVisibility

    -- ** Global variables, functions, and aliases (globals)
    , getGlobalParent
    , isDeclaration
    , getLinkage
    , setLinkage
    , getSection
    , setSection
    , getVisibility
    , setVisibility
    , getAlignment
    , setAlignment

    -- ** Global variables
    , addGlobal
    , addGlobalInAddressSpace
    , getNamedGlobal
    , getFirstGlobal
    , getLastGlobal
    , getNextGlobal
    , getPreviousGlobal
    , deleteGlobal
    , getInitializer
    , setInitializer
    , isThreadLocal
    , setThreadLocal
    , isGlobalConstant
    , setGlobalConstant

    -- ** Aliases
    , addAlias2

    -- * Parameter passing
    , Raw.Attribute
    , AttributeRef
    , AttributeKind(..)

    -- ** Calling conventions
    , CallingConvention(..)
    , fromCallingConvention
    , toCallingConvention

    -- ** Functions
    , addFunction
    , getNamedFunction
    , getFirstFunction
    , getLastFunction
    , getNextFunction
    , getPreviousFunction
    , deleteFunction
    , getIntrinsicID
    , getFunctionCallConv
    , setFunctionCallConv
    , getGC
    , setGC
    , AttributeIndex(AttributeIndex)
    , Raw.attributeReturnIndex, Raw.attributeFunctionIndex
    , getEnumAttributeKindForName
    , getLastEnumAttributeKind
    , createEnumAttribute
    , getEnumAttributeKind
    , getEnumAttributeValue
    , createStringAttribute
    , getStringAttributeKind
    , getStringAttributeValue
    , isEnumAttribute
    , isStringAttribute
    , addAttributeAtIndex
    , getAttributeCountAtIndex
    , getAttributesAtIndex
    , getEnumAttributeAtIndex
    , getStringAttributeAtIndex
    , removeEnumAttributeAtIndex
    , removeStringAttributeAtIndex
    , addTargetDependentFunctionAttr
    , addCallSiteAttribute
    , getCallSiteAttributeCount
    , getCallSiteAttributes
    , getCallSiteEnumAttribute
    , getCallSiteStringAttribute
    , removeCallSiteEnumAttribute
    , removeCallSiteStringAttribute
    , getCalledValue

    -- ** Parameters
    , countParams
    , getParams
    , getParam
    , getParamParent
    , getFirstParam
    , getLastParam
    , getNextParam
    , getPreviousParam
    , setParamAlignment

    -- ** Basic blocks
    , Raw.BasicBlock
    , BasicBlockRef
    , basicBlockAsValue
    , valueIsBasicBlock
    , valueAsBasicBlock
    , getBasicBlockName
    , getBasicBlockParent
    , getBasicBlockTerminator
    , countBasicBlocks
    , getBasicBlocks
    , getFirstBasicBlock
    , getLastBasicBlock
    , getNextBasicBlock
    , getPreviousBasicBlock
    , getEntryBasicBlock
    , appendBasicBlockInContext
    , insertBasicBlockInContext
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock
    , removeBasicBlockFromParent
    , moveBasicBlockBefore
    , moveBasicBlockAfter
    , getFirstInstruction
    , getLastInstruction

    -- ** Instructions
    , getInstructionParent
    , getNextInstruction
    , getPreviousInstruction
    , instructionEraseFromParent
    , getInstructionOpcode
    , getICmpPredicate

    -- ** Call Sites
    , getInstructionCallConv
    , setInstructionCallConv
    , setInstrParamAlignment

    -- ** Call Instructions (only)
    , isTailCall
    , setTailCall

    -- ** Switch Instructions (only)
    , getSwitchDefaultDest

    -- ** Phi nodes
    , addIncoming
    , countIncoming
    , getIncomingValue
    , getIncomingBlock

    -- * Instruction building
    , Raw.Builder
    , BuilderRef
    , createBuilderInContext
    , createBuilder
    , positionBuilder
    , positionBefore
    , positionAtEnd
    , getInsertBlock
    , clearInsertionPosition
    , insertIntoBuilder
    , insertIntoBuilderWithName
    , ptrDisposeBuilder

    -- ** Metadata
    , setCurrentDebugLocation
    , getCurrentDebugLocation
    , setInstDebugLocation

    -- ** Terminators
    , buildRetVoid
    , buildRet
    , buildAggregateRet
    , buildBr
    , buildCondBr
    , buildSwitch
    , buildIndirectBr
    , buildInvoke2
    , buildLandingPad
    , buildResume
    , buildUnreachable

    , addCase
    , addDestination
    , addClause
    , setCleanup

    -- ** Arithmetic
    , buildAdd
    , buildNSWAdd
    , buildNUWAdd
    , buildFAdd
    , buildSub
    , buildNSWSub
    , buildNUWSub
    , buildFSub
    , buildMul
    , buildNSWMul
    , buildNUWMul
    , buildFMul
    , buildUDiv
    , buildSDiv
    , buildExactSDiv
    , buildFDiv
    , buildURem
    , buildSRem
    , buildFRem
    , buildShl
    , buildLShr
    , buildAShr
    , buildAnd
    , buildOr
    , buildXor
    , buildBinOp
    , buildNeg
    , buildNSWNeg
    , buildNUWNeg
    , buildFNeg
    , buildNot

    -- ** Memory
    , buildMalloc
    , buildArrayMalloc
    , buildAlloca
    , buildArrayAlloca
    , buildFree
    , buildLoad2
    , buildStore
    , buildGEP2
    , buildInBoundsGEP2
    , buildStructGEP2
    , buildGlobalString
    , buildGlobalStringPtr

    -- ** Casts
    , buildTrunc
    , buildZExt
    , buildSExt
    , buildFPToUI
    , buildFPToSI
    , buildUIToFP
    , buildSIToFP
    , buildFPTrunc
    , buildFPExt
    , buildPtrToInt
    , buildIntToPtr
    , buildBitCast
    , buildZExtOrBitCast
    , buildSExtOrBitCast
    , buildTruncOrBitCast
    , buildCast
    , buildPointerCast
    , buildIntCast
    , buildFPCast

    -- ** Comparisons
    , buildICmp
    , buildFCmp

    -- ** Miscellaneous instructions
    , buildPhi
    , buildCall2
    , buildSelect
    , buildVAArg
    , buildExtractElement
    , buildInsertElement
    , buildShuffleVector
    , buildExtractValue
    , buildInsertValue
    , buildIsNull
    , buildIsNotNull
    , buildPtrDiff2

    -- * Memory buffers
    , Raw.MemoryBuffer
    , MemoryBufferRef
    , createMemoryBufferWithContentsOfFile
    , createMemoryBufferWithSTDIN
    , disposeMemoryBuffer

    -- ** Raw.PassRegistry
    , Raw.PassRegistry
    , PassRegistryRef
    , getGlobalPassRegistry

    -- ** Pass manager
    , Raw.PassManager
    , PassManagerRef
    , ptrDisposePassManager

    , createPassManager
    , createFunctionPassManagerForModule
    , runPassManager
    , initializeFunctionPassManager
    , runFunctionPassManager
    , finalizeFunctionPassManager
    , disposePassManager

    -- ** Functions from extras.cpp
    , getNumUses
    , instGetOpcode
    , cmpInstGetIntPredicate
    , cmpInstGetRealPredicate

    ) where

import qualified LLVM.FFI.Version as Version
import qualified LLVM.FFI.Base as LLVM
import qualified LLVM.FFI.Core14 as Core14
import qualified LLVM.Raw.Core as Raw
import LLVM.Raw.Core (
         PassRegistryRef, ContextRef, AttributeRef, AttributeIndex,
         ModuleRef, TypeRef,
         BasicBlockRef, ValueRef, UseRef, BuilderRef,
         MemoryBufferRef, PassManagerRef, PassRegistryRef, ContextRef)

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Typeable (Typeable)

import Data.Word (Word8, Word64)

import Prelude
         (IO, Eq, Ord, Bounded, Enum, Show, Read, String,
          ($), (++), (.), error,
           fmap, fromIntegral, show, toEnum, )


type CDouble  = C.CDouble
type CInt     = C.CInt
type CUInt    = C.CUInt
type CLLong   = C.CLLong
type CULLong  = C.CULLong

type FunctionRef = ValueRef


#include <llvm/Config/llvm-config.h>
#include <llvm-c/Core.h>


defaultTargetTriple, hostTriple :: String
defaultTargetTriple = (#const_str LLVM_DEFAULT_TARGET_TRIPLE)
hostTriple          = (#const_str LLVM_HOST_TRIPLE)


data TypeKind
    = VoidTypeKind
    | FloatTypeKind
    | DoubleTypeKind
    | X86_FP80TypeKind
    | FP128TypeKind
    | PPC_FP128TypeKind
    | LabelTypeKind
    | IntegerTypeKind
    | FunctionTypeKind
    | StructTypeKind
    | ArrayTypeKind
    | PointerTypeKind
    | OpaqueTypeKind
    | VectorTypeKind
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable)

getTypeKind :: TypeRef -> IO TypeKind
getTypeKind = fmap (toEnum . fromIntegral . Raw.unTypeKind) . getTypeKindRaw

data CallingConvention = C
                       | Fast
                       | Cold
                       | X86StdCall
                       | X86FastCall
                       | GHC
                         deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

fromCallingConvention :: CallingConvention -> Raw.CallingConvention
fromCallingConvention c =
    Raw.CallingConvention $
    case c of
        C -> (#const LLVMCCallConv)
        Fast -> (#const LLVMFastCallConv)
        Cold -> (#const LLVMColdCallConv)
        X86StdCall -> (#const LLVMX86FastcallCallConv)
        X86FastCall -> (#const LLVMX86StdcallCallConv)
        GHC -> 10

toCallingConvention :: Raw.CallingConvention -> CallingConvention
toCallingConvention (Raw.CallingConvention c) =
    case c of
        (#const LLVMCCallConv) -> C
        (#const LLVMFastCallConv) -> Fast
        (#const LLVMColdCallConv) -> Cold
        (#const LLVMX86StdcallCallConv) -> X86StdCall
        (#const LLVMX86FastcallCallConv) -> X86FastCall
        10 -> GHC
        _ ->
            error $ "LLVM.Core.FFI.toCallingConvention: " ++
                                "unsupported calling convention" ++ show c

-- |An enumeration for the kinds of linkage for global values.
data Linkage
    = ExternalLinkage     -- ^Externally visible function
    | AvailableExternallyLinkage
    | LinkOnceAnyLinkage  -- ^Keep one copy of function when linking (inline)
    | LinkOnceODRLinkage  -- ^Same, but only replaced by something equivalent.
    | LinkOnceODRAutoHideLinkage -- ^Like LinkOnceODR, but possibly hidden.
    | WeakAnyLinkage      -- ^Keep one copy of named function when linking (weak)
    | WeakODRLinkage      -- ^Same, but only replaced by something equivalent.
    | AppendingLinkage    -- ^Special purpose, only applies to global arrays
    | InternalLinkage     -- ^Rename collisions when linking (static functions)
    | PrivateLinkage      -- ^Like Internal, but omit from symbol table
    | DLLImportLinkage    -- ^Function to be imported from DLL
    | DLLExportLinkage    -- ^Function to be accessible from DLL
    | ExternalWeakLinkage -- ^ExternalWeak linkage description
    | GhostLinkage        -- ^Stand-in functions for streaming fns from BC files
    | CommonLinkage       -- ^Tentative definitions
    | LinkerPrivateLinkage -- ^Like Private, but linker removes.
    | LinkerPrivateWeakLinkage -- ^Like LinkerPrivate, but is weak.
    deriving (Show, Eq, Ord, Enum, Typeable)

fromLinkage :: Linkage -> Raw.Linkage
fromLinkage c =
    Raw.Linkage $
    case c of
        ExternalLinkage             -> (#const LLVMExternalLinkage)
        AvailableExternallyLinkage  -> (#const LLVMAvailableExternallyLinkage)
        LinkOnceAnyLinkage          -> (#const LLVMLinkOnceAnyLinkage)
        LinkOnceODRLinkage          -> (#const LLVMLinkOnceODRLinkage)
        LinkOnceODRAutoHideLinkage  -> (#const LLVMLinkOnceODRAutoHideLinkage)
        WeakAnyLinkage              -> (#const LLVMWeakAnyLinkage)
        WeakODRLinkage              -> (#const LLVMWeakODRLinkage)
        AppendingLinkage            -> (#const LLVMAppendingLinkage)
        InternalLinkage             -> (#const LLVMInternalLinkage)
        PrivateLinkage              -> (#const LLVMPrivateLinkage)
        DLLImportLinkage            -> (#const LLVMDLLImportLinkage)
        DLLExportLinkage            -> (#const LLVMDLLExportLinkage)
        ExternalWeakLinkage         -> (#const LLVMExternalWeakLinkage)
        GhostLinkage                -> (#const LLVMGhostLinkage)
        CommonLinkage               -> (#const LLVMCommonLinkage)
        LinkerPrivateLinkage        -> (#const LLVMLinkerPrivateLinkage)
        LinkerPrivateWeakLinkage    -> (#const LLVMLinkerPrivateWeakLinkage)

toLinkage :: Raw.Linkage -> Linkage
toLinkage (Raw.Linkage c) =
    case c of
        (#const LLVMExternalLinkage)             -> ExternalLinkage
        (#const LLVMAvailableExternallyLinkage)  -> AvailableExternallyLinkage
        (#const LLVMLinkOnceAnyLinkage)          -> LinkOnceAnyLinkage
        (#const LLVMLinkOnceODRLinkage)          -> LinkOnceODRLinkage
        (#const LLVMLinkOnceODRAutoHideLinkage)  -> LinkOnceODRAutoHideLinkage
        (#const LLVMWeakAnyLinkage)              -> WeakAnyLinkage
        (#const LLVMWeakODRLinkage)              -> WeakODRLinkage
        (#const LLVMAppendingLinkage)            -> AppendingLinkage
        (#const LLVMInternalLinkage)             -> InternalLinkage
        (#const LLVMPrivateLinkage)              -> PrivateLinkage
        (#const LLVMDLLImportLinkage)            -> DLLImportLinkage
        (#const LLVMDLLExportLinkage)            -> DLLExportLinkage
        (#const LLVMExternalWeakLinkage)         -> ExternalWeakLinkage
        (#const LLVMGhostLinkage)                -> GhostLinkage
        (#const LLVMCommonLinkage)               -> CommonLinkage
        (#const LLVMLinkerPrivateLinkage)        -> LinkerPrivateLinkage
        (#const LLVMLinkerPrivateWeakLinkage)    -> LinkerPrivateWeakLinkage
        _ -> error "toLinkage: bad value"

-- |An enumeration for the kinds of visibility of global values.
data Visibility
    = DefaultVisibility   -- ^The GV is visible
    | HiddenVisibility    -- ^The GV is hidden
    | ProtectedVisibility -- ^The GV is protected
    deriving (Show, Eq, Ord, Enum)

fromVisibility :: Visibility -> Raw.Visibility
fromVisibility c =
    Raw.Visibility $
    case c of
        DefaultVisibility   -> (#const LLVMDefaultVisibility)
        HiddenVisibility    -> (#const LLVMHiddenVisibility)
        ProtectedVisibility -> (#const LLVMProtectedVisibility)

toVisibility :: Raw.Visibility -> Visibility
toVisibility (Raw.Visibility c) =
    case c of
        (#const LLVMDefaultVisibility)   -> DefaultVisibility
        (#const LLVMHiddenVisibility)    -> HiddenVisibility
        (#const LLVMProtectedVisibility) -> ProtectedVisibility
        _ -> error "toVisibility: bad value"


data IntPredicate =
      IntEQ   -- ^ equal
    | IntNE   -- ^ not equal
    | IntUGT  -- ^ unsigned greater than
    | IntUGE  -- ^ unsigned greater or equal
    | IntULT  -- ^ unsigned less than
    | IntULE  -- ^ unsigned less or equal
    | IntSGT  -- ^ signed greater than
    | IntSGE  -- ^ signed greater or equal
    | IntSLT  -- ^ signed less than
    | IntSLE  -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show, Typeable)

fromIntPredicate :: IntPredicate -> Raw.IntPredicate
fromIntPredicate p =
    Raw.IntPredicate $
    case p of
        IntEQ  -> (#const LLVMIntEQ )
        IntNE  -> (#const LLVMIntNE )
        IntUGT -> (#const LLVMIntUGT)
        IntUGE -> (#const LLVMIntUGE)
        IntULT -> (#const LLVMIntULT)
        IntULE -> (#const LLVMIntULE)
        IntSGT -> (#const LLVMIntSGT)
        IntSGE -> (#const LLVMIntSGE)
        IntSLT -> (#const LLVMIntSLT)
        IntSLE -> (#const LLVMIntSLE)

toIntPredicate :: Raw.IntPredicate -> IntPredicate
toIntPredicate (Raw.IntPredicate p) =
    case p of
        (#const LLVMIntEQ ) -> IntEQ
        (#const LLVMIntNE ) -> IntNE
        (#const LLVMIntUGT) -> IntUGT
        (#const LLVMIntUGE) -> IntUGE
        (#const LLVMIntULT) -> IntULT
        (#const LLVMIntULE) -> IntULE
        (#const LLVMIntSGT) -> IntSGT
        (#const LLVMIntSGE) -> IntSGE
        (#const LLVMIntSLT) -> IntSLT
        (#const LLVMIntSLE) -> IntSLE
        _ -> error "toIntPredicate: bad value"

data FPPredicate =
      FPFalse -- ^ Always false (always folded)
    | FPOEQ   -- ^ True if ordered and equal
    | FPOGT   -- ^ True if ordered and greater than
    | FPOGE   -- ^ True if ordered and greater than or equal
    | FPOLT   -- ^ True if ordered and less than
    | FPOLE   -- ^ True if ordered and less than or equal
    | FPONE   -- ^ True if ordered and operands are unequal
    | FPORD   -- ^ True if ordered (no nans)
    | FPUNO   -- ^ True if unordered: isnan(X) | isnan(Y)
    | FPUEQ   -- ^ True if unordered or equal
    | FPUGT   -- ^ True if unordered or greater than
    | FPUGE   -- ^ True if unordered, greater than, or equal
    | FPULT   -- ^ True if unordered or less than
    | FPULE   -- ^ True if unordered, less than, or equal
    | FPUNE   -- ^ True if unordered or not equal
    | FPTrue  -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show, Typeable)

fromRealPredicate :: FPPredicate -> Raw.RealPredicate
fromRealPredicate p =
    Raw.RealPredicate $
    case p of
        FPFalse -> (#const LLVMRealPredicateFalse)
        FPTrue  -> (#const LLVMRealPredicateTrue)
        FPOEQ -> (#const LLVMRealOEQ)
        FPOGT -> (#const LLVMRealOGT)
        FPOGE -> (#const LLVMRealOGE)
        FPOLT -> (#const LLVMRealOLT)
        FPOLE -> (#const LLVMRealOLE)
        FPONE -> (#const LLVMRealONE)
        FPORD -> (#const LLVMRealORD)
        FPUNO -> (#const LLVMRealUNO)
        FPUEQ -> (#const LLVMRealUEQ)
        FPUGT -> (#const LLVMRealUGT)
        FPUGE -> (#const LLVMRealUGE)
        FPULT -> (#const LLVMRealULT)
        FPULE -> (#const LLVMRealULE)
        FPUNE -> (#const LLVMRealUNE)

toRealPredicate :: Raw.RealPredicate -> FPPredicate
toRealPredicate (Raw.RealPredicate p) =
    case p of
        (#const LLVMRealPredicateFalse) -> FPFalse
        (#const LLVMRealPredicateTrue) -> FPTrue
        (#const LLVMRealOEQ) -> FPOEQ
        (#const LLVMRealOGT) -> FPOGT
        (#const LLVMRealOGE) -> FPOGE
        (#const LLVMRealOLT) -> FPOLT
        (#const LLVMRealOLE) -> FPOLE
        (#const LLVMRealONE) -> FPONE
        (#const LLVMRealORD) -> FPORD
        (#const LLVMRealUNO) -> FPUNO
        (#const LLVMRealUEQ) -> FPUEQ
        (#const LLVMRealUGT) -> FPUGT
        (#const LLVMRealUGE) -> FPUGE
        (#const LLVMRealULT) -> FPULT
        (#const LLVMRealULE) -> FPULE
        (#const LLVMRealUNE) -> FPUNE
        _ -> error "toRealPredicate: bad value"



-- ** Initialization
initializeCore :: PassRegistryRef -> IO ()
initializeCore = Raw.initializeCore

-- ** Error Handling
disposeMessage :: CString -> IO ()
disposeMessage = Raw.disposeMessage

-- ** Contexts
contextCreate :: IO ContextRef
contextCreate = Raw.contextCreate

getGlobalContext :: IO ContextRef
getGlobalContext = Raw.getGlobalContext

contextDispose :: ContextRef -> IO ()
contextDispose = Raw.contextDispose

getMDKindIDInContext :: ContextRef -> CString -> CUInt -> IO CUInt
getMDKindIDInContext = Raw.getMDKindIDInContext

getMDKindID :: CString -> CUInt -> IO CUInt
getMDKindID = Raw.getMDKindID

-- ** Attributes

newtype AttributeKind = AttributeKind CUInt

getEnumAttributeKindForName :: CString -> C.CSize -> IO AttributeKind
getEnumAttributeKindForName name slen =
   fmap AttributeKind $ Raw.getEnumAttributeKindForName name slen

getLastEnumAttributeKind :: IO AttributeKind
getLastEnumAttributeKind = fmap AttributeKind $ Raw.getLastEnumAttributeKind

createEnumAttribute :: ContextRef -> AttributeKind -> Word64 -> IO AttributeRef
createEnumAttribute c (AttributeKind kindId) = Raw.createEnumAttribute c kindId

getEnumAttributeKind :: AttributeRef -> IO AttributeKind
getEnumAttributeKind = fmap AttributeKind . Raw.getEnumAttributeKind

getEnumAttributeValue :: AttributeRef -> IO Word64
getEnumAttributeValue = Raw.getEnumAttributeValue

createStringAttribute ::
   ContextRef -> CString -> CUInt -> CString -> CUInt -> IO AttributeRef
createStringAttribute = Raw.createStringAttribute

getStringAttributeKind :: AttributeRef -> Ptr CUInt -> IO CString
getStringAttributeKind = Raw.getStringAttributeKind

getStringAttributeValue :: AttributeRef -> Ptr CUInt -> IO CString
getStringAttributeValue = Raw.getStringAttributeValue

isEnumAttribute :: AttributeRef -> IO LLVM.Bool
isEnumAttribute = Raw.isEnumAttribute

isStringAttribute :: AttributeRef -> IO LLVM.Bool
isStringAttribute = Raw.isStringAttribute

-- ** Modules
moduleCreateWithName :: CString -> IO ModuleRef
moduleCreateWithName = Raw.moduleCreateWithName

moduleCreateWithNameInContext :: CString -> ContextRef -> IO ModuleRef
moduleCreateWithNameInContext = Raw.moduleCreateWithNameInContext

disposeModule :: ModuleRef -> IO ()
disposeModule = Raw.disposeModule

foreign import ccall unsafe "&LLVMDisposeModule" ptrDisposeModule
    :: LLVM.FinalizerPtr Raw.Module

-- ** Data Layout
getDataLayout :: ModuleRef -> IO CString
getDataLayout = Raw.getDataLayout

setDataLayout :: ModuleRef -> CString -> IO ()
setDataLayout = Raw.setDataLayout


-- ** Targets
getTarget :: ModuleRef -> IO CString
getTarget = Raw.getTarget

setTarget :: ModuleRef -> CString -> IO ()
setTarget = Raw.setTarget


-- ** Dump module
dumpModule :: ModuleRef -> IO ()
dumpModule = Raw.dumpModule

setModuleInlineAsm :: ModuleRef -> CString -> IO ()
setModuleInlineAsm = Raw.setModuleInlineAsm

getModuleContext :: ModuleRef -> IO ContextRef
getModuleContext = Raw.getModuleContext


-- ** Functions
addFunction :: ModuleRef -> CString -> TypeRef -> IO FunctionRef
addFunction = Raw.addFunction

getNamedFunction :: ModuleRef -> CString -> IO FunctionRef
getNamedFunction = Raw.getNamedFunction

getFirstFunction :: ModuleRef -> IO ValueRef
getFirstFunction = Raw.getFirstFunction

getLastFunction :: ModuleRef -> IO ValueRef
getLastFunction = Raw.getLastFunction

getNextFunction :: ValueRef -> IO ValueRef
getNextFunction = Raw.getNextFunction

getPreviousFunction :: ValueRef -> IO ValueRef
getPreviousFunction = Raw.getPreviousFunction


-- ** Types
getTypeKindRaw :: TypeRef -> IO Raw.TypeKind
getTypeKindRaw = Raw.getTypeKind

typeIsSized :: TypeRef -> IO LLVM.Bool
typeIsSized = Raw.typeIsSized

getTypeContext :: TypeRef -> IO ContextRef
getTypeContext = Raw.getTypeContext


-- ** Integer types
int1TypeInContext :: ContextRef -> IO TypeRef
int1TypeInContext = Raw.int1TypeInContext

int8TypeInContext :: ContextRef -> IO TypeRef
int8TypeInContext = Raw.int8TypeInContext

int16TypeInContext :: ContextRef -> IO TypeRef
int16TypeInContext = Raw.int16TypeInContext

int32TypeInContext :: ContextRef -> IO TypeRef
int32TypeInContext = Raw.int32TypeInContext

int64TypeInContext :: ContextRef -> IO TypeRef
int64TypeInContext = Raw.int64TypeInContext

intTypeInContext :: ContextRef -> CUInt -> IO TypeRef
intTypeInContext = Raw.intTypeInContext


int1Type :: IO TypeRef
int1Type = Raw.int1Type
int8Type :: IO TypeRef
int8Type = Raw.int8Type
int16Type :: IO TypeRef
int16Type = Raw.int16Type
int32Type :: IO TypeRef
int32Type = Raw.int32Type
int64Type :: IO TypeRef
int64Type = Raw.int64Type
integerType :: CUInt -> IO TypeRef
integerType = Raw.intType
getIntTypeWidth :: TypeRef -> IO CUInt
getIntTypeWidth = Raw.getIntTypeWidth


-- ** Real types
floatTypeInContext :: ContextRef -> IO TypeRef
floatTypeInContext = Raw.floatTypeInContext

doubleTypeInContext :: ContextRef -> IO TypeRef
doubleTypeInContext = Raw.doubleTypeInContext

x86FP80TypeInContext :: ContextRef -> IO TypeRef
x86FP80TypeInContext = Raw.x86FP80TypeInContext

fp128TypeInContext :: ContextRef -> IO TypeRef
fp128TypeInContext = Raw.fP128TypeInContext

ppcFP128TypeInContext :: ContextRef -> IO TypeRef
ppcFP128TypeInContext = Raw.pPCFP128TypeInContext


floatType :: IO TypeRef
floatType = Raw.floatType

doubleType :: IO TypeRef
doubleType = Raw.doubleType

x86FP80Type :: IO TypeRef
x86FP80Type = Raw.x86FP80Type

fp128Type :: IO TypeRef
fp128Type = Raw.fP128Type

ppcFP128Type :: IO TypeRef
ppcFP128Type = Raw.pPCFP128Type


-- ** Function types
-- | Create a function type.
functionType
        :: TypeRef              -- ^ return type
        -> Ptr TypeRef          -- ^ array of argument types
        -> CUInt                -- ^ number of elements in array
        -> LLVM.Bool            -- ^ non-zero if function is varargs
        -> IO TypeRef
functionType = Raw.functionType

-- | Indicate whether a function takes varargs.
isFunctionVarArg :: TypeRef -> IO LLVM.Bool
isFunctionVarArg = Raw.isFunctionVarArg

-- | Give a function's return type.
getReturnType :: TypeRef -> IO TypeRef
getReturnType = Raw.getReturnType

-- | Give the number of fixed parameters that a function takes.
countParamTypes :: TypeRef -> IO CUInt
countParamTypes = Raw.countParamTypes

-- | Fill out an array with the types of a function's fixed
-- parameters.
getParamTypes     :: TypeRef -> Ptr TypeRef -> IO ()
getParamTypes = Raw.getParamTypes


-- ** Struct Raw.Type
structTypeInContext :: ContextRef -> Ptr TypeRef -> CUInt -> LLVM.Bool -> IO TypeRef
structTypeInContext = Raw.structTypeInContext

structType :: Ptr TypeRef -> CUInt -> LLVM.Bool -> IO TypeRef
structType = Raw.structType

structCreateNamed :: ContextRef -> CString -> IO TypeRef
structCreateNamed = Raw.structCreateNamed

getStructName :: TypeRef -> IO CString
getStructName = Raw.getStructName

structSetBody :: TypeRef -> Ptr TypeRef -> CUInt -> LLVM.Bool -> IO ()
structSetBody = Raw.structSetBody

countStructElementTypes :: TypeRef -> IO CUInt
countStructElementTypes = Raw.countStructElementTypes

getStructElementTypes :: TypeRef -> Ptr TypeRef -> IO ()
getStructElementTypes = Raw.getStructElementTypes

isPackedStruct :: TypeRef -> IO LLVM.Bool
isPackedStruct = Raw.isPackedStruct

isOpaqueStruct :: TypeRef -> IO LLVM.Bool
isOpaqueStruct = Raw.isOpaqueStruct

getTypeByName :: ModuleRef -> CString -> IO TypeRef
getTypeByName = Raw.getTypeByName


-- ** Array, Pointer, and Vector types
arrayType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> IO TypeRef
arrayType = Raw.arrayType

pointerType
    :: TypeRef                  -- ^ pointed-to type
    -> CUInt                    -- ^ address space
    -> IO TypeRef
pointerType = Raw.pointerType

vectorType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> IO TypeRef
vectorType = Raw.vectorType


-- | Get the type of a sequential type's elements.
getElementType :: TypeRef -> IO TypeRef
getElementType = Raw.getElementType

getArrayLength :: TypeRef -> IO CUInt
getArrayLength = Raw.getArrayLength

getPointerAddressSpace :: TypeRef -> IO CUInt
getPointerAddressSpace = Raw.getPointerAddressSpace

getVectorSize :: TypeRef -> IO CUInt
getVectorSize = Raw.getVectorSize


-- ** Other Types

voidTypeInContext :: ContextRef -> IO TypeRef
voidTypeInContext = Raw.voidTypeInContext

labelTypeInContext :: ContextRef -> IO TypeRef
labelTypeInContext = Raw.labelTypeInContext

x86MMXTypeInContext :: ContextRef -> IO TypeRef
x86MMXTypeInContext = Raw.x86MMXTypeInContext


foreign import ccall unsafe "LLVMVoidType" voidType :: IO TypeRef
foreign import ccall unsafe "LLVMLabelType" labelType :: IO TypeRef
foreign import ccall unsafe "LLVMX86MMXType" x86MMXType :: IO TypeRef

-- ** Values
typeOf :: ValueRef -> IO TypeRef
typeOf = Raw.typeOf

getValueName :: ValueRef -> IO CString
getValueName = Raw.getValueName

setValueName :: ValueRef -> CString -> IO ()
setValueName = Raw.setValueName

dumpValue :: ValueRef -> IO ()
dumpValue = Raw.dumpValue

replaceAllUsesWith :: ValueRef -> ValueRef -> IO ()
replaceAllUsesWith = Raw.replaceAllUsesWith

hasMetadata :: ValueRef -> IO LLVM.Bool
hasMetadata = fmap (LLVM.Bool . fromIntegral) . Raw.hasMetadata

getMetadata :: ValueRef -> CUInt -> IO ValueRef
getMetadata = Raw.getMetadata

setMetadata :: ValueRef -> CUInt -> ValueRef -> IO ()
setMetadata = Raw.setMetadata


-- ** Uses
getFirstUse :: ValueRef -> IO UseRef
getFirstUse = Raw.getFirstUse

getNextUse :: UseRef -> IO UseRef
getNextUse = Raw.getNextUse

getUser :: UseRef -> IO ValueRef
getUser = Raw.getUser

getUsedValue :: UseRef -> IO ValueRef
getUsedValue = Raw.getUsedValue


-- ** Users
getOperand :: ValueRef -> CUInt -> IO ValueRef
getOperand = Raw.getOperand

setOperand :: ValueRef -> CUInt -> ValueRef -> IO ()
setOperand = Raw.setOperand

getNumOperands :: ValueRef -> IO CUInt
getNumOperands = fmap fromIntegral . Raw.getNumOperands


-- ** Constants
constNull :: TypeRef -> IO ValueRef
constNull = Raw.constNull

constAllOnes :: TypeRef -> IO ValueRef
constAllOnes = Raw.constAllOnes

getUndef :: TypeRef -> IO ValueRef
getUndef = Raw.getUndef

isConstant :: ValueRef -> IO LLVM.Bool
isConstant = Raw.isConstant

isUndef :: ValueRef -> IO LLVM.Bool
isUndef = Raw.isUndef

isNull :: ValueRef -> IO LLVM.Bool
isNull = Raw.isNull

constPointerNull :: TypeRef -> IO ValueRef
constPointerNull = Raw.constPointerNull


-- ** Metadata
mDStringInContext :: ContextRef -> CString -> CUInt -> IO ValueRef
mDStringInContext = Raw.mDStringInContext

mDString :: CString -> CUInt -> IO ValueRef
mDString = Raw.mDString

mDNodeInContext :: ContextRef -> Ptr ValueRef -> CUInt -> IO ValueRef
mDNodeInContext = Raw.mDNodeInContext

mDNode :: Ptr ValueRef -> CUInt -> IO ValueRef
mDNode = Raw.mDNode

getMDString :: ValueRef -> Ptr CUInt -> IO CString
getMDString = Raw.getMDString

{-
getMDNodeNumOperands :: ValueRef -> IO CInt
getMDNodeNumOperands = Raw.getMDNodeNumOperands

getMDNodeOperand :: ValueRef -> CUInt -> IO (Ptr ValueRef)
getMDNodeOperand = Raw.getMDNodeOperand
-}

getNamedMetadataNumOperands :: ModuleRef -> CString -> IO CUInt
getNamedMetadataNumOperands = Raw.getNamedMetadataNumOperands

getNamedMetadataOperands :: ModuleRef -> CString -> Ptr ValueRef -> IO ()
getNamedMetadataOperands = Raw.getNamedMetadataOperands


-- ** Scalar Constants
constInt :: TypeRef -> CULLong -> LLVM.Bool -> IO ValueRef
constInt = Raw.constInt

constIntOfArbitraryPrecision :: TypeRef -> CUInt -> Ptr Word64 -> IO ValueRef
constIntOfArbitraryPrecision = Raw.constIntOfArbitraryPrecision

constIntOfString :: TypeRef -> CString -> Word8 -> IO ValueRef
constIntOfString = Raw.constIntOfString

constIntOfStringAndSize :: TypeRef -> CString -> CUInt -> Word8 -> IO ValueRef
constIntOfStringAndSize = Raw.constIntOfStringAndSize

constReal :: TypeRef -> CDouble -> IO ValueRef
constReal = Raw.constReal

constRealOfString :: TypeRef -> CString -> IO ValueRef
constRealOfString = Raw.constRealOfString

constRealOfStringAndSize :: TypeRef -> CString -> CUInt -> IO ValueRef
constRealOfStringAndSize = Raw.constRealOfStringAndSize

constIntGetZExtValue :: ValueRef -> IO CULLong
constIntGetZExtValue = Raw.constIntGetZExtValue

constIntGetSExtValue :: ValueRef -> IO CLLong
constIntGetSExtValue = Raw.constIntGetSExtValue


-- ** Composite Constants
constStringInContext :: ContextRef -> CString -> CUInt -> LLVM.Bool -> IO ValueRef
constStringInContext = Raw.constStringInContext

constStructInContext :: ContextRef -> Ptr ValueRef -> CUInt -> LLVM.Bool -> IO ValueRef
constStructInContext = Raw.constStructInContext

constString :: CString -> CUInt -> LLVM.Bool -> IO ValueRef
constString = Raw.constString

constArray :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef
constArray = Raw.constArray

constStruct :: Ptr ValueRef -> CUInt -> LLVM.Bool -> IO ValueRef
constStruct = Raw.constStruct

constNamedStruct :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef
constNamedStruct = Raw.constNamedStruct

constVector :: Ptr ValueRef -> CUInt -> IO ValueRef
constVector = Raw.constVector


-- ** Constant expressions
getConstOpcode :: ValueRef -> IO Raw.Opcode
getConstOpcode = Raw.getConstOpcode

alignOf :: TypeRef -> IO ValueRef
alignOf = Raw.alignOf

sizeOf :: TypeRef -> IO ValueRef
sizeOf = Raw.sizeOf

constNeg :: ValueRef -> IO ValueRef
constNeg = Raw.constNeg

constNSWNeg :: ValueRef -> IO ValueRef
constNSWNeg = Raw.constNSWNeg

constNUWNeg :: ValueRef -> IO ValueRef
constNUWNeg = Raw.constNUWNeg

constNot :: ValueRef -> IO ValueRef
constNot = Raw.constNot

constAdd :: ValueRef -> ValueRef -> IO ValueRef
constAdd = Raw.constAdd

constNSWAdd :: ValueRef -> ValueRef -> IO ValueRef
constNSWAdd = Raw.constNSWAdd

constNUWAdd :: ValueRef -> ValueRef -> IO ValueRef
constNUWAdd = Raw.constNUWAdd

constSub :: ValueRef -> ValueRef -> IO ValueRef
constSub = Raw.constSub

constNSWSub :: ValueRef -> ValueRef -> IO ValueRef
constNSWSub = Raw.constNSWSub

constNUWSub :: ValueRef -> ValueRef -> IO ValueRef
constNUWSub = Raw.constNUWSub

constMul :: ValueRef -> ValueRef -> IO ValueRef
constMul = Raw.constMul

constNSWMul :: ValueRef -> ValueRef -> IO ValueRef
constNSWMul = Raw.constNSWMul

constNUWMul :: ValueRef -> ValueRef -> IO ValueRef
constNUWMul = Raw.constNUWMul

constAnd :: ValueRef -> ValueRef -> IO ValueRef
constAnd = Raw.constAnd

constOr :: ValueRef -> ValueRef -> IO ValueRef
constOr = Raw.constOr

constXor :: ValueRef -> ValueRef -> IO ValueRef
constXor = Raw.constXor

constICmp :: Raw.IntPredicate -> ValueRef -> ValueRef -> IO ValueRef
constICmp = Raw.constICmp

constFCmp :: Raw.RealPredicate -> ValueRef -> ValueRef -> IO ValueRef
constFCmp = Raw.constFCmp

constShl :: ValueRef -> ValueRef -> IO ValueRef
constShl = Raw.constShl

constLShr :: ValueRef -> ValueRef -> IO ValueRef
constLShr = Raw.constLShr

constAShr :: ValueRef -> ValueRef -> IO ValueRef
constAShr = Raw.constAShr

constGEP2 :: TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef
constGEP2 = Core14.constGEP2

constInBoundsGEP2 :: TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef
constInBoundsGEP2 = Core14.constInBoundsGEP2

constTrunc :: ValueRef -> TypeRef -> IO ValueRef
constTrunc = Raw.constTrunc

constSExt :: ValueRef -> TypeRef -> IO ValueRef
constSExt = Raw.constSExt

constZExt :: ValueRef -> TypeRef -> IO ValueRef
constZExt = Raw.constZExt

constFPTrunc :: ValueRef -> TypeRef -> IO ValueRef
constFPTrunc = Raw.constFPTrunc

constFPExt :: ValueRef -> TypeRef -> IO ValueRef
constFPExt = Raw.constFPExt

constUIToFP :: ValueRef -> TypeRef -> IO ValueRef
constUIToFP = Raw.constUIToFP

constSIToFP :: ValueRef -> TypeRef -> IO ValueRef
constSIToFP = Raw.constSIToFP

constFPToUI :: ValueRef -> TypeRef -> IO ValueRef
constFPToUI = Raw.constFPToUI

constFPToSI :: ValueRef -> TypeRef -> IO ValueRef
constFPToSI = Raw.constFPToSI

constPtrToInt :: ValueRef -> TypeRef -> IO ValueRef
constPtrToInt = Raw.constPtrToInt

constIntToPtr :: ValueRef -> TypeRef -> IO ValueRef
constIntToPtr = Raw.constIntToPtr

constBitCast :: ValueRef -> TypeRef -> IO ValueRef
constBitCast = Raw.constBitCast

constSExtOrBitCast :: ValueRef -> TypeRef -> IO ValueRef
constSExtOrBitCast = Raw.constSExtOrBitCast

constZExtOrBitCast :: ValueRef -> TypeRef -> IO ValueRef
constZExtOrBitCast = Raw.constZExtOrBitCast

constTruncOrBitCast :: ValueRef -> TypeRef -> IO ValueRef
constTruncOrBitCast = Raw.constTruncOrBitCast

constPointerCast :: ValueRef -> TypeRef -> IO ValueRef
constPointerCast = Raw.constPointerCast

constIntCast :: ValueRef -> TypeRef -> LLVM.Bool -> IO ValueRef
constIntCast = Raw.constIntCast

constFPCast :: ValueRef -> TypeRef -> IO ValueRef
constFPCast = Raw.constFPCast

constSelect :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef
constSelect = Raw.constSelect

constExtractElement :: ValueRef -> ValueRef -> IO ValueRef
constExtractElement = Raw.constExtractElement

constInsertElement :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef
constInsertElement = Raw.constInsertElement

constShuffleVector :: ValueRef -> ValueRef -> ValueRef -> IO ValueRef
constShuffleVector = Raw.constShuffleVector

constInlineAsm :: TypeRef -> CString -> CString -> LLVM.Bool -> LLVM.Bool -> IO ValueRef
constInlineAsm = Raw.constInlineAsm

blockAddress :: ValueRef -> BasicBlockRef -> IO ValueRef
blockAddress = Raw.blockAddress


-- ** Operations on globals
getGlobalParent :: ValueRef -> IO ModuleRef
getGlobalParent = Raw.getGlobalParent

isDeclaration :: ValueRef -> IO LLVM.Bool
isDeclaration = Raw.isDeclaration

getLinkage :: ValueRef -> IO Raw.Linkage
getLinkage = Raw.getLinkage

setLinkage :: ValueRef -> Raw.Linkage -> IO ()
setLinkage = Raw.setLinkage

getSection :: ValueRef -> IO CString
getSection = Raw.getSection

setSection :: ValueRef -> CString -> IO ()
setSection = Raw.setSection

getVisibility :: ValueRef -> IO Raw.Visibility
getVisibility = Raw.getVisibility

setVisibility :: ValueRef -> Raw.Visibility -> IO ()
setVisibility = Raw.setVisibility

getAlignment :: ValueRef -> IO CUInt
getAlignment = Raw.getAlignment

setAlignment :: ValueRef -> CUInt -> IO ()
setAlignment = Raw.setAlignment


-- ** Global Variables
addGlobal :: ModuleRef -> TypeRef -> CString -> IO ValueRef
addGlobal = Raw.addGlobal

addGlobalInAddressSpace :: ModuleRef -> TypeRef -> CString -> CUInt -> IO ValueRef
addGlobalInAddressSpace = Raw.addGlobalInAddressSpace

getNamedGlobal :: ModuleRef -> CString -> IO ValueRef
getNamedGlobal = Raw.getNamedGlobal

getFirstGlobal :: ModuleRef -> IO ValueRef
getFirstGlobal = Raw.getFirstGlobal

getLastGlobal :: ModuleRef -> IO ValueRef
getLastGlobal = Raw.getLastGlobal

getNextGlobal :: ValueRef -> IO ValueRef
getNextGlobal = Raw.getNextGlobal

getPreviousGlobal :: ValueRef -> IO ValueRef
getPreviousGlobal = Raw.getPreviousGlobal

deleteGlobal :: ValueRef -> IO ()
deleteGlobal = Raw.deleteGlobal

setInitializer :: ValueRef -> ValueRef -> IO ()
setInitializer = Raw.setInitializer

getInitializer :: ValueRef -> IO ValueRef
getInitializer = Raw.getInitializer

isThreadLocal :: ValueRef -> IO LLVM.Bool
isThreadLocal = Raw.isThreadLocal

setThreadLocal :: ValueRef -> LLVM.Bool -> IO ()
setThreadLocal = Raw.setThreadLocal

isGlobalConstant :: ValueRef -> IO LLVM.Bool
isGlobalConstant = Raw.isGlobalConstant

setGlobalConstant :: ValueRef -> LLVM.Bool -> IO ()
setGlobalConstant = Raw.setGlobalConstant


-- ** Aliases
addAlias2 :: ModuleRef -> TypeRef -> CUInt -> ValueRef -> CString -> IO ValueRef
addAlias2 = Core14.addAlias2

deleteFunction :: FunctionRef -> IO ()
deleteFunction = Raw.deleteFunction

getIntrinsicID :: FunctionRef -> IO CUInt
getIntrinsicID = Raw.getIntrinsicID

getFunctionCallConv :: FunctionRef -> IO Raw.CallingConvention
getFunctionCallConv = fmap Raw.CallingConvention . Raw.getFunctionCallConv

setFunctionCallConv :: FunctionRef -> Raw.CallingConvention -> IO ()
setFunctionCallConv f = Raw.setFunctionCallConv f . Raw.unCallingConvention

getGC :: ValueRef -> IO CString
getGC = Raw.getGC

setGC :: ValueRef -> CString -> IO ()
setGC = Raw.setGC


-- ** Raw.Attribute attachment

addAttributeAtIndex :: ValueRef -> AttributeIndex -> AttributeRef -> IO ()
addAttributeAtIndex = Raw.addAttributeAtIndex

getAttributeCountAtIndex :: ValueRef -> AttributeIndex -> IO CUInt
getAttributeCountAtIndex = Raw.getAttributeCountAtIndex

getAttributesAtIndex :: ValueRef -> AttributeIndex -> Ptr AttributeRef -> IO ()
getAttributesAtIndex = Raw.getAttributesAtIndex

getEnumAttributeAtIndex :: ValueRef -> AttributeIndex -> AttributeKind -> IO AttributeRef
getEnumAttributeAtIndex v i (AttributeKind kindId) = Raw.getEnumAttributeAtIndex v i kindId

getStringAttributeAtIndex :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO AttributeRef
getStringAttributeAtIndex = Raw.getStringAttributeAtIndex

removeEnumAttributeAtIndex :: ValueRef -> AttributeIndex -> AttributeKind -> IO ()
removeEnumAttributeAtIndex v i (AttributeKind kindId) = Raw.removeEnumAttributeAtIndex v i kindId

removeStringAttributeAtIndex :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO ()
removeStringAttributeAtIndex = Raw.removeStringAttributeAtIndex

addTargetDependentFunctionAttr :: ValueRef -> CString -> CString -> IO ()
addTargetDependentFunctionAttr = Raw.addTargetDependentFunctionAttr


-- ** Parameters
countParams :: FunctionRef -> IO CUInt
countParams = Raw.countParams

getParams
    :: FunctionRef
    -> Ptr ValueRef             -- ^ array to fill out
    -> IO ()
getParams = Raw.getParams

getParam
    :: FunctionRef
    -> CUInt                    -- ^ offset into array
    -> IO ValueRef
getParam = Raw.getParam

getParamParent :: ValueRef -> IO ValueRef
getParamParent = Raw.getParamParent

getFirstParam :: ValueRef -> IO ValueRef
getFirstParam = Raw.getFirstParam

getLastParam :: ValueRef -> IO ValueRef
getLastParam = Raw.getLastParam

getNextParam :: ValueRef -> IO ValueRef
getNextParam = Raw.getNextParam

getPreviousParam :: ValueRef -> IO ValueRef
getPreviousParam = Raw.getPreviousParam

setParamAlignment :: ValueRef -> CUInt -> IO ()
setParamAlignment = Raw.setParamAlignment


-- ** Basic Blocks
basicBlockAsValue :: BasicBlockRef -> IO ValueRef
basicBlockAsValue = Raw.basicBlockAsValue

valueIsBasicBlock :: ValueRef -> IO LLVM.Bool
valueIsBasicBlock = Raw.valueIsBasicBlock

valueAsBasicBlock
    :: ValueRef                 -- ^ basic block
    -> IO BasicBlockRef
valueAsBasicBlock = Raw.valueAsBasicBlock

getBasicBlockName :: BasicBlockRef -> IO CString
getBasicBlockName = Raw.getBasicBlockName

getBasicBlockParent :: BasicBlockRef -> IO ValueRef
getBasicBlockParent = Raw.getBasicBlockParent

getBasicBlockTerminator :: BasicBlockRef -> IO ValueRef
getBasicBlockTerminator = Raw.getBasicBlockTerminator

countBasicBlocks
    :: ValueRef                 -- ^ function
    -> IO CUInt
countBasicBlocks = Raw.countBasicBlocks

getBasicBlocks
    :: ValueRef                 -- ^ function
    -> Ptr BasicBlockRef        -- ^ array to fill out
    -> IO ()
getBasicBlocks = Raw.getBasicBlocks

getFirstBasicBlock :: ValueRef -> IO BasicBlockRef
getFirstBasicBlock = Raw.getFirstBasicBlock

getLastBasicBlock :: ValueRef -> IO BasicBlockRef
getLastBasicBlock = Raw.getLastBasicBlock

getNextBasicBlock :: BasicBlockRef -> IO BasicBlockRef
getNextBasicBlock = Raw.getNextBasicBlock

getPreviousBasicBlock :: BasicBlockRef -> IO BasicBlockRef
getPreviousBasicBlock = Raw.getPreviousBasicBlock

getEntryBasicBlock
    :: ValueRef                 -- ^ function
    -> IO BasicBlockRef
getEntryBasicBlock = Raw.getEntryBasicBlock

appendBasicBlockInContext :: ContextRef -> ValueRef -> CString -> IO BasicBlockRef
appendBasicBlockInContext = Raw.appendBasicBlockInContext

insertBasicBlockInContext :: ContextRef -> BasicBlockRef -> CString -> IO BasicBlockRef
insertBasicBlockInContext = Raw.insertBasicBlockInContext

appendBasicBlock
    :: ValueRef                 -- ^ function
    -> CString                  -- ^ name for label
    -> IO BasicBlockRef
appendBasicBlock = Raw.appendBasicBlock

insertBasicBlock
    :: BasicBlockRef            -- ^ insert before this one
    -> CString                  -- ^ name for label
    -> IO BasicBlockRef
insertBasicBlock = Raw.insertBasicBlock

deleteBasicBlock :: BasicBlockRef -> IO ()
deleteBasicBlock = Raw.deleteBasicBlock

removeBasicBlockFromParent :: BasicBlockRef -> IO ()
removeBasicBlockFromParent = Raw.removeBasicBlockFromParent

moveBasicBlockBefore :: BasicBlockRef -> BasicBlockRef -> IO ()
moveBasicBlockBefore = Raw.moveBasicBlockBefore

moveBasicBlockAfter :: BasicBlockRef -> BasicBlockRef -> IO ()
moveBasicBlockAfter = Raw.moveBasicBlockAfter

getFirstInstruction :: BasicBlockRef -> IO ValueRef
getFirstInstruction = Raw.getFirstInstruction

getLastInstruction :: BasicBlockRef -> IO ValueRef
getLastInstruction = Raw.getLastInstruction


-- ** Instructions
getInstructionParent :: ValueRef -> IO BasicBlockRef
getInstructionParent = Raw.getInstructionParent

getNextInstruction :: ValueRef -> IO ValueRef
getNextInstruction = Raw.getNextInstruction

getPreviousInstruction :: ValueRef -> IO ValueRef
getPreviousInstruction = Raw.getPreviousInstruction

instructionEraseFromParent :: ValueRef -> IO ()
instructionEraseFromParent = Raw.instructionEraseFromParent

getInstructionOpcode :: ValueRef -> IO Raw.Opcode
getInstructionOpcode = Raw.getInstructionOpcode

getICmpPredicate :: ValueRef -> IO Raw.IntPredicate
getICmpPredicate = Raw.getICmpPredicate


-- ** Call sites
setInstructionCallConv :: ValueRef -> Raw.CallingConvention -> IO ()
setInstructionCallConv v =
   Raw.setInstructionCallConv v . Raw.unCallingConvention

getInstructionCallConv :: ValueRef -> IO Raw.CallingConvention
getInstructionCallConv =
   fmap Raw.CallingConvention . Raw.getInstructionCallConv

setInstrParamAlignment :: ValueRef -> AttributeIndex -> CUInt -> IO ()
setInstrParamAlignment = Raw.setInstrParamAlignment


addCallSiteAttribute :: ValueRef -> AttributeIndex -> AttributeRef -> IO ()
addCallSiteAttribute = Raw.addCallSiteAttribute

getCallSiteAttributeCount :: ValueRef -> AttributeIndex -> IO CUInt
getCallSiteAttributeCount = Raw.getCallSiteAttributeCount

getCallSiteAttributes :: ValueRef -> AttributeIndex -> Ptr AttributeRef -> IO ()
getCallSiteAttributes = Raw.getCallSiteAttributes

getCallSiteEnumAttribute :: ValueRef -> AttributeIndex -> AttributeKind -> IO AttributeRef
getCallSiteEnumAttribute v i (AttributeKind kindId) = Raw.getCallSiteEnumAttribute v i kindId

getCallSiteStringAttribute :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO AttributeRef
getCallSiteStringAttribute = Raw.getCallSiteStringAttribute

removeCallSiteEnumAttribute :: ValueRef -> AttributeIndex -> AttributeKind -> IO ()
removeCallSiteEnumAttribute v i (AttributeKind kindId) = Raw.removeCallSiteEnumAttribute v i kindId

removeCallSiteStringAttribute :: ValueRef -> AttributeIndex -> CString -> CUInt -> IO ()
removeCallSiteStringAttribute = Raw.removeCallSiteStringAttribute

getCalledValue :: ValueRef -> IO ValueRef
getCalledValue = Raw.getCalledValue


-- ** Call instructions
isTailCall :: ValueRef -> IO LLVM.Bool
isTailCall = Raw.isTailCall

setTailCall :: ValueRef -> LLVM.Bool -> IO ()
setTailCall = Raw.setTailCall


-- ** Switch Instructions
getSwitchDefaultDest :: ValueRef -> IO BasicBlockRef
getSwitchDefaultDest = Raw.getSwitchDefaultDest


-- ** Phi Nodes
addIncoming :: ValueRef -> Ptr ValueRef -> Ptr BasicBlockRef -> CUInt -> IO ()
addIncoming = Raw.addIncoming

countIncoming :: ValueRef -> IO CUInt
countIncoming = Raw.countIncoming

getIncomingValue :: ValueRef -> CUInt -> IO ValueRef
getIncomingValue = Raw.getIncomingValue

getIncomingBlock :: ValueRef -> CUInt -> IO BasicBlockRef
getIncomingBlock = Raw.getIncomingBlock


-- ** Builders
createBuilderInContext :: ContextRef -> IO BuilderRef
createBuilderInContext = Raw.createBuilderInContext

createBuilder :: IO BuilderRef
createBuilder = Raw.createBuilder

positionBuilder :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()
positionBuilder = Raw.positionBuilder

positionBefore :: BuilderRef -> ValueRef -> IO ()
positionBefore = Raw.positionBuilderBefore

positionAtEnd :: BuilderRef -> BasicBlockRef -> IO ()
positionAtEnd = Raw.positionBuilderAtEnd

getInsertBlock :: BuilderRef -> IO BasicBlockRef
getInsertBlock = Raw.getInsertBlock

clearInsertionPosition :: BuilderRef -> IO ()
clearInsertionPosition = Raw.clearInsertionPosition

insertIntoBuilder :: BuilderRef -> ValueRef -> IO ()
insertIntoBuilder = Raw.insertIntoBuilder

insertIntoBuilderWithName :: BuilderRef -> ValueRef -> CString -> IO ()
insertIntoBuilderWithName = Raw.insertIntoBuilderWithName

foreign import ccall unsafe "&LLVMDisposeBuilder" ptrDisposeBuilder
    :: LLVM.FinalizerPtr Raw.Builder


-- ** Metadata
getCurrentDebugLocation :: BuilderRef -> IO ValueRef
getCurrentDebugLocation = Raw.getCurrentDebugLocation

setCurrentDebugLocation :: BuilderRef -> ValueRef -> IO ()
setCurrentDebugLocation = Raw.setCurrentDebugLocation

setInstDebugLocation :: BuilderRef -> ValueRef -> IO ()
setInstDebugLocation = Raw.setInstDebugLocation


-- ** Terminators
buildRetVoid :: BuilderRef -> IO ValueRef
buildRetVoid = Raw.buildRetVoid

buildRet :: BuilderRef -> ValueRef -> IO ValueRef
buildRet = Raw.buildRet

buildAggregateRet :: BuilderRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
buildAggregateRet = Raw.buildAggregateRet

buildBr :: BuilderRef -> BasicBlockRef -> IO ValueRef
buildBr = Raw.buildBr

buildCondBr :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef -> IO ValueRef
buildCondBr = Raw.buildCondBr

buildSwitch :: BuilderRef -> ValueRef -> BasicBlockRef -> CUInt -> IO ValueRef
buildSwitch = Raw.buildSwitch

buildIndirectBr :: BuilderRef -> ValueRef -> CUInt -> IO ValueRef
buildIndirectBr = Raw.buildIndirectBr

buildInvoke2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef
buildInvoke2 = Core14.buildInvoke2

buildLandingPad :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef
buildLandingPad = Raw.buildLandingPad

buildResume :: BuilderRef -> ValueRef -> IO ValueRef
buildResume = Raw.buildResume

buildUnreachable :: BuilderRef -> IO ValueRef
buildUnreachable = Raw.buildUnreachable


-- ** Switch instructions
addCase :: ValueRef -> ValueRef -> BasicBlockRef -> IO ()
addCase = Raw.addCase


-- ** IndirectBr instructions
addDestination :: ValueRef -> BasicBlockRef -> IO ()
addDestination = Raw.addDestination


-- ** LandingPad instructions
addClause :: ValueRef -> ValueRef -> IO ()
addClause = Raw.addClause


-- ** Resume instructions
setCleanup :: ValueRef -> LLVM.Bool -> IO ()
setCleanup = Raw.setCleanup


-- ** Arithmetic
buildAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildAdd = Raw.buildAdd

buildNSWAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildNSWAdd = Raw.buildNSWAdd

buildNUWAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildNUWAdd = Raw.buildNUWAdd

buildFAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildFAdd = Raw.buildFAdd

buildSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildSub = Raw.buildSub

buildNSWSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildNSWSub = Raw.buildNSWSub

buildNUWSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildNUWSub = Raw.buildNUWSub

buildFSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildFSub = Raw.buildFSub

buildMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildMul = Raw.buildMul

buildNSWMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildNSWMul = Raw.buildNSWMul

buildNUWMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildNUWMul = Raw.buildNUWMul

buildFMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildFMul = Raw.buildFMul

buildUDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildUDiv = Raw.buildUDiv

buildSDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildSDiv = Raw.buildSDiv

buildExactSDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildExactSDiv = Raw.buildExactSDiv

buildFDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildFDiv = Raw.buildFDiv

buildURem :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildURem = Raw.buildURem

buildSRem :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildSRem = Raw.buildSRem

buildFRem :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildFRem = Raw.buildFRem

buildShl :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildShl = Raw.buildShl

buildLShr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildLShr = Raw.buildLShr

buildAShr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildAShr = Raw.buildAShr

buildAnd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildAnd = Raw.buildAnd

buildOr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildOr = Raw.buildOr

buildXor :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildXor = Raw.buildXor

buildBinOp :: BuilderRef -> Raw.Opcode -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildBinOp = Raw.buildBinOp

buildNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildNeg = Raw.buildNeg

buildNSWNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildNSWNeg = Raw.buildNSWNeg

buildNUWNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildNUWNeg = Raw.buildNUWNeg

buildFNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildFNeg = Raw.buildFNeg

buildNot :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildNot = Raw.buildNot


-- ** Floating point attributes
foreign import ccall unsafe "LLVMSetHasUnsafeAlgebra" setFastMath
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasUnsafeAlgebra" setHasUnsafeAlgebra
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasNoNaNs" setHasNoNaNs
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasNoInfs" setHasNoInfs
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasNoSignedZeros" setHasNoSignedZeros
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasAllowReciprocal" setHasAllowReciprocal
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasAllowReassoc" setHasAllowReassoc
    :: ValueRef -> LLVM.Bool -> IO ()
foreign import ccall unsafe "LLVMSetHasApproxFunc" setHasApproxFunc
    :: ValueRef -> LLVM.Bool -> IO ()


-- ** Memory
buildMalloc :: BuilderRef -> TypeRef -> CString -> IO ValueRef
buildMalloc = Raw.buildMalloc

buildArrayMalloc :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
buildArrayMalloc = Raw.buildArrayMalloc

buildAlloca :: BuilderRef -> TypeRef -> CString -> IO ValueRef
buildAlloca = Raw.buildAlloca

buildArrayAlloca :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
buildArrayAlloca = Raw.buildArrayAlloca

buildFree :: BuilderRef -> ValueRef -> IO ValueRef
buildFree = Raw.buildFree

buildLoad2 :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
buildLoad2 = Core14.buildLoad2

buildStore :: BuilderRef -> ValueRef -> ValueRef -> IO ValueRef
buildStore = Raw.buildStore

buildGEP2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
buildGEP2 = Core14.buildGEP2

buildInBoundsGEP2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
buildInBoundsGEP2 = Core14.buildInBoundsGEP2

buildStructGEP2 :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef
buildStructGEP2 = Core14.buildStructGEP2

buildGlobalString :: BuilderRef -> CString -> CString -> IO ValueRef
buildGlobalString = Raw.buildGlobalString

buildGlobalStringPtr :: BuilderRef -> CString -> CString -> IO ValueRef
buildGlobalStringPtr = Raw.buildGlobalStringPtr


-- Casts
buildTrunc :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildTrunc = Raw.buildTrunc

buildZExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildZExt = Raw.buildZExt

buildSExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildSExt = Raw.buildSExt

buildFPToUI :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildFPToUI = Raw.buildFPToUI

buildFPToSI :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildFPToSI = Raw.buildFPToSI

buildUIToFP :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildUIToFP = Raw.buildUIToFP

buildSIToFP :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildSIToFP = Raw.buildSIToFP

buildFPTrunc :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildFPTrunc = Raw.buildFPTrunc

buildFPExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildFPExt = Raw.buildFPExt

buildPtrToInt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildPtrToInt = Raw.buildPtrToInt

buildIntToPtr :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildIntToPtr = Raw.buildIntToPtr

buildBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildBitCast = Raw.buildBitCast

buildZExtOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildZExtOrBitCast = Raw.buildZExtOrBitCast

buildSExtOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildSExtOrBitCast = Raw.buildSExtOrBitCast

buildTruncOrBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildTruncOrBitCast = Raw.buildTruncOrBitCast

buildCast :: BuilderRef -> Raw.Opcode -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildCast = Raw.buildCast

buildPointerCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildPointerCast = Raw.buildPointerCast

foreign import ccall unsafe "LLVMBuildIntCast" buildIntCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

buildFPCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildFPCast = Raw.buildFPCast


-- Comparisons
buildICmp :: BuilderRef -> Raw.IntPredicate -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildICmp = Raw.buildICmp

buildFCmp :: BuilderRef -> Raw.RealPredicate -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildFCmp = Raw.buildFCmp


-- Miscellaneous instructions
buildPhi :: BuilderRef -> TypeRef -> CString -> IO ValueRef
buildPhi = Raw.buildPhi

buildCall2 :: BuilderRef -> TypeRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
buildCall2 = Core14.buildCall2

buildSelect :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildSelect = Raw.buildSelect

buildVAArg :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
buildVAArg = Raw.buildVAArg

buildExtractElement :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildExtractElement = Raw.buildExtractElement

buildInsertElement :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildInsertElement = Raw.buildInsertElement

buildShuffleVector :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildShuffleVector = Raw.buildShuffleVector

buildExtractValue :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef
buildExtractValue = Raw.buildExtractValue

buildInsertValue :: BuilderRef -> ValueRef -> ValueRef -> CUInt -> CString -> IO ValueRef
buildInsertValue = Raw.buildInsertValue

buildIsNull :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildIsNull = Raw.buildIsNull

buildIsNotNull :: BuilderRef -> ValueRef -> CString -> IO ValueRef
buildIsNotNull = Raw.buildIsNotNull

buildPtrDiff2 :: BuilderRef -> TypeRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
buildPtrDiff2 = Core14.buildPtrDiff2


-- ** Memory Buffers
createMemoryBufferWithContentsOfFile :: CString -> Ptr MemoryBufferRef -> Ptr CString -> IO LLVM.Bool
createMemoryBufferWithContentsOfFile = Raw.createMemoryBufferWithContentsOfFile

createMemoryBufferWithSTDIN :: Ptr MemoryBufferRef -> Ptr CString -> IO LLVM.Bool
createMemoryBufferWithSTDIN = Raw.createMemoryBufferWithSTDIN

disposeMemoryBuffer :: MemoryBufferRef -> IO ()
disposeMemoryBuffer = Raw.disposeMemoryBuffer


-- ** Pass Registry
getGlobalPassRegistry :: IO PassRegistryRef
getGlobalPassRegistry = Raw.getGlobalPassRegistry


-- ** Pass Managers
createPassManager :: IO PassManagerRef
createPassManager = Raw.createPassManager

createFunctionPassManagerForModule :: ModuleRef -> IO PassManagerRef
createFunctionPassManagerForModule = Raw.createFunctionPassManagerForModule

runPassManager :: PassManagerRef -> ModuleRef -> IO LLVM.Bool
runPassManager = Raw.runPassManager

initializeFunctionPassManager :: PassManagerRef -> IO LLVM.Bool
initializeFunctionPassManager = Raw.initializeFunctionPassManager

runFunctionPassManager :: PassManagerRef -> ValueRef -> IO LLVM.Bool
runFunctionPassManager = Raw.runFunctionPassManager

finalizeFunctionPassManager :: PassManagerRef -> IO LLVM.Bool
finalizeFunctionPassManager = Raw.finalizeFunctionPassManager

disposePassManager :: PassManagerRef -> IO ()
disposePassManager = Raw.disposePassManager

foreign import ccall unsafe "&LLVMDisposePassManager" ptrDisposePassManager
    :: LLVM.FinalizerPtr Raw.PassManager


-- ** Functions from extras.cpp
foreign import ccall unsafe "LLVMValueGetNumUses" getNumUses
    :: ValueRef -> IO CInt
foreign import ccall unsafe "LLVMInstGetOpcode" instGetOpcode
    :: ValueRef -> IO CInt
foreign import ccall unsafe "LLVMCmpInstGetPredicate" cmpInstGetIntPredicate
    :: ValueRef -> IO Raw.IntPredicate
foreign import ccall unsafe "LLVMCmpInstGetPredicate" cmpInstGetRealPredicate
    :: ValueRef -> IO Raw.RealPredicate
