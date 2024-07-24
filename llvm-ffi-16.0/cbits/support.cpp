#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "llvm-c/Core.h"
#include "llvm-c/Target.h"

#include "llvm-c/ExecutionEngine.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Support/Host.h"

#include "support.h"

using namespace llvm;

#if HS_LLVM_VERSION < 305
#define nullptr 0
#endif


unsigned LLVMInitNativeTarget()
{
    LLVMBool init = LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmParser();
    LLVMInitializeNativeAsmPrinter();
    return init;
}

unsigned LLVMInstGetOpcode(LLVMValueRef inst)
{
    llvm::Instruction *instp = llvm::unwrap<llvm::Instruction>(inst);
    assert(instp);
    return instp->getOpcode();
}

unsigned LLVMCmpInstGetPredicate(LLVMValueRef cmpinst)
{
    llvm::CmpInst *instp = llvm::unwrap<llvm::CmpInst>(cmpinst);
    assert(instp);
    return instp->getPredicate();
}

unsigned LLVMValueGetNumUses(LLVMValueRef value)
{
    llvm::Value *valuep = llvm::unwrap(value);
    assert(valuep);
    return valuep->getNumUses();
}


#if HS_LLVM_VERSION < 700
char *LLVMGetHostCPUName(void) {
  return strdup(sys::getHostCPUName().data());
}
#endif



/*
getHostCPUFeatures supports X86 only starting with LLVM-3.7
How does llc -mattr=help work? It seems to emit directly to stderr.
*/
LLVMFeatureMapRef LLVMGetHostFeatures() {
  LLVMFeatureMapRef features = new(LLVMFeatureMap);
  if (sys::getHostCPUFeatures(*features)) {
    return features;
  } else {
    delete features;
    return nullptr;
  }
}

void LLVMFreeFeatures(LLVMFeatureMapRef features) {
  delete(features);
}

LLVMFeatureIteratorRef LLVMCheckFeature
    (LLVMFeatureMapRef features, LLVMFeatureIteratorRef featureRef) {
  if (*featureRef == features->end()) {
    delete featureRef;
    return nullptr;
  } else {
    return featureRef;
  }
}

LLVMFeatureIteratorRef LLVMGetFirstFeature(LLVMFeatureMapRef features) {
  return LLVMCheckFeature(features, new LLVMFeatureIterator(features->begin()));
}

LLVMFeatureIteratorRef LLVMGetNextFeature
    (LLVMFeatureMapRef features, LLVMFeatureIteratorRef featureRef) {
  (*featureRef)++;
  return LLVMCheckFeature(features, featureRef);
}

const char *LLVMGetFeatureName(LLVMFeatureIteratorRef featureRef) {
  return (*featureRef)->first().data();
}

LLVMBool LLVMGetFeatureSupport(LLVMFeatureIteratorRef featureRef) {
  return (*featureRef)->second;
}



LLVMBool LLVMCreateExecutionEngineKindForModuleCPU
  (LLVMExecutionEngineRef *OutEE,
   LLVMEngineKind kind,
   LLVMModuleRef M,
   char **OutError) {
  std::string Error;
#if HS_LLVM_VERSION < 306
  EngineBuilder builder(unwrap(M));
#else
  EngineBuilder builder(std::unique_ptr<Module>(unwrap(M)));
#endif
  builder.setEngineKind((EngineKind::Kind)kind)
         .setMCPU(sys::getHostCPUName().data())
         .setErrorStr(&Error);
  if (ExecutionEngine *EE = builder.create()){
    *OutEE = wrap(EE);
    return 0;
  }
  *OutError = strdup(Error.c_str());
  return 1;
}


void LLVMSetHasUnsafeAlgebra(LLVMValueRef Instr, LLVMBool B) {
#if HS_LLVM_VERSION < 600
  (unwrap<Instruction>(Instr))->setHasUnsafeAlgebra(B);
#else
  (unwrap<Instruction>(Instr))->setFast(B);
#endif
}
void LLVMSetHasNoNaNs(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasNoNaNs(B);
}
void LLVMSetHasNoInfs(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasNoInfs(B);
}
void LLVMSetHasNoSignedZeros(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasNoSignedZeros(B);
}
void LLVMSetHasAllowReciprocal(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasAllowReciprocal(B);
}
void LLVMSetHasAllowReassoc(LLVMValueRef Instr, LLVMBool B) {
#if HS_LLVM_VERSION >= 600
  (unwrap<Instruction>(Instr))->setHasAllowReassoc(B);
#endif
}
/* It is not exported by Instruction.h
void LLVMSetHasAllowContract(LLVMValueRef Instr, LLVMBool B) {
#if HS_LLVM_VERSION >= 500
  (unwrap<Instruction>(Instr))->setHasAllowContract(B);
#endif
}
*/
void LLVMSetHasApproxFunc(LLVMValueRef Instr, LLVMBool B) {
#if HS_LLVM_VERSION >= 600
  (unwrap<Instruction>(Instr))->setHasApproxFunc(B);
#endif
}
