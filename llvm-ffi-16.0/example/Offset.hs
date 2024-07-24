module Main where

import Common (withArrayLen, createExecutionEngine)

import qualified LLVM.FFI.ExecutionEngine as EE
import qualified LLVM.FFI.Target as Target
import qualified LLVM.FFI.Core as LLVM
import qualified LLVM.Target.Native as Native

import Foreign.C.String (withCString)
import Foreign.C.Types (CULLong)

import Control.Exception (bracket)


offset :: IO CULLong
offset = do
   Native.initializeNativeTarget
   int1Type <- LLVM.int1Type
   int8Type <- LLVM.int8Type
   int32Type <- LLVM.int32Type
   int64Type <- LLVM.int64Type
   structType <-
      withArrayLen [int1Type, int32Type, int8Type] $ \n ptr ->
         LLVM.structType ptr n LLVM.false
   nullPtr <- LLVM.constPointerNull structType
   zero <- LLVM.constInt int32Type 0 LLVM.false
   one <- LLVM.constInt int32Type 1 LLVM.false
   putStrLn "getElementPtr"
   -- crash
   elementPtr <-
      withArrayLen [zero,one] $ \n ixsPtr ->
         LLVM.constGEP2 structType nullPtr ixsPtr n
   putStrLn "ptrToInt"
   elementOffset <- LLVM.constPtrToInt elementPtr int64Type
   LLVM.constIntGetZExtValue elementOffset

offsetTarget :: IO [CULLong]
offsetTarget = do
   Native.initializeNativeTarget
   int1Type <- LLVM.int1Type
   int8Type <- LLVM.int8Type
   int32Type <- LLVM.int32Type
   structType <-
      withArrayLen [int1Type, int1Type, int32Type, int8Type] $ \n ptr ->
         LLVM.structType ptr n LLVM.false

   modul <- withCString "_module" LLVM.moduleCreateWithName
   withCString LLVM.hostTriple $ LLVM.setTarget modul

   bracket (createExecutionEngine modul) EE.disposeExecutionEngine $
         \execEngine -> do
      td <- EE.getExecutionEngineTargetData execEngine
      mapM (Target.offsetOfElement td structType) [0..2]

main :: IO ()
main = print =<< offsetTarget
