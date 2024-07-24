module Common where

import qualified LLVM.FFI.ExecutionEngine as EE
import qualified LLVM.FFI.Core as LLVM

import qualified Foreign.C.String as CStr
import qualified Foreign.Marshal.Array as Array
import qualified Foreign.Marshal.Alloc as Alloc
import Foreign.C.Types (CUInt)
import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)

import Control.Exception (bracket)
import Control.Monad (when)

import qualified System.Exit as Exit
import Text.Printf (printf)


withArrayLen :: (Storable a) => [a] -> (CUInt -> Ptr a -> IO b) -> IO b
withArrayLen xs act =
   Array.withArrayLen xs $ \len ptr -> act (fromIntegral len) ptr

noResult :: IO () -> IO ()
noResult = id


getString :: IO CStr.CString -> IO String
getString get = bracket get LLVM.disposeMessage CStr.peekCString

createExecutionEngine :: LLVM.ModuleRef -> IO EE.ExecutionEngineRef
createExecutionEngine modul =
   Alloc.alloca $ \execEngineRef ->
   Alloc.alloca $ \errorMsgRef -> do
      err <-
         EE.createExecutionEngineForModuleCPU
            execEngineRef modul errorMsgRef
      when (err/=LLVM.false) $ do
         bracket (peek errorMsgRef) Alloc.free $ \errorMsg -> do
            noResult $
               printf "createExecutionEngine: %s\n"
                  =<< CStr.peekCString errorMsg
         Exit.exitFailure
      peek execEngineRef
