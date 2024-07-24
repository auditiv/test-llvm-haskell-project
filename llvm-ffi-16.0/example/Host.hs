{- |
This program shows some interesting facts about the host machine.
-}
module Main where

import Common (getString, createExecutionEngine)

import qualified LLVM.FFI.ExecutionEngine as EE
import qualified LLVM.FFI.Target as Target
import qualified LLVM.FFI.Support.Host as Host
import qualified LLVM.FFI.Core as LLVM
import qualified LLVM.Target.Native as Native

import Foreign.C.String (withCString)

import Control.Exception (bracket)


main :: IO ()
main = do
   Native.initializeNativeTarget

   putStrLn =<< getString Host.getHostCPUName
   putStrLn LLVM.hostTriple

   modul <- withCString "host" LLVM.moduleCreateWithName
   withCString LLVM.hostTriple $ LLVM.setTarget modul
   bracket (createExecutionEngine modul) EE.disposeExecutionEngine $
         \execEngine -> do
      td <- EE.getExecutionEngineTargetData execEngine
      putStrLn =<< getString (Target.copyStringRepOfTargetData td)
