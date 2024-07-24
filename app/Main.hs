module Main where

import HaskellSay (haskellSay)
import Data.Word
import LLVM.FFI.Core
import LLVM.FFI.ExecutionEngine
--import LLVM.IRBuilder.Module (CodeGenModule)
--import LLVM.IRBuilder.Monad (Function)
--import Data.Int (Int32)

-- If `Function` is not correctly imported, you might need to find the specific module
-- or confirm if it's actually called `Function` in your LLVM Haskell bindings.

--mAddMul :: CodeGenModule (Function (Int32 -> Int32 -> Int32 -> IO Int32))
--mAddMul =
--  createFunction ExternalLinkage $ \x y z -> do
--    t <- add x y
--    r <- mul t z
--    ret r
-- EXAMPLE FUNCTION FOR SEEING LLVM BINDINGS
-- this function should compute: (x + y) * z
--mAddMul :: CodeGenModule (Function (Int32 -> Int32 -> Int32 -> IO Int32))
--mAddMul = 
--  createFunction ExternalLinkage $ \ x y z -> do
--    t <- add x y
--    r <- mul t z
--    ret r

main :: IO ()
main = do
  --addMul <- simpleFunction mAddMul
  --let addMul' = unsafePurify addMul
  --print(addMul' 2 3 4) -- / (2 + 3) *  4 = 24
  haskellSay "Hello, Haskell Coder! You are about to get deep see dived into the sea of Digital Ocean! Enjoy :)"


-- HELPER FUNCTION to convert IO function into pure function
--unsafePurify :: IO a -> a
--unsafePurify = unsafePerformIO
--
--
--
--
--
--
--
