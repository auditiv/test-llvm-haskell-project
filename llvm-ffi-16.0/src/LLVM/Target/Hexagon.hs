{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.Hexagon(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeHexagonTargetInfo
    initializeHexagonTarget

foreign import ccall unsafe "LLVMInitializeHexagonTargetInfo" initializeHexagonTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeHexagonTarget" initializeHexagonTarget :: IO ()
