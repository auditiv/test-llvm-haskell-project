{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.NVPTX(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeNVPTXTargetInfo
    initializeNVPTXTarget

foreign import ccall unsafe "LLVMInitializeNVPTXTargetInfo" initializeNVPTXTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeNVPTXTarget" initializeNVPTXTarget :: IO ()
