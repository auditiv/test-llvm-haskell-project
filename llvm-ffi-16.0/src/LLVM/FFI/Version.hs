{-# LANGUAGE CPP #-}
module LLVM.FFI.Version where

{- |
Version of LLVM we have linked to.
-}
version :: Int
version = HS_LLVM_VERSION
