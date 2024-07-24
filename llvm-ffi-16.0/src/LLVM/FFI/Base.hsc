{-# LANGUAGE Safe #-}
module LLVM.FFI.Base where

-- import Foreign.ForeignPtr (FinalizerPtr) -- unsafe before GHC-7.10
import Foreign.Ptr (Ptr, FunPtr)

import qualified Data.Bool as Bool
import Data.Int (Int32)

import Prelude
         (IO, Eq, Enum, Show, fromIntegral, show, fromEnum, toEnum, (.), (==))

#include <llvm-c/Core.h>


type FinalizerPtr a = FunPtr (Ptr a -> IO ())


newtype Bool = Bool (#type LLVMBool)
    deriving (Eq)

instance Enum Bool where
    fromEnum (Bool b) = fromIntegral b
    toEnum = Bool . fromIntegral

instance Show Bool where
    show b = if b == false then "false" else "true"

false, true :: Bool
false = Bool 0; true = Bool 1

consBool :: Bool.Bool -> Bool
consBool = toEnum . fromEnum

deconsBool :: Bool -> Bool.Bool
deconsBool = toEnum . fromEnum
