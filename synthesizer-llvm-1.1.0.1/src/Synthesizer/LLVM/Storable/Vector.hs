{-# LANGUAGE TypeFamilies #-}
module Synthesizer.LLVM.Storable.Vector where

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable (Storable)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import qualified System.Unsafe as Unsafe


unsafeToPointers :: (Storable a) => SV.Vector a -> (ForeignPtr a, Ptr a, Int)
unsafeToPointers v =
   let (fp,s,l) = SVB.toForeignPtr v
   in  (fp, Unsafe.foreignPtrToPtr fp `advancePtr` s, l)
