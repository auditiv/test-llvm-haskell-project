{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Plug.Output (
   T(..),
   Default(..),
   split,
   storableVector,
   ) where

import qualified Synthesizer.Zip as Zip

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory

import qualified LLVM.Core as LLVM

import Control.Applicative (liftA2)

import qualified Synthesizer.LLVM.Storable.Vector as SVU
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import qualified Foreign.ForeignPtr as FPtr

import Data.Tuple.Strict (zipPair)


data T a b =
   forall state ioContext parameters.
      (Marshal.C parameters, Memory.C state) =>
   Cons
      (forall r.
       MultiValue.T parameters -> a -> state -> LLVM.CodeGenFunction r state)
         -- compute next value
      (forall r. MultiValue.T parameters -> LLVM.CodeGenFunction r state)
         -- initial state
      (Int -> IO (ioContext, parameters))
         {- initialization from IO monad
         This is called once per output chunk
         with the number of input samples.
         This number is also the maximum possible number of output samples.
         This will be run within Unsafe.performIO,
         so no observable In/Out actions please!
         -}
      (Int -> ioContext -> IO b)
         {-
         finalization from IO monad, also run within Unsafe.performIO
         The integer argument is the actually produced size of data.
         We must clip the allocated output vectors accordingly.
         -}


class Default b where
   type Element b
   deflt :: T (Element b) b


instance (Default c, Default d) => Default (Zip.T c d) where
   type Element (Zip.T c d) = (Element c, Element d)
   deflt = split deflt deflt

split :: T a c -> T b d -> T (a,b) (Zip.T c d)
split (Cons nextA startA createA deleteA)
      (Cons nextB startB createB deleteB) = Cons
   (MultiValue.uncurry $ \parameterA parameterB (a,b) (sa,sb) ->
      liftA2 (,) (nextA parameterA a sa) (nextB parameterB b sb))
   (MultiValue.uncurry $ \parameterA parameterB ->
      liftA2 (,) (startA parameterA) (startB parameterB))
   (\len -> liftA2 zipPair (createA len) (createB len))
   (\len (ca,cb) -> liftA2 Zip.Cons (deleteA len ca) (deleteB len cb))


instance (Storable.C a) => Default (SV.Vector a) where
   type Element (SV.Vector a) = MultiValue.T a
   deflt = storableVector

storableVector :: (Storable.C a) => T (MultiValue.T a) (SV.Vector a)
storableVector = Cons
   (\ _param -> MultiValue.liftM . Storable.storeNext)
   return
   (\len -> do
      vec <- SVB.create len (const $ return ())
      -- offset should be always zero, but we must not rely on that
      let (_fp,ptr,_l) = SVU.unsafeToPointers vec
      return (vec, ptr))
   (\len vec -> do
      let (fp,_s,_l) = SVB.toForeignPtr vec
      -- keep the foreign ptr alive
      FPtr.touchForeignPtr fp
      return $ SV.take len vec)
