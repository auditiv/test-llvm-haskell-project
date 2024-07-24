{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.LLVM.Private.Render where

import qualified Synthesizer.LLVM.Generator.Source as Source
import qualified Synthesizer.LLVM.Storable.ChunkIterator as ChunkIt
import qualified Synthesizer.LLVM.Storable.LazySizeIterator as SizeIt
import qualified Synthesizer.LLVM.EventIterator as EventIt
import Synthesizer.LLVM.Generator.Private (T(Cons))

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Storable.Vector as SVU
import qualified Synthesizer.LLVM.ConstantPiece as Const

import qualified Synthesizer.PiecewiseConstant.Signal as PC

import qualified LLVM.DSL.Render.Argument as Arg
import qualified LLVM.DSL.Execution as Exec
import LLVM.DSL.Expression (Exp(Exp))

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Maybe as Maybe
import qualified LLVM.Extra.Control as C
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import qualified Data.EventList.Relative.BodyTime as EventList
import qualified Numeric.NonNegative.Wrapper as NonNeg
import qualified Numeric.NonNegative.Chunky as NonNegChunky

import Control.Monad (join)
import Control.Applicative (liftA3)

import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Ptr (Ptr)

import Data.Foldable (traverse_)
import Data.Int (Int)
import Data.Word (Word, Word8, Word32)



foreign import ccall safe "dynamic" derefStartPtr ::
   Exec.Importer (LLVM.Ptr param -> IO (LLVM.Ptr globalState))

foreign import ccall safe "dynamic" derefStopPtr ::
   Exec.Importer (LLVM.Ptr globalState -> IO ())



type Pair a b = LLVM.Struct (a,(b,()))
type Triple a b c = LLVM.Struct (a,(b,(c,())))

tripleStruct ::
   (LLVM.IsSized a, LLVM.IsSized b, LLVM.IsSized c) =>
   LLVM.Value a -> LLVM.Value b -> LLVM.Value c ->
   LLVM.CodeGenFunction r (LLVM.Value (Triple a b c))
tripleStruct a b c = do
   s0 <- LLVM.insertvalue Tuple.undef a TypeNum.d0
   s1 <- LLVM.insertvalue s0 b TypeNum.d1
   LLVM.insertvalue s1 c TypeNum.d2


type WithGlobalState param = LLVM.Struct (param, ())

{- |
This is a pretty ugly hack, but its seems to be the least ugly one.
We need to solve the following problem:
We have a function of type @Exp param -> T value@.
This means that all methods in @T value@ depend on @Exp param@.
We need to choose one piece of LLVM code in @Exp param@
that generates appropriate code for all methods in @T value@.
If we access a function parameter via @Memory.load paramPtr@
this means that all methods must end up in the same LLVM function
in order to access this parameter.
Thus I have to put all functionality in one LLVM function
and then the three functions in 'compileChunky'
jump into the handler function with a 'Word8' code
specifying the actual sub-routine.
We need to squeeze all possible inputs and outputs
through one function interface.

However, since the handler is marked as internal
the optimizer inlines it in the three functions from 'compileChunky'
and eliminates dead code.
This way, we end up with the code that we would have written otherwise.

The alternative would be to construct @T value@ multiple times.
Due to existential quantification we cannot prove
that the pointer types of different methods match,
so we need to cast pointers.
However, with the current approach we also have to do that.
-}
compileHandler ::
   (Marshal.C param, Marshal.Struct param ~ paramStruct,
    Storable.C a, MultiValue.T a ~ value) =>
   (Exp param -> T value) ->
   LLVM.CodeGenModule
      (LLVM.Function
         (Word8 -> LLVM.Ptr paramStruct -> Word -> Ptr a ->
          IO (Pair (LLVM.Ptr (WithGlobalState paramStruct)) Word)))
compileHandler sig =
   LLVM.createNamedFunction LLVM.InternalLinkage "handlesignal" $
   \phase paramPtr loopLen bufferPtr ->
   case sig $ Exp (Memory.load paramPtr) of
      Cons next start stop -> do
         paramGlobalStatePtr <- LLVM.bitcast paramPtr

         let create = do
               newParamGlobalStatePtr <- LLVM.malloc
               (global,state) <- start
               flip LLVM.store newParamGlobalStatePtr =<<
                  join
                     (liftA3 tripleStruct
                        (LLVM.load paramPtr)
                        (Memory.compose global)
                        (Memory.compose state))
               newOpaqueParamGlobalStatePtr <-
                  LLVM.bitcast
                     (newParamGlobalStatePtr `asTypeOf` paramGlobalStatePtr)
               LLVM.insertvalue Tuple.undef
                  newOpaqueParamGlobalStatePtr TypeNum.d0

         let delete = do
               globalPtr <-
                  LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
               stop =<< Memory.load globalPtr
               LLVM.free paramGlobalStatePtr
               return Tuple.undef

         let fill = do
               globalPtr <-
                  LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
               statePtr <-
                  LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d2, ())
               global <- Memory.load globalPtr
               sInit <- Memory.load statePtr
               local <- LLVM.alloca
               (pos,sExit) <-
                  Storable.arrayLoopMaybeCont loopLen bufferPtr sInit $
                     \ ptr s0 -> do
                  (y,s1) <- next global local s0
                  MaybeCont.lift $ Storable.store y ptr
                  return s1
               Memory.store (Maybe.fromJust sExit) statePtr
               LLVM.insertvalue Tuple.undef pos TypeNum.d1

         doCreate <- A.cmp LLVM.CmpEQ (LLVM.valueOf 0) phase
         doDelete <- A.cmp LLVM.CmpEQ (LLVM.valueOf 1) phase
         C.ret =<<
            (C.ifThenElse doCreate create $
             C.ifThenElse doDelete delete fill)


class RunArg a where
   type DSLArg a
   buildArg :: Arg.T a (DSLArg a)

instance RunArg () where
   type DSLArg () = ()
   buildArg = Arg.unit

instance (RunArg a, RunArg b) => RunArg (a,b) where
   type DSLArg (a,b) = (DSLArg a, DSLArg b)
   buildArg = Arg.pair buildArg buildArg

instance (RunArg a, RunArg b, RunArg c) => RunArg (a,b,c) where
   type DSLArg (a,b,c) = (DSLArg a, DSLArg b, DSLArg c)
   buildArg = Arg.triple buildArg buildArg buildArg

instance RunArg Float where
   type DSLArg Float = Exp Float
   buildArg = Arg.primitive

instance RunArg Int where
   type DSLArg Int = Exp Int
   buildArg = Arg.primitive

instance RunArg Word where
   type DSLArg Word = Exp Word
   buildArg = Arg.primitive

instance RunArg Word32 where
   type DSLArg Word32 = Exp Word32
   buildArg = Arg.primitive

instance (RunArg a) => RunArg (Stereo.T a) where
   type DSLArg (Stereo.T a) = Stereo.T (DSLArg a)
   buildArg =
      case buildArg of
         Arg.Cons pass create ->
            Arg.Cons
               (fmap pass . Stereo.unExpression)
               (\s -> do
                  pf <- traverse create s
                  return (fst<$>pf, traverse_ snd pf))

instance
   (TypeNum.Natural n, Marshal.C a, LLVM.IsSized (Marshal.Struct a),
    TypeNum.Natural (n TypeNum.:*: LLVM.SizeOf (Marshal.Struct a))) =>
      RunArg (MultiValue.Array n a) where
   type DSLArg (MultiValue.Array n a) = Exp (MultiValue.Array n a)
   buildArg = Arg.primitive

instance (Storable.C a) => RunArg (SV.Vector a) where
   type DSLArg (SV.Vector a) = T (MultiValue.T a)
   buildArg =
      Arg.Cons
         Source.storableVector
         (\av -> do
            let (fp,ptr,l) = SVU.unsafeToPointers av
            return (Source.consStorableVector ptr l, touchForeignPtr fp))

newtype Buffer a = Buffer (SV.Vector a)

buffer :: SV.Vector a -> Buffer a
buffer = Buffer

instance (Storable.C a) => RunArg (Buffer a) where
   type DSLArg (Buffer a) = Exp (Source.StorableVector a)
   buildArg =
      Arg.Cons id
         (\(Buffer av) -> do
            let (fp,ptr,l) = SVU.unsafeToPointers av
            return (Source.consStorableVector ptr l, touchForeignPtr fp))

instance (Storable.C a) => RunArg (SVL.Vector a) where
   type DSLArg (SVL.Vector a) = T (MultiValue.T a)
   buildArg =
      Arg.newDispose ChunkIt.new ChunkIt.dispose Source.storableVectorLazy

class TimeInteger int where
   subdivideLong :: EventList.T (NonNeg.T int) a -> EventList.T NonNeg.Int a

instance TimeInteger Int where
   subdivideLong = id

instance TimeInteger Integer where
   subdivideLong = PC.subdivideLongStrict

instance
   (time ~ NonNeg.T int, TimeInteger int, Marshal.C a) =>
      RunArg (EventList.T time a) where
   type DSLArg (EventList.T time a) = T (Const.T (MultiValue.T a))
   buildArg =
      Arg.newDispose
         (EventIt.new . subdivideLong) EventIt.dispose Source.eventList

instance (a ~ SVL.ChunkSize) => RunArg (NonNegChunky.T a) where
   type DSLArg (NonNegChunky.T a) = T (Const.T ())
   buildArg =
      Arg.newDispose SizeIt.new SizeIt.dispose Source.lazySize
