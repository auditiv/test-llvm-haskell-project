{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Synthesizer.LLVM.Generator.Render (
   -- * type driven
   RunArg, DSLArg,
   run,
   runChunky,
   runChunkyOnVector,
   Render.Buffer, Render.buffer,

   -- * explicit argument converters
   runExplicit,
   build, -- ToDo: better name
   WithShape,

   -- * utilities
   Render.TimeInteger(Render.subdivideLong), -- ToDo: Is Render the right module to define this?
   ) where

import qualified Synthesizer.LLVM.Private.Render as Render
import qualified Synthesizer.LLVM.Causal.Parametric as Parametric
import qualified Synthesizer.LLVM.Generator.Source as Source
import Synthesizer.LLVM.Private.Render
         (RunArg (DSLArg, buildArg),
          Triple, tripleStruct, derefStartPtr, derefStopPtr)
import Synthesizer.LLVM.Generator.Private (T(Cons))

import qualified Synthesizer.LLVM.Storable.Vector as SVU
import qualified Synthesizer.LLVM.ForeignPtr as ForeignPtr

import qualified Synthesizer.Causal.Class as CausalClass

import qualified LLVM.DSL.Render.Run as Run
import qualified LLVM.DSL.Render.Argument as Arg
import qualified LLVM.DSL.Execution as Exec
import LLVM.DSL.Render.Run ((*->))
import LLVM.DSL.Expression (Exp(Exp))

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Maybe as Maybe
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector as SV

import Control.Monad (join)
import Control.Applicative (liftA3)

import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Ptr (Ptr)

import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Int (Int)
import Data.Word (Word)

import qualified System.Unsafe as Unsafe


foreign import ccall safe "dynamic" derefFillPtr ::
   Exec.Importer (LLVM.Ptr param -> Word -> Ptr struct -> IO Word)


compile ::
   (Storable.C a, MultiValue.T a ~ value,
    Marshal.C param, Marshal.Struct param ~ paramStruct) =>
   (Exp param -> T value) ->
   IO (LLVM.Ptr paramStruct -> Word -> Ptr a -> IO Word)
compile sig =
   Exec.compile "signal" $
   Exec.createFunction derefFillPtr "fill" $ \paramPtr size bPtr ->
   case sig (Exp (Memory.load paramPtr)) of
      Cons next start stop -> do
         (global,s) <- start
         local <- LLVM.alloca
         (pos,_) <- Storable.arrayLoopMaybeCont size bPtr s $ \ ptri s0 -> do
            (y,s1) <- next global local s0
            MaybeCont.lift $ Storable.store y ptri
            return s1
         stop global
         return pos

runAux ::
   (Marshal.C p, Storable.C a, MultiValue.T a ~ value) =>
   (Exp p -> T value) -> IO (IO () -> Int -> p -> IO (SV.Vector a))
runAux sig = do
   fill <- compile sig
   return $ \final len param ->
      Marshal.with param $ \paramPtr ->
      SVB.createAndTrim len $ \ptr -> do
         n <- fill paramPtr (fromIntegral len) ptr
         final
         return $ fromIntegral n

_run ::
   (Marshal.C p, Storable.C a, MultiValue.T a ~ value) =>
   (Exp p -> T value) -> IO (Int -> p -> IO (SV.Vector a))
_run = fmap ($ return ()) . runAux

foreign import ccall safe "dynamic" derefChunkPtr ::
   Exec.Importer (LLVM.Ptr globalState -> Word -> Ptr a -> IO Word)


compileChunky ::
   (LLVM.IsSized paramStruct, LLVM.Value (LLVM.Ptr paramStruct) ~ pPtr,
    Memory.C state, Memory.Struct state ~ stateStruct,
    Memory.C global, Memory.Struct global ~ globalStruct,
    Triple paramStruct globalStruct stateStruct ~ triple,
    LLVM.IsSized local,
    Storable.C a, MultiValue.T a ~ value) =>
   (forall r z. (Tuple.Phi z) =>
    pPtr -> global -> LLVM.Value (LLVM.Ptr local) ->
    () -> state -> MaybeCont.T r z (value, state)) ->
   (forall r. pPtr -> LLVM.CodeGenFunction r (global, state)) ->
   (forall r. pPtr -> global -> LLVM.CodeGenFunction r ()) ->
   IO (LLVM.Ptr paramStruct -> IO (LLVM.Ptr triple),
       Exec.Finalizer triple,
       LLVM.Ptr triple -> Word -> Ptr a -> IO Word)
compileChunky next start stop =
   Exec.compile "signal-chunky" $
   liftA3 (,,)
      (Exec.createFunction derefStartPtr "startsignal" $
         \paramPtr -> do
            paramGlobalStatePtr <- LLVM.malloc
            (global,state) <- start paramPtr
            flip LLVM.store paramGlobalStatePtr =<<
               join
                  (liftA3 tripleStruct
                     (LLVM.load paramPtr)
                     (Memory.compose global)
                     (Memory.compose state))
            return paramGlobalStatePtr)
      (Exec.createFinalizer derefStopPtr "stopsignal" $
         \paramGlobalStatePtr -> do
            paramPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d0, ())
            stop paramPtr =<<
               Memory.load =<<
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
            LLVM.free paramGlobalStatePtr)
      (Exec.createFunction derefChunkPtr "fillsignal" $
         \paramGlobalStatePtr loopLen ptr -> do
            paramPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d0, ())
            global <-
               Memory.load =<<
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
            statePtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d2, ())
            sInit <- Memory.load statePtr
            local <- LLVM.alloca
            (pos,sExit) <-
               Storable.arrayLoopMaybeCont loopLen ptr sInit $
                  \ ptri s0 -> do
               (y,s1) <- next paramPtr global local () s0
               MaybeCont.lift $ Storable.store y ptri
               return s1
            Memory.store (Maybe.fromJust sExit) statePtr
            return pos)


runChunkyAux ::
   (Storable.C a, MultiValue.T a ~ value, Marshal.C p) =>
   (Exp p -> T value) -> IO (IO () -> SVL.ChunkSize -> p -> IO (SVL.Vector a))
runChunkyAux sig = do
   paramd <-
      Parametric.fromProcessPtr "Signal.run" (CausalClass.fromSignal . sig)
   case paramd of
      Parametric.Cons next start stop -> do
         (startFunc,stopFunc,fill) <- compileChunky next start stop
         return $ \final (SVL.ChunkSize size) p -> do
            statePtr <- ForeignPtr.newParamMV stopFunc startFunc p

            let go =
                  Unsafe.interleaveIO $ do
                     v <-
                        ForeignPtr.with statePtr $ \sptr ->
                        SVB.createAndTrim size $
                        fmap (fromIntegral :: Word -> Int) .
                        fill sptr (fromIntegral size)
                     (if SV.length v > 0
                        then fmap (v:)
                        else id) $
                        (if SV.length v < size
                           then final >> return []
                           else go)
            fmap SVL.fromChunks go

runChunky ::
   (Storable.C a, MultiValue.T a ~ value, Marshal.C p) =>
   (Exp p -> T value) -> IO (SVL.ChunkSize -> p -> IO (SVL.Vector a))
runChunky = fmap ($ return ()) . runChunkyAux


runChunkyOnVector ::
   (Storable.C a, MultiValue.T a ~ al) =>
   (Storable.C b, MultiValue.T b ~ bl) =>
   (T al -> T bl) ->
   IO (SVL.ChunkSize -> SV.Vector a -> IO (SVL.Vector b))
runChunkyOnVector sig = do
   f <- runChunkyAux (sig . Source.storableVector)
   return $ \chunkSize av -> do
      let (fp,ptr,l) = SVU.unsafeToPointers av
      f (touchForeignPtr fp) chunkSize (Source.consStorableVector ptr l)



type WithShape shape = Compose IO ((->) shape)

class Run f where
   type DSL f
   type Shape f
   build :: (Marshal.C p) => Run.T (WithShape (Shape f)) p (DSL f) f

instance (Storable.C a) => Run (SVL.Vector a) where
   type DSL (SVL.Vector a) = T (MultiValue.T a)
   type Shape (SVL.Vector a) = SVL.ChunkSize
   build =
      Run.Cons $
      Compose .
      fmap (\f shape create -> Unsafe.performIO $ buildIOGen f shape create) .
      runChunkyAux

instance (Storable.C a) => Run (SV.Vector a) where
   type DSL (SV.Vector a) = T (MultiValue.T a)
   type Shape (SV.Vector a) = Int
   build =
      Run.Cons $
      Compose .
      fmap (\f shape create -> Unsafe.performIO $ buildIOGen f shape create) .
      runAux

instance (RunIO a) => Run (IO a) where
   type DSL (IO a) = T (DSL_IO a)
   type Shape (IO a) = ShapeIO a
   build = buildIO

instance (RunArg a, Run f) => Run (a -> f) where
   type DSL (a -> f) = DSLArg a -> DSL f
   type Shape (a -> f) = Shape f
   build = buildArg *-> build


class RunIO a where
   type DSL_IO a
   type ShapeIO a
   buildIO ::
      (Marshal.C p) => Run.T (WithShape (ShapeIO a)) p (T (DSL_IO a)) (IO a)

instance (Storable.C a) => RunIO (SVL.Vector a) where
   type DSL_IO (SVL.Vector a) = MultiValue.T a
   type ShapeIO (SVL.Vector a) = SVL.ChunkSize
   buildIO = Run.Cons $ Compose . fmap buildIOGen . runChunkyAux

instance (Storable.C a) => RunIO (SV.Vector a) where
   type DSL_IO (SV.Vector a) = MultiValue.T a
   type ShapeIO (SV.Vector a) = Int
   buildIO = Run.Cons $ Compose . fmap buildIOGen . runAux

buildIOGen ::
   (Monad m) => (final -> shape -> p -> m a) -> shape -> m (p, final) -> m a
buildIOGen f shape create = do (p,final) <- create; f final shape p



{-
do f <- run (\n -> takeWhile (<*n) (iterate (1+) 0) <> takeWhile (<*n) (iterate (2+) 0)); f SVL.defaultChunkSize (12::Float) :: IO (SVL.Vector Float)
do f <- Sig.run (\n -> Sig.takeWhile (Expr.<*n) (Sig.iterate (1+) 0) <> Sig.takeWhile (Expr.<*n) (Sig.iterate (2+) 0)); f SVL.defaultChunkSize (12::Float) :: IO (SVL.Vector Float)
-}
run :: (Run f) => DSL f -> IO (Shape f -> f)
run = runExplicit build

{-
ToDo:
Export it, but we also need to export 'build' then.
To this end we should define a nice data type for 'build'.
-}
runExplicit ::
   Run.T (WithShape (Shape f)) () fdsl f ->
   fdsl -> IO (Shape f -> f)
runExplicit builder sig =
   getCompose $ Run.run builder sig

_exampleExplicit ::
   (Exp Float -> Exp Word -> T (MultiValue.T Float)) ->
   IO (Int -> Float -> Word -> SV.Vector Float)
_exampleExplicit = runExplicit (Arg.primitive *-> Arg.primitive *-> build)
