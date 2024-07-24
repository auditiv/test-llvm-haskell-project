{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Synthesizer.LLVM.Causal.Render (
   -- * type driven
   RunArg, DSLArg,
   run,
   runPlugged,
   processIO,
   Render.Buffer, Render.buffer,

   -- * explicit argument converters
   runPluggedExplicit,
   build, -- ToDo: better name
   Plugs,

   -- * internally used in FunctionalPlug
   processIOParametric,
   ) where

import qualified Synthesizer.LLVM.Private.Render as Render
import qualified Synthesizer.LLVM.Causal.Parametric as Parametric
import Synthesizer.LLVM.Causal.Private (T(Cons))
import Synthesizer.LLVM.Private.Render
         (RunArg (DSLArg, buildArg),
          Triple, tripleStruct, derefStartPtr, derefStopPtr)

import qualified Synthesizer.LLVM.Plug.Input as PIn
import qualified Synthesizer.LLVM.Plug.Output as POut

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Generic.Cut as Cut

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

import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector as SV

import qualified Control.Monad.Trans.Reader as MR
import Control.Monad (when, join)
import Control.Applicative (liftA3)

import Foreign.Ptr (Ptr)

import Data.Tuple.HT (snd3)
import Data.Word (Word)



foreign import ccall safe "dynamic" derefFillPtr ::
   Exec.Importer (LLVM.Ptr global -> Word -> Ptr a -> Ptr b -> IO Word)


compile ::
   (Storable.C a, MultiValue.T a ~ al,
    Storable.C b, MultiValue.T b ~ bl,
    Marshal.C param, Marshal.Struct param ~ paramStruct) =>
   (Exp param -> T al bl) ->
   IO (LLVM.Ptr paramStruct -> Word -> Ptr a -> Ptr b -> IO Word)
compile proc =
   Exec.compile "process" $
   Exec.createFunction derefFillPtr "fill" $ \paramPtr size aPtr bPtr ->
   case proc (Exp (Memory.load paramPtr)) of
      Cons next start stop -> do
         (global,s) <- start
         local <- LLVM.alloca
         (pos,_) <- Storable.arrayLoopMaybeCont2 size aPtr bPtr s $
               \aPtri bPtri s0 -> do
            a <- MaybeCont.lift $ Storable.load aPtri
            (b,s1) <- next global local a s0
            MaybeCont.lift $ Storable.store b bPtri
            return s1
         stop global
         return pos

runAux ::
   (Marshal.C p,
    Storable.C a, MultiValue.T a ~ al,
    Storable.C b, MultiValue.T b ~ bl) =>
   (Exp p -> T al bl) ->
   IO (IO () -> p -> SV.Vector a -> IO (SV.Vector b))
runAux proc = do
   fill <- compile proc
   return $ \final param as ->
      Marshal.with param $ \paramPtr ->
      SVB.withStartPtr as $ \ aPtr len ->
      SVB.createAndTrim len $ \bPtr -> do
         n <- fill paramPtr (fromIntegral len) aPtr bPtr
         final
         return $ fromIntegral n

_run ::
   (Marshal.C p,
    Storable.C a, MultiValue.T a ~ al,
    Storable.C b, MultiValue.T b ~ bl) =>
   (Exp p -> T al bl) -> IO (p -> SV.Vector a -> IO (SV.Vector b))
_run = fmap ($ return ()) . runAux



foreign import ccall safe "dynamic" derefChunkPtr ::
   Exec.Importer (LLVM.Ptr globalState -> Word -> Ptr a -> Ptr b -> IO Word)

_compileChunky ::
   (LLVM.IsSized paramStruct, LLVM.Value (LLVM.Ptr paramStruct) ~ pPtr,
    Memory.C state, Memory.Struct state ~ stateStruct,
    Memory.C global, Memory.Struct global ~ globalStruct,
    Triple paramStruct globalStruct stateStruct ~ triple,
    LLVM.IsSized local,
    Storable.C a, MultiValue.T a ~ valueA,
    Storable.C b, MultiValue.T b ~ valueB) =>
   (forall r z. (Tuple.Phi z) =>
    pPtr ->
    global -> LLVM.Value (LLVM.Ptr local) ->
    valueA -> state -> MaybeCont.T r z (valueB, state)) ->
   (forall r. pPtr -> LLVM.CodeGenFunction r (global, state)) ->
   (forall r. pPtr -> global -> LLVM.CodeGenFunction r ()) ->
   IO (LLVM.Ptr paramStruct -> IO (LLVM.Ptr triple),
       Exec.Finalizer triple,
       LLVM.Ptr triple -> Word -> Ptr a -> Ptr b -> IO Word)
_compileChunky next start stop =
   Exec.compile "process-chunky" $
   liftA3 (,,)
      (Exec.createFunction derefStartPtr "startprocess" $
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
      (Exec.createFinalizer derefStopPtr "stopprocess" $
         \paramGlobalStatePtr -> do
            paramPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d0, ())
            stop paramPtr =<<
               Memory.load =<<
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
            LLVM.free paramGlobalStatePtr)
      (Exec.createFunction derefChunkPtr "fillprocess" $
         \paramGlobalStatePtr loopLen aPtr bPtr -> do
            paramPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d0, ())
            globalPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
            statePtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d2, ())
            global <- Memory.load globalPtr
            sInit <- Memory.load statePtr
            local <- LLVM.alloca
            (pos,sExit) <-
               Storable.arrayLoopMaybeCont2 loopLen aPtr bPtr sInit $
                  \ aPtri bPtri s0 -> do
               a <- MaybeCont.lift $ Storable.load aPtri
               (b,s1) <- next paramPtr global local a s0
               MaybeCont.lift $ Storable.store b bPtri
               return s1
            Memory.store (Maybe.fromJust sExit) statePtr
            return pos)


foreign import ccall safe "dynamic" derefChunkPluggedPtr ::
   Exec.Importer
      (LLVM.Ptr globalStateStruct -> Word ->
       LLVM.Ptr inp -> LLVM.Ptr out -> IO Word)

compilePlugged ::
   (Tuple.Undefined stateIn, Tuple.Phi stateIn) =>
   (Tuple.Undefined stateOut, Tuple.Phi stateOut) =>
   (LLVM.IsSized paramStruct, LLVM.Value (LLVM.Ptr paramStruct) ~ pPtr,
    Memory.C state, Memory.Struct state ~ stateStruct,
    Memory.C global, Memory.Struct global ~ globalStruct,
    Triple paramStruct globalStruct stateStruct ~ triple) =>
   (LLVM.IsSized local) =>
   (Memory.C paramIn, Memory.Struct paramIn ~ inStruct) =>
   (Memory.C paramOut, Memory.Struct paramOut ~ outStruct) =>
   (forall r.
    paramIn -> stateIn -> LLVM.CodeGenFunction r (valueA, stateIn)) ->
   (forall r.
    paramIn -> LLVM.CodeGenFunction r stateIn) ->
   (forall r z. (Tuple.Phi z) =>
    pPtr -> global -> LLVM.Value (LLVM.Ptr local) ->
    valueA -> state -> MaybeCont.T r z (valueB, state)) ->
   (forall r. pPtr -> LLVM.CodeGenFunction r (global, state)) ->
   (forall r. pPtr -> global -> LLVM.CodeGenFunction r ()) ->
   (forall r.
    paramOut -> valueB -> stateOut -> LLVM.CodeGenFunction r stateOut) ->
   (forall r.
    paramOut -> LLVM.CodeGenFunction r stateOut) ->
   IO (LLVM.Ptr paramStruct -> IO (LLVM.Ptr triple),
       LLVM.Ptr triple -> IO (),
       LLVM.Ptr triple ->
         Word -> LLVM.Ptr inStruct -> LLVM.Ptr outStruct -> IO Word)
compilePlugged nextIn startIn next start stop nextOut startOut =
   Exec.compile "process-plugged" $
   liftA3 (,,)
      (Exec.createFunction derefStartPtr "startprocess" $
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
      (Exec.createFunction derefStopPtr "stopprocess" $
         \paramGlobalStatePtr -> do
            paramPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d0, ())
            stop paramPtr =<<
               Memory.load =<<
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
            LLVM.free paramGlobalStatePtr)
      (Exec.createFunction derefChunkPluggedPtr "fillprocess" $
         \paramGlobalStatePtr loopLen inPtr outPtr -> do
            paramPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d0, ())
            globalPtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d1, ())
            statePtr <-
               LLVM.getElementPtr0 paramGlobalStatePtr (TypeNum.d2, ())
            global <- Memory.load globalPtr
            sInit <- Memory.load statePtr
            inParam  <- Memory.load inPtr
            outParam <- Memory.load outPtr
            inInit  <- startIn  inParam
            outInit <- startOut outParam
            local <- LLVM.alloca
            (pos,sExit) <-
               MaybeCont.fixedLengthLoop loopLen (inInit, sInit, outInit) $
                  \ (in0,s0,out0) -> do
               (a,in1) <- MaybeCont.lift $ nextIn inParam in0
               (b,s1) <- next paramPtr global local a s0
               out1 <- MaybeCont.lift $ nextOut outParam b out0
               return (in1, s1, out1)
            Memory.store (snd3 $ Maybe.fromJust sExit) statePtr
            return pos)


{-
I liked to write something with signature

> import qualified Synthesizer.Causal.Process as Causal
>
> liftStorableChunk ::
>    (Exp param -> T valueA valueB) ->
>    IO (param -> Causal.T (SV.Vector a) (SV.Vector b))

but it does not quite work this way.
@Causal.T@ from @synthesizer-core@ uses an immutable state internally,
whereas @T@ uses mutable states.
In principle the immutable state of @Causal.T@
could be used for breaking the processing of a stream
and continue it on two different streams in parallel.
I have no function that makes use of this feature,
and thus an @ST@ monad might be a way out.

With this function we can convert an LLVM causal process to a causal IO arrow.
We also need the plugs in order
to read and write LLVM values from and to Haskell data chunks.

In a second step we could convert this to a processor of lazy lists,
and thus to a processor of chunky storable vectors.
-}
processIOParametric ::
   (Marshal.C p, Cut.Read a, x ~ LLVM.Value (LLVM.Ptr (Marshal.Struct p))) =>
   PIn.T a b -> Parametric.T x b c -> POut.T c d ->
   IO (Arg.Creator p -> PIO.T a d)
processIOParametric
      (PIn.Cons nextIn startIn createIn deleteIn)
      paramd
      (POut.Cons nextOut startOut createOut deleteOut) = do
   case paramd of
      Parametric.Cons next start stop -> do
         (startFunc, stopFunc, fill) <-
            compilePlugged
               nextIn startIn
               next start stop
               nextOut startOut
         return $ \createContext -> PIO.Cons
            (\a s@(_,statePtr) -> do
               let maximumSize = Cut.length a
               (contextIn, paramIn)  <- createIn a
               (contextOut,paramOut) <- createOut maximumSize
               actualSize <-
                  Marshal.with paramIn $ \inptr ->
                  Marshal.with paramOut $ \outptr ->
                  fill statePtr (fromIntegral maximumSize) inptr outptr
               -- print actualSize
               when (fromIntegral actualSize > maximumSize) $
                  error $ "CausalParametrized.Process: " ++
                          "output size " ++ show actualSize ++
                          " > input size " ++ show maximumSize
               deleteIn contextIn
               b <- deleteOut (fromIntegral actualSize) contextOut
               return (b, s))
            (do
               (p, deleteContext) <- createContext
               ptr <- Marshal.with p startFunc
               return (deleteContext, ptr))
            (\(deleteContext, ptr) -> stopFunc ptr >> deleteContext)

processIOCore ::
   (Marshal.C p, Cut.Read a) =>
   PIn.T a b -> (Exp p -> T b c) -> POut.T c d ->
   IO (Arg.Creator p -> PIO.T a d)
processIOCore pin proc pout = do
   paramd <- Parametric.fromProcessPtr "Causal.process" proc
   processIOParametric pin paramd pout

processIO ::
   (Marshal.C p, Cut.Read a, PIn.Default a, POut.Default d) =>
   (Exp p -> T (PIn.Element a) (POut.Element d)) ->
   IO (p -> PIO.T a d)
processIO proc =
   fmap (\f p -> f (return (p, return ()))) $
   processIOCore PIn.deflt proc POut.deflt


type Plugs f a b = MR.ReaderT (PIn.T (In f) a, POut.T b (Out f)) IO

class Run f where
   type DSL f a b
   type In f
   type Out f
   build :: (Marshal.C p) => Run.T (Plugs f a b) p (DSL f a b) f

instance (Cut.Read a) => Run (PIO.T a b) where
   type DSL (PIO.T a b) al bl = T al bl
   type In (PIO.T a b) = a
   type Out (PIO.T a b) = b
   build =
      Run.Cons $ \proc ->
         MR.ReaderT $ \(pin,pout) -> processIOCore pin proc pout

instance (RunArg a, Run f) => Run (a -> f) where
   type DSL (a -> f) al bl = DSLArg a -> DSL f al bl
   type In (a -> f) = In f
   type Out (a -> f) = Out f
   build = buildArg *-> build


runPluggedExplicit ::
   Run.T (Plugs f a b) () (DSL f a b) f ->
   PIn.T (In f) a -> DSL f a b -> POut.T b (Out f) -> IO f
runPluggedExplicit builder pin proc pout =
   MR.runReaderT (Run.run builder proc) (pin,pout)

runPlugged ::
   (Run f) => PIn.T (In f) a -> DSL f a b -> POut.T b (Out f) -> IO f
runPlugged = runPluggedExplicit build

run ::
   (Run f) =>
   (In f ~ a, PIn.Default a, PIn.Element a ~ al) =>
   (Out f ~ b, POut.Default b, POut.Element b ~ bl) =>
   DSL f al bl -> IO f
run proc = runPlugged PIn.deflt proc POut.deflt

_exampleExplicit ::
   (Exp Float -> Exp Word -> T (MultiValue.T Float) (MultiValue.T Word)) ->
   IO (Float -> Word -> PIO.T (SV.Vector Float) (SV.Vector Word))
_exampleExplicit proc =
   runPluggedExplicit
      (Arg.primitive *-> Arg.primitive *-> build)
      PIn.storableVector proc POut.storableVector
