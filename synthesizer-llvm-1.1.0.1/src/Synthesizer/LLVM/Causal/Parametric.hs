{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Causal.Parametric where

import qualified Synthesizer.LLVM.Causal.Private as Causal

import LLVM.DSL.Expression (Exp(Exp))

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Tuple as Tuple

import qualified LLVM.Core as LLVM

import Control.Monad.IO.Class (liftIO)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)


data T p a b =
   forall global local state.
      (Memory.C global, LLVM.IsSized local, Memory.C state) =>
      Cons (forall r c.
            (Tuple.Phi c) =>
            p -> global -> LLVM.Value (LLVM.Ptr local) ->
            a -> state -> MaybeCont.T r c (b, state))
           (forall r. p -> LLVM.CodeGenFunction r (global, state))
           (forall r. p -> global -> LLVM.CodeGenFunction r ())


fromProcess :: String -> (Exp p -> Causal.T a b) -> IO (T (MultiValue.T p) a b)
fromProcess name f = do
   ref <- newIORef $ error $ name ++ ": uninitialized parameter reference"
   return $
      case f (Exp (liftIO (readIORef ref))) of
         Causal.Cons next start stop ->
            Cons
               (\p global local a state ->
                  liftIO (writeIORef ref p) >> next global local a state)
               (\p -> liftIO (writeIORef ref p) >> start)
               (\p global -> liftIO (writeIORef ref p) >> stop global)


type MarshalPtr a = LLVM.Ptr (Marshal.Struct a)

fromProcessPtr ::
   (Marshal.C p) =>
   String -> (Exp p -> Causal.T a b) ->
   IO (T (LLVM.Value (MarshalPtr p)) a b)
fromProcessPtr name f = do
   ref <- newIORef $ error $ name ++ ": uninitialized parameter reference"
   return $
      case f (Exp (liftIO (readIORef ref))) of
         Causal.Cons next start stop ->
            Cons
               (\p global local a state ->
                  MaybeCont.lift (loadParam ref p) >> next global local a state)
               (\p -> loadParam ref p >> start)
               (\p global -> loadParam ref p >> stop global)

loadParam ::
   (Marshal.C param) =>
   IORef (MultiValue.T param) ->
   LLVM.Value (MarshalPtr param) ->
   LLVM.CodeGenFunction r ()
loadParam ref ptr = liftIO . writeIORef ref =<< Memory.load ptr
