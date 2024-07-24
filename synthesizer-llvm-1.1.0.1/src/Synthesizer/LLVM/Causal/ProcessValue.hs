{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Causal.ProcessValue (
   Causal.T,
   mapAccum,
   fromModifier,
   ) where

import qualified Synthesizer.LLVM.Causal.Private as Causal

import qualified Synthesizer.LLVM.Value as Value

import qualified Synthesizer.Plain.Modifier as Modifier

import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Memory as Memory

import qualified LLVM.Core as LLVM

import Control.Monad.Trans.State (runState)



mapAccum ::
   (Memory.C state) =>
   (forall r. a -> state -> LLVM.CodeGenFunction r (b, state)) ->
   (forall r. LLVM.CodeGenFunction r state) ->
   Causal.T a b
mapAccum next = Causal.simple (\a s -> MaybeCont.lift $ next a s)

fromModifier ::
   (Value.Flatten ah, Value.Registers ah ~ al,
    Value.Flatten bh, Value.Registers bh ~ bl,
    Value.Flatten ch, Value.Registers ch ~ cl,
    Value.Flatten sh, Value.Registers sh ~ sl,
    Memory.C sl) =>
   Modifier.Simple sh ch ah bh -> Causal.T (cl,al) bl
fromModifier (Modifier.Simple initial step) =
   mapAccum
      (\(c,a) s ->
         Value.flatten $
         runState
            (step (Value.unfold c) (Value.unfold a))
            (Value.unfold s))
      (Value.flatten initial)
