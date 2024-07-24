{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Functions on lazy storable vectors that are implemented using LLVM.
-}
module Synthesizer.LLVM.Storable.Process (
   makeArranger,
   continuePacked,
   ) where

import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Generic.Cut as CutG

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Absolute.TimeBody  as AbsEventList

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Arithmetic as A

import qualified Type.Data.Num.Decimal as TypeNum

import qualified Control.Arrow as Arr
import qualified Data.Foldable as Fold
import Foreign.Marshal.Array (advancePtr)

import qualified System.Unsafe as Unsafe

import qualified Number.NonNegative as NonNeg

import NumericPrelude.Numeric
import NumericPrelude.Base



{-
Same algorithm as in Synthesizer.Storable.Cut.arrangeEquidist
-}
{- |
The element vectors in the event lists
must fit into the length of the event list.
-}
makeArranger ::
   (Arr.Arrow arrow, Storable.C a, MultiValue.Additive a) =>
   IO (arrow
          (EventListTT.T NonNeg.Int (SV.Vector a))
          (SV.Vector a))
makeArranger = do
   mixer <- SigStL.makeMixer A.add
   fill <- SigStL.fillBuffer A.zero
   return $ Arr.arr $ \ now ->
      let -- summation is done twice, for 'sz' and for 'xs'
          sznn = EventListTT.duration now
          sz = NonNeg.toNumber sznn
          xs =
             AbsEventList.toPairList $
             AbsEventList.mapTime NonNeg.toNumber $
             EventList.toAbsoluteEventList 0 $
             EventListTM.switchTimeR const now
      in  Unsafe.performIO $
          SVB.createAndTrim sz $ \dstPtr -> do
             fill (fromIntegral sz) dstPtr
             Fold.forM_ xs $ \(i,s) ->
                SVB.withStartPtr s $ \srcPtr len ->
                let llen =
                       if len <= sz-i
                         then fromIntegral len
                         else error "Process.arrange: chunk larger that event list"
                in  mixer llen srcPtr (advancePtr dstPtr i)
             return sz


continuePacked ::
   (CutG.Transform a, Storable.Vector b, TypeNum.Positive n) =>
   PIO.T a (SV.Vector (Serial.T n b)) ->
   (b -> PIO.T a (SV.Vector (Serial.T n b))) ->
   PIO.T a (SV.Vector (Serial.T n b))
continuePacked proc0 proc1 =
   PIO.continueChunk proc0
      (proc1 Arr.<<^ SV.last . SigStL.unpackStrict)
