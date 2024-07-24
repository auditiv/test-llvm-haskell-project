{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RebindableSyntax #-}
{- |
<http://arxiv.org/abs/0911.5171>
-}
module Synthesizer.LLVM.Causal.Helix (
   -- * time and phase control based on the helix model
   static,
   staticPacked,
   dynamic,
   dynamicLimited,

   -- * useful control curves
   zigZag,
   zigZagPacked,
   zigZagLong,
   zigZagLongPacked,
   ) where

import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Generator.Source as Source
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Private as SigPriv
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Causal.RingBufferForward as RingBuffer
import qualified Synthesizer.LLVM.Frame.SerialVector as SerialExp
import qualified Synthesizer.LLVM.Frame.SerialVector.Code as Serial
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as SerialClass
import qualified Synthesizer.LLVM.Interpolation as Ip
import Synthesizer.LLVM.Causal.Functional (($&), (&|&))
import Synthesizer.LLVM.Private (noLocalPtr)

import Synthesizer.Causal.Class (($*), ($<))

import qualified LLVM.DSL.Expression.Vector as ExprVec
import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp, (<*), (>=*))

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value.Vector as MultiValueVec
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Memory as Memory

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum

import Data.Word (Word)

import Control.Arrow (first, (<<<))
import Control.Category (id)
import Control.Functor.HT (unzip)
import Data.Traversable (mapM)
import Data.Tuple.HT (mapPair, mapFst)

import qualified Algebra.Ring as Ring

import NumericPrelude.Numeric hiding (splitFraction)
import NumericPrelude.Base hiding (unzip, zip, mapM, id)

import Prelude ()


{- |
Inputs are @(shape, phase)@.

The shape parameter is limited at the beginning and at the end
such that only available data is used for interpolation.
Actually, we allow almost one step less than possible,
since the right boundary of the interval of admissible @shape@ values is open.
-}
static ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Storable.C vh, MultiValue.T vh ~ v) =>
   (Marshal.C a, MultiValue.Field a, MultiValue.RationalConstant a) =>
   (MultiValue.Fraction a, MultiValue.NativeFloating a ar) =>
   (MultiValueVec.NativeFloating a ar, MultiValue.T a ~ am) =>
   (forall r. Ip.T r nodesLeap am v) ->
   (forall r. Ip.T r nodesStep am v) ->
   Exp Int ->
   Exp a ->
   Exp (Source.StorableVector vh) ->
   Causal.T (am, am) v
static ipLeap ipStep periodInt period vec =
   let periodWord = wordFromInt periodInt
       cellMargin = combineMarginParams ipLeap ipStep periodInt
   in  interpolateCell ipLeap ipStep
       <<<
       first (peekCell cellMargin periodWord vec)
       <<<
       flattenShapePhaseProc periodWord period
       <<<
       first
          (limitShape cellMargin periodInt
              (intFromWord $ Source.storableVectorLength vec))

intFromWord :: Exp Word -> Exp Int
intFromWord = Expr.liftReprM LLVM.bitcast

wordFromInt :: Exp Int -> Exp Word
wordFromInt = Expr.liftReprM LLVM.bitcast

staticPacked ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Storable.C vh, MultiValue.T vh ~ ve, SerialClass.Element v ~ ve) =>
   (SerialClass.Size (nodesLeap (nodesStep v)) ~ n,
    SerialClass.Write (nodesLeap (nodesStep v)),
    SerialClass.Element (nodesLeap (nodesStep v)) ~
       nodesLeap (nodesStep (SerialClass.Element v))) =>
   (TypeNum.Positive n) =>
   (Marshal.C a, MultiVector.Field a, MultiVector.Real a,
    MultiVector.Fraction a, MultiVector.RationalConstant a,
    MultiVector.NativeFloating n a ar) =>
   (forall r. Ip.T r nodesLeap (Serial.Value n a) v) ->
   (forall r. Ip.T r nodesStep (Serial.Value n a) v) ->
   Exp Int ->
   Exp a ->
   Exp (Source.StorableVector vh) ->
   Causal.T (Serial.Value n a, Serial.Value n a) v
staticPacked ipLeap ipStep periodInt period vec =
   let periodWord = wordFromInt periodInt
       cellMargin = combineMarginParams ipLeap ipStep periodInt
   in  interpolateCell ipLeap ipStep
       <<<
       first (CausalPS.pack
          (peekCell (elementMargin cellMargin) periodWord vec))
       <<<
       flattenShapePhaseProcPacked periodWord period
       <<<
       first
          (limitShapePacked cellMargin periodInt
              (intFromWord $ Source.storableVectorLength vec))


{- |
In contrast to 'dynamic' this one ends
when the end of the manipulated signal is reached.
-}
dynamicLimited ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Marshal.C a, MultiValue.Field a, MultiValue.Fraction a,
    MultiValue.Select a, MultiValue.Comparison a,
    MultiValue.NativeFloating a ar,
    MultiValue.RationalConstant a,
    MultiValueVec.NativeFloating a ar) =>
   (MultiValue.T a ~ am) =>
   (Memory.C v) =>
   (forall r. Ip.T r nodesLeap am v) ->
   (forall r. Ip.T r nodesStep am v) ->
   Exp Int ->
   Exp a ->
   Sig.T v ->
   Causal.T (am, am) v
dynamicLimited ipLeap ipStep periodInt period sig =
   dynamicGen
      (\cellMargin (skips, fracs) ->
         let windows =
               (RingBuffer.trackSkip
                     (wordFromInt $ Ip.marginNumberExp cellMargin) sig)
                  $& skips
         in  (windows,
              Causal.delay1 zero $& skips,
              Causal.delay1 zero $& fracs))
      ipLeap ipStep periodInt period

{- |
If the time control exceeds the end of the input signal,
then the last waveform is locked.
This is analogous to 'static'.
-}
dynamic ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Marshal.C a, MultiValue.Field a, MultiValue.Fraction a,
    MultiValue.Select a, MultiValue.Comparison a,
    MultiValue.NativeFloating a ar,
    MultiValue.RationalConstant a,
    MultiValueVec.NativeFloating a ar) =>
   (MultiValue.T a ~ am) =>
   (Memory.C v) =>
   (forall r. Ip.T r nodesLeap am v) ->
   (forall r. Ip.T r nodesStep am v) ->
   Exp Int ->
   Exp a ->
   Sig.T v ->
   Causal.T (am, am) v
dynamic ipLeap ipStep periodInt period sig =
   dynamicGen
      (\cellMargin (skips, fracs) ->
         let {-
             For conformance with 'static'
             we stop one step before the definite end.
             We achieve this by using a buffer
             that is one step longer than necessary.
             -}
             ((running, actualSkips), windows) =
                mapFst unzip $ unzip $
                (RingBuffer.trackSkipHold
                   (wordFromInt (Ip.marginNumberExp cellMargin) + 1) sig)
                   $& skips
             holdFracs =
                Causal.zipWith (\r fr -> Expr.select r fr 1)
                $&
                running &|& (Causal.delay1 zero $& fracs)
         in  (windows, actualSkips, holdFracs))
      ipLeap ipStep periodInt period

dynamicGen ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Marshal.C a, MultiValue.Field a, MultiValue.Fraction a,
    MultiValue.Select a, MultiValue.Comparison a,
    MultiValue.NativeFloating a ar,
    MultiValue.RationalConstant a,
    MultiValueVec.NativeFloating a ar) =>
   (MultiValue.T a ~ am) =>
   (Memory.C v) =>
   (Exp (Ip.Margin (nodesLeap (nodesStep v))) ->
    (Func.T (am, am) (MultiValue.T Word),
     Func.T (am, am) am) ->
    (Func.T (am, am) (RingBuffer.T v),
     Func.T (am, am) (MultiValue.T Word),
     Func.T (am, am) am)) ->
   (forall r. Ip.T r nodesLeap am v) ->
   (forall r. Ip.T r nodesStep am v) ->
   Exp Int ->
   Exp a ->
   Causal.T (am, am) v
dynamicGen limitMaxShape ipLeap ipStep periodInt period =
   let periodWord = wordFromInt periodInt
       cellMargin = combineMarginParams ipLeap ipStep periodInt
       minShape = wordFromInt $ fst $ shapeMargin cellMargin periodInt

   in  Func.withArgs $ \(shape, phase) ->
          let (windows, skips, fracs) =
                 limitMaxShape cellMargin $
                 unzip (integrateFrac $& (limitMinShape minShape $& shape))
              (offsets, shapePhases) =
                 unzip
                    (flattenShapePhaseProc periodWord period $&
                       (constantFromWord minShape + fracs)
                       &|&
                       (Causal.osciCoreSync $&
                          phase
                          &|&
                          negate
                             (Causal.map ((/period)) $&
                                (Causal.map Expr.fromIntegral $& skips))))
          in interpolateCell ipLeap ipStep $&
                 (CausalPriv.map
                    (\(buffer, offset) -> do
                       p <- Expr.unExp periodWord
                       cellFromBuffer p buffer offset)
                  $&
                  windows
                  &|&
                  offsets)
                 &|&
                 shapePhases

constantFromWord ::
   (MultiValue.NativeFloating a ar) =>
   Exp Word -> Func.T inp (MultiValue.T a)
constantFromWord x =
   Func.fromSignal (Causal.map Expr.fromIntegral $* Sig.constant x)

limitMinShape ::
   (Marshal.C a, MultiValue.Select a, MultiValue.Comparison a,
    MultiValue.NativeFloating a ar) =>
   Exp Word ->
   Causal.T (MultiValue.T a) (MultiValue.T a)
limitMinShape xLim =
   Causal.mapAccum
      (\x lim ->
         Expr.unzip $
         Expr.select (x>=*lim) (Expr.zip (x-lim) zero) (Expr.zip zero (lim-x)))
      (Expr.fromIntegral xLim)

integrateFrac ::
   (Marshal.C a, MultiValue.Additive a,
    MultiValueVec.NativeFloating a ar, LLVM.IsPrimitive ar) =>
   Causal.T (MultiValue.T a) (MultiValue.T Word, MultiValue.T a)
integrateFrac =
   Causal.mapAccum
      (\a frac ->
         let s = ExprVec.splitFractionToInt (a+frac)
         in (s, snd s))
      zero


interpolateCell ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (forall r. Ip.T r nodesLeap a v) ->
   (forall r. Ip.T r nodesStep a v) ->
   Causal.T (nodesLeap (nodesStep v), (a, a)) v
interpolateCell ipLeap ipStep =
   CausalPriv.map
      (\(nodes, (leap,step)) ->
         ipLeap leap =<< mapM (ipStep step) nodes)

cellFromBuffer ::
   (Memory.C a, Ip.C nodesLeap, Ip.C nodesStep) =>
   MultiValue.T Word ->
   RingBuffer.T a ->
   MultiValue.T Word ->
   LLVM.CodeGenFunction r (nodesLeap (nodesStep a))
cellFromBuffer periodInt buffer offset =
   Ip.indexNodesExp
      (Ip.indexNodesExp (flip RingBuffer.index buffer) A.one)
      periodInt offset

elementMargin ::
   Exp (Ip.Margin (nodesLeap (nodesStep v))) ->
   Exp (Ip.Margin (nodesLeap (nodesStep (SerialClass.Element v))))
elementMargin = Expr.liftReprM return

peekCell ::
   (Storable.C a, MultiValue.T a ~ value, Ip.C nodesLeap, Ip.C nodesStep) =>
   Exp (Ip.Margin (nodesLeap (nodesStep value))) ->
   Exp Word ->
   Exp (Source.StorableVector a) ->
   Causal.T (MultiValue.T Word) (nodesLeap (nodesStep value))
peekCell margin periodWord vec =
   CausalPriv.map
      (\n -> do
         ~(MultiValue.Cons (ptr,_l)) <- Expr.unExp vec
         ~(MultiValue.Cons offset) <-
            Expr.unExp $ intFromWord (Expr.lift0 n) - Ip.marginOffsetExp margin
         perInt <- Expr.unExp $ intFromWord periodWord
         Ip.loadNodesExp (Ip.loadNodesExp Storable.load A.one) perInt
            =<< Storable.advancePtr offset ptr)


flattenShapePhaseProc ::
   (MultiValue.Field a, MultiValue.RationalConstant a, MultiValue.Fraction a) =>
   (MultiValue.NativeFloating a ar, MultiValueVec.NativeFloating a ar) =>
   Exp Word ->
   Exp a ->
   Causal.T
      (MultiValue.T a, MultiValue.T a)
      (MultiValue.T Word, (MultiValue.T a, MultiValue.T a))
flattenShapePhaseProc periodInt period =
   Causal.map
      (\(shape, phase) -> flattenShapePhase periodInt period shape phase)

_flattenShapePhaseProc ::
   (MultiValue.Field a, MultiValue.RationalConstant a, MultiValue.Fraction a) =>
   (MultiValue.NativeFloating a ar) =>
   Exp Word ->
   Exp a ->
   Causal.T
      (MultiValue.T a, MultiValue.T a)
      (MultiValue.T Word, (MultiValue.T a, MultiValue.T a))
_flattenShapePhaseProc period32 period =
   CausalPriv.map
      (\(shape, phase) -> do
         perInt <- Expr.unExp period32
         per <- Expr.unExp period
         _flattenShapePhase perInt per shape phase)

flattenShapePhaseProcPacked ::
   (TypeNum.Positive n, MultiVector.Field a, MultiVector.RationalConstant a) =>
   (MultiVector.Fraction a, MultiVector.NativeFloating n a ar) =>
   Exp Word ->
   Exp a ->
   Causal.T
      (Serial.Value n a, Serial.Value n a)
      (Serial.Value n Word, (Serial.Value n a, Serial.Value n a))
flattenShapePhaseProcPacked periodInt period =
   Causal.zipWith
      (flattenShapePhase
         (SerialExp.upsample periodInt) (SerialExp.upsample period))

flattenShapePhase ::
   (MultiValue.Field a, MultiValue.RationalConstant a, MultiValue.Fraction a) =>
   (MultiValueVec.NativeFloating a ar, MultiValueVec.NativeInteger i ir) =>
   (LLVM.ShapeOf ir ~ LLVM.ShapeOf ar) =>
   Exp i -> Exp a ->
   Exp a -> Exp a ->
   (Exp i, (Exp a, Exp a))
flattenShapePhase periodInt period shape phase =
   let qLeap = Expr.fraction $ shape/period - phase
       (n,qStep) =
          ExprVec.splitFractionToInt $
          {-
          If 'shape' is correctly limited,
          the value is always non-negative algebraically,
          but maybe not numerically.
          -}
          Expr.max zero $
          shape - qLeap * ExprVec.fromIntegral periodInt
   in (n,(qLeap,qStep))

_flattenShapePhase ::
   (MultiValue.Field a, MultiValue.RationalConstant a, MultiValue.Fraction a) =>
   (MultiValue.NativeFloating a ar, MultiValue.NativeInteger i ir) =>
   MultiValue.T i ->
   MultiValue.T a ->
   MultiValue.T a -> MultiValue.T a ->
   LLVM.CodeGenFunction r (MultiValue.T i, (MultiValue.T a, MultiValue.T a))
_flattenShapePhase = Expr.unliftM4 $ \periodInt period shape phase ->
   let qLeap = Expr.fraction $ shape/period - phase
       (n,qStep) =
          Expr.splitFractionToInt $
          {-
          If 'shape' is correctly limited,
          the value is always non-negative algebraically,
          but maybe not numerically.
          -}
          Expr.max zero $
          shape - qLeap * Expr.fromIntegral periodInt
   in  (n,(qLeap,qStep))


limitShape ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Marshal.C t, MultiValue.Real t, MultiValue.NativeFloating t tr) =>
   (i ~ Int) =>
   Exp (Ip.Margin (nodesLeap (nodesStep value))) ->
   Exp i -> Exp i -> Causal.MV t t
limitShape margin periodInt len =
   Causal.zipWith Expr.limit
   $<
   limitShapeSignal margin periodInt len

limitShapePacked ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Marshal.C t, MultiValue.NativeFloating t tr) =>
   (TypeNum.Positive n, MultiVector.Real t) =>
   (i ~ Int) =>
   Exp (Ip.Margin (nodesLeap (nodesStep value))) ->
   Exp i ->
   Exp i ->
   Causal.T (Serial.Value n t) (Serial.Value n t)
limitShapePacked margin periodInt len =
   Causal.zipWith
      (\(minShape,maxShape) shape ->
         SerialExp.limit
            (SerialExp.upsample minShape,
             SerialExp.upsample maxShape)
            shape)
   $<
   limitShapeSignal margin periodInt len

limitShapeSignal ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (Marshal.C t, MultiValue.NativeFloating t tr) =>
   (i ~ Int) =>
   Exp (Ip.Margin (nodesLeap (nodesStep value))) ->
   Exp i ->
   Exp i ->
   Sig.T (MultiValue.T t, MultiValue.T t)
limitShapeSignal margin periodInt len =
   SigPriv.Cons
      (\minMax -> noLocalPtr $ \() -> return (minMax, ()))
      (do
         limits <-
            Expr.bundle
               (mapPair (Expr.fromIntegral, Expr.fromIntegral) $
                shapeLimits margin periodInt len)
         return (limits, ()))
      (const $ return ())


shapeLimits ::
   (Ip.C nodesLeap, Ip.C nodesStep, Exp Int ~ t) =>
   Exp (Ip.Margin (nodesLeap (nodesStep value))) ->
   t -> t -> (t, t)
shapeLimits margin periodInt len =
   case shapeMargin margin periodInt of
      (leftMargin, rightMargin) -> (leftMargin, len - rightMargin)

shapeMargin ::
   (Ip.C nodesLeap, Ip.C nodesStep, Exp Int ~ i) =>
   Exp (Ip.Margin (nodesLeap (nodesStep value))) ->
   i -> (i, i)
shapeMargin margin periodInt =
   let (marginNumber, marginOffset) =
         Expr.unzip $
         Expr.lift1 (uncurry MultiValue.zip . Ip.unzipMargin) margin
       leftMargin = marginOffset + periodInt
       rightMargin = marginNumber - leftMargin
   in  (leftMargin, rightMargin)

_shapeLimits ::
   (Ip.C nodesLeap, Ip.C nodesStep) =>
   (MultiValue.NativeFloating t tr) =>
   (MultiValue.Additive t) =>
   Ip.Margin (nodesLeap (nodesStep value)) ->
   Exp Word -> Exp t -> (Exp t, Exp t)
_shapeLimits margin periodInt len =
   let (leftMargin, rightMargin) = _shapeMargin margin periodInt
   in  (Expr.fromIntegral leftMargin, len - Expr.fromIntegral rightMargin)

_shapeMargin ::
   (Ip.C nodesLeap, Ip.C nodesStep, Ring.C i) =>
   Ip.Margin (nodesLeap (nodesStep value)) ->
   i -> (i, i)
_shapeMargin margin periodInt =
   let leftMargin = fromIntegral (Ip.marginOffset margin) + periodInt
       rightMargin = fromIntegral (Ip.marginNumber margin) - leftMargin
   in  (leftMargin, rightMargin)

combineMarginParams ::
   (Ip.C nodesStep, Ip.C nodesLeap) =>
   (forall r. Ip.T r nodesLeap a v) ->
   (forall r. Ip.T r nodesStep a v) ->
   Exp Int ->
   Exp (Ip.Margin (nodesLeap (nodesStep v)))
combineMarginParams ipLeap ipStep periodInt =
   let marginLeap = Ip.toMargin ipLeap in
   let marginStep = Ip.toMargin ipStep in
   Expr.lift2 Ip.zipMargin
      (fromIntegral (Ip.marginNumber marginStep) +
       fromIntegral (Ip.marginNumber marginLeap) * periodInt)
      (fromIntegral (Ip.marginOffset marginStep) +
       fromIntegral (Ip.marginOffset marginLeap) * periodInt)

_combineMargins ::
   Ip.Margin (nodesLeap value) ->
   Ip.Margin (nodesStep value) ->
   Int ->
   Ip.Margin (nodesLeap (nodesStep value))
_combineMargins marginLeap marginStep periodInt =
   Ip.Margin {
      Ip.marginNumber =
         Ip.marginNumber marginStep +
         Ip.marginNumber marginLeap * periodInt,
      Ip.marginOffset =
         Ip.marginOffset marginStep +
         Ip.marginOffset marginLeap * periodInt
   }


{- |
@zigZagLong loopStart loopLength@
creates a curve that starts at 0
and is linear until it reaches @loopStart+loopLength@.
Then it begins looping in a ping-pong manner
between @loopStart+loopLength@ and @loopStart@.
It is useful as @shape@ control for looping a sound.
Input of the causal process is the slope (or frequency) control.
Slope values must not be negative.

*Main> Sig.renderChunky SVL.defaultChunkSize (Causal.take 25 <<< Helix.zigZagLong 6 10 $* 2) () :: SVL.Vector Float
VectorLazy.fromChunks [Vector.pack [0.0,1.999999,3.9999995,6.0,8.0,10.0,12.0,14.0,15.999999,14.000001,12.0,10.0,7.999999,6.0,8.0,10.0,12.0,14.0,16.0,14.0,11.999999,9.999998,7.999998,6.0000024,8.000002]]
-}
zigZagLong ::
   (Marshal.C a) =>
   (MultiValue.Select a, MultiValue.Comparison a, MultiValue.Fraction a) =>
   (MultiValue.Field a, MultiValue.RationalConstant a) =>
   Exp a -> Exp a -> Causal.MV a a
zigZagLong =
   zigZagLongGen (Causal.fromSignal . Sig.constant) zigZag

zigZagLongPacked ::
   (Marshal.Vector n a) =>
   (MultiVector.Field a, MultiVector.Fraction a) =>
   (MultiVector.RationalConstant a) =>
   (MultiVector.Select a, MultiVector.Comparison a) =>
   Exp a -> Exp a -> Causal.T (Serial.Value n a) (Serial.Value n a)
zigZagLongPacked =
   zigZagLongGen (Causal.fromSignal . SigPS.constant) zigZagPacked

zigZagLongGen ::
   (MultiValue.RationalConstant a, MultiValue.Field a) =>
   (A.RationalConstant al, A.Field al) =>
   (Exp a -> Causal.T al al) ->
   (Exp a -> Causal.T al al) ->
   Exp a -> Exp a -> Causal.T al al
zigZagLongGen constant zz prefix loop =
   zz (negate $ prefix/loop) * constant loop + constant prefix
   <<<
   id / constant loop

{- |
@zigZag start@ creates a zig-zag curve with values between 0 and 1, inclusively,
that is useful as @shape@ control for looping a sound.
Input of the causal process is the slope (or frequency) control.
Slope values must not be negative.
The start value must be at most 2 and may be negative.
-}
zigZag ::
   (Marshal.C a) =>
   (MultiValue.Select a, MultiValue.Comparison a, MultiValue.Fraction a) =>
   (MultiValue.Field a, MultiValue.RationalConstant a) =>
   Exp a -> Causal.MV a a
zigZag start =
   Causal.map (\x -> 1 - abs (1-x))
   <<<
   Causal.mapAccum
      (\d t0 -> let t1 = t0+d in (t0, wrap Expr.select (0<*) t1))
      start

zigZagPacked ::
   (TypeNum.Positive n) =>
   (Marshal.C a) =>
   (MultiVector.Field a, MultiVector.Fraction a) =>
   (MultiVector.RationalConstant a) =>
   (MultiVector.Select a, MultiVector.Comparison a) =>
   Exp a -> Causal.T (Serial.Value n a) (Serial.Value n a)
zigZagPacked start =
   Causal.map (\x -> 1 - abs (1-x))
   <<<
   Causal.mapAccum
      (\d t0 ->
         let (t1,cum) = SerialExp.cumulate t0 d
         in (wrap SerialExp.select (SerialExp.cmp LLVM.CmpLT zero) cum, t1))
      start

wrap ::
   (MultiValue.Field a, MultiValue.Fraction a, MultiValue.RationalConstant a) =>
   (Exp b -> Exp a -> Exp a -> Exp a) ->
   (Exp a -> Exp b) ->
   Exp a -> Exp a
wrap select positive a = select (positive a) (2 * Expr.fraction (a/2)) a
