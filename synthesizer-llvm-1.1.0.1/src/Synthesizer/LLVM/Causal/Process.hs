{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Causal.Process (
   Causal.T, MV,
   CausalClass.fromSignal,
   CausalClass.toSignal,
   (CausalClass.$<), (CausalClass.$>), (CausalClass.$*),
   ($<#), ($>#), ($*#),
   map,
   zipWith,
   takeWhile,
   take,
   mix,
   raise,
   envelope,
   envelopeStereo,
   amplify,
   amplifyStereo,
   mapLinear,
   mapExponential,
   loop,
   loopZero,
   integrate,
   integrateZero,
   delay1,
   delayControlled,
   delayControlledInterpolated,
   differentiate,
   feedbackControlled,
   feedbackControlledZero,
   mapAccum,
   fromModifier,
   osciCoreSync,
   osciCore,
   osci,
   shapeModOsci,
   skip,
   frequencyModulation,
   frequencyModulationLinear,
   Causal.quantizeLift,
   track,
   delay,
   delayZero,
   Causal.replicateControlled,
   replicateControlledParam,
   stereoFromMono,
   stereoFromMonoControlled,
   stereoFromMonoParametric,
   comb,
   combStereo,
   reverbExplicit,
   reverbParams,
   trigger,
   arrayElement,
   vectorize,
   pipeline,
   ) where

import qualified Synthesizer.LLVM.Causal.Parametric as Parametric
import qualified Synthesizer.LLVM.Causal.Private as Causal
import qualified Synthesizer.LLVM.Generator.Private as SigPriv
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.RingBuffer as RingBuffer
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame as Frame
import Synthesizer.LLVM.Generator.Private (arraySize)
import Synthesizer.LLVM.Private (noLocalPtr, unbool)

import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Causal.Class as CausalClass
import Synthesizer.Causal.Class (($*), ($<))

import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Vector as MultiVector
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Maybe as Maybe
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Iterator as Iter
import qualified LLVM.Extra.Control as C
import qualified LLVM.Extra.Arithmetic as A

import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal ((:<:))
import Type.Base.Proxy (Proxy(Proxy))

import qualified Data.List as List
import Data.Traversable (sequenceA)
import Data.Tuple.HT (mapSnd, swap)
import Data.Word (Word)

import qualified Control.Arrow as Arrow
import qualified Control.Category as Cat
import qualified Control.Monad.Trans.State as MS
import qualified Control.Functor.HT as FuncHT
import qualified Control.Applicative.HT as App
import Control.Arrow (Arrow, arr, (<<<), (^<<), (<<^), (>>>), (***), (&&&))
import Control.Applicative (pure, liftA2, liftA3, (<$>))

import qualified System.Unsafe as Unsafe
import System.Random (Random, RandomGen, randomR)

import qualified Algebra.Additive as Additive
import NumericPrelude.Numeric
import NumericPrelude.Base hiding (map, zipWith, takeWhile, take)
import Prelude ()


type MV a b = Causal.T (MultiValue.T a) (MultiValue.T b)


infixl 0 $<#, $>#, $*#

{- |
provide constant input in a comfortable way
-}
($*#) ::
   (CausalClass.C process, CausalClass.SignalOf process ~ signal,
    MultiValue.C a) =>
   process (MultiValue.T a) b -> a -> signal b
proc $*# x = CausalClass.applyConst proc $ MultiValue.cons x

($<#) ::
   (CausalClass.C process, MultiValue.C a) =>
   process (MultiValue.T a, b) c -> a -> process b c
proc $<# x = CausalClass.applyConstFst proc $ MultiValue.cons x

($>#) ::
   (CausalClass.C process, MultiValue.C b) =>
   process (a, MultiValue.T b) c -> b -> process a c
proc $># x = CausalClass.applyConstSnd proc $ MultiValue.cons x



map ::
   (Expr.Aggregate ae a, Expr.Aggregate be b) =>
   (ae -> be) -> Causal.T a b
map f = Causal.map (\a -> Expr.bundle (f (Expr.dissect a)))

zipWith ::
   (Expr.Aggregate ae a, Expr.Aggregate be b, Expr.Aggregate ce c) =>
   (ae -> be -> ce) -> Causal.T (a,b) c
zipWith f = map (uncurry f)

takeWhile :: (Expr.Aggregate ae a) => (ae -> Exp Bool) -> Causal.T a a
takeWhile p = Causal.simple
   (\a () -> do
      MaybeCont.guard . unbool =<< MaybeCont.lift (Expr.unliftM1 p a)
      return (a,()))
   (return ())

take :: Exp Word -> Causal.T a a
take len =
   arr snd $< (takeWhile (0 Expr.<*) $* Sig.iterate (subtract 1) len)


{- |
You may also use '(+)'.
-}
mix :: (A.Additive a) => Causal.T (a,a) a
mix = Causal.zipWith Frame.mix

{- |
You may also use '(+)' and a 'Sig.constant' signal or a number literal.
-}
raise :: (Marshal.C a, MultiValue.Additive a) => Exp a -> MV a a
raise x = mix $< Sig.constant x


{- |
You may also use '(*)'.
-}
envelope :: (A.PseudoRing a) => Causal.T (a, a) a
envelope = Causal.zipWith Frame.amplifyMono

envelopeStereo :: (A.PseudoRing a) => Causal.T (a, Stereo.T a) (Stereo.T a)
envelopeStereo = Causal.zipWith Frame.amplifyStereo

{- |
You may also use '(*)' and a 'Sig.constant' signal or a number literal.
-}
amplify ::
   (Expr.Aggregate ea a, Memory.C a, A.PseudoRing a) =>
   ea -> Causal.T a a
amplify x = envelope $< Sig.constant x

amplifyStereo ::
   (Marshal.C a, MultiValue.PseudoRing a, Stereo.T (MultiValue.T a) ~ stereo) =>
   Exp a -> Causal.T stereo stereo
amplifyStereo x = envelopeStereo $< Sig.constant x


mapLinear ::
   (Marshal.C a, MultiValue.T a ~ am,
    MultiValue.PseudoRing a, MultiValue.IntegerConstant a) =>
   Exp a -> Exp a -> Causal.T am am
mapLinear depth center = map (\x -> center + depth*x)

-- ToDo: use base 2
mapExponential ::
   (Marshal.C a, MultiValue.T a ~ am,
    MultiValue.Transcendental a, MultiValue.RationalConstant a) =>
   Exp a -> Exp a -> Causal.T am am
mapExponential depth center =
   let logDepth = log depth
   in map (\x -> center * exp (logDepth * x))


loop ::
   (Expr.Aggregate ce c, Memory.C c) =>
   ce -> Causal.T (a,c) (b,c) -> Causal.T a b
loop initial = Causal.loop (Expr.bundle initial)

loopZero ::
   (A.Additive c, Memory.C c) =>
   Causal.T (a,c) (b,c) -> Causal.T a b
loopZero = Causal.loop (return A.zero)

loopConst ::
   (Memory.C c) =>
   c -> Causal.T (a,c) (b,c) -> Causal.T a b
loopConst c = Causal.loop (return c)


integrate ::
   (Expr.Aggregate ae a, A.Additive a, Memory.C a) => ae -> Causal.T a a
integrate initial = loop initial (arr snd &&& Causal.zipWith A.add)

integrateZero :: (A.Additive a, Memory.C a) => Causal.T a a
integrateZero = loopZero (arr snd &&& Causal.zipWith A.add)


feedbackControlledAux ::
   (Arrow arrow) =>
   arrow ((ctrl,a),c) b ->
   arrow (ctrl,b) c ->
   arrow ((ctrl,a),c) (b,c)
feedbackControlledAux forth back =
   arr snd &&& back  <<<  arr (fst.fst) &&& forth

feedbackControlled ::
   (Expr.Aggregate ce c, Memory.C c) =>
   ce -> Causal.T ((ctrl,a),c) b -> Causal.T (ctrl,b) c -> Causal.T (ctrl,a) b
feedbackControlled initial forth back =
   loop initial (feedbackControlledAux forth back)

feedbackControlledZero ::
   (A.Additive c, Memory.C c) =>
   Causal.T ((ctrl,a),c) b -> Causal.T (ctrl,b) c -> Causal.T (ctrl,a) b
feedbackControlledZero forth back =
   loopZero (feedbackControlledAux forth back)


arrayPtr ::
   (TypeNum.Natural n, LLVM.IsSized a) =>
   LLVM.Value (LLVM.Ptr a) ->
   LLVM.CodeGenFunction r (LLVM.Value (LLVM.Ptr (LLVM.Array n a)))
arrayPtr = LLVM.bitcast

replicateControlledParam ::
   (TypeNum.Natural n) =>
   (Tuple.Undefined a, Tuple.Phi a) =>
   (Marshal.C b, (n TypeNum.:*: LLVM.SizeOf (Marshal.Struct b)) ~ bSize,
    TypeNum.Natural bSize) =>
   (Exp b -> Causal.T (c,a) a) ->
   Exp (MultiValue.Array n b) -> Causal.T (c,a) a
replicateControlledParam f ps = Unsafe.performIO $ do
   let n :: Word
       n = TypeNum.integralFromProxy $ arraySize ps
   paramd <- Parametric.fromProcessPtr "Causal.replicateControlledParam" f
   return $
      case paramd of
         Parametric.Cons next start stop ->
            Causal.Cons
               (\(bPtr,globalPtr) localPtr (c,a0) statePtr -> do
                  a1 <-
                     MaybeCont.fromBool $
                     Iter.mapWhileState_
                        (\(biPtr,globalIPtr,localIPtr,stateIPtr)
                              (_cont,ai0) -> do
                           global <- Memory.load globalIPtr
                           local <- Memory.load localIPtr
                           state0 <- Memory.load stateIPtr
                           (conti,(ai1,state1)) <-
                              MaybeCont.toBool $
                              next biPtr global local (c,ai0) state0
                           flip LLVM.store stateIPtr =<< Memory.compose state1
                           return (conti,(conti,ai1)))
                        (Iter.take (LLVM.valueOf n) $
                         App.lift4 (,,,)
                           (Iter.arrayPtrs bPtr)
                           (Iter.arrayPtrs globalPtr)
                           (Iter.arrayPtrs localPtr)
                           (Iter.arrayPtrs statePtr))
                        (LLVM.valueOf True, a0)
                  return (a1, statePtr))
               (do
                  bArr <- Expr.unExp ps
                  bPtr <- LLVM.arrayMalloc n
                  Memory.store bArr =<< arrayPtr bPtr
                  {-
                  ToDo:
                  Instead of a pointer to a malloced with dynamic length
                  we could use LLVM.Array.
                  However, we would have to establish the constraint
                  Natural (n :*: LLVM.SizeOf (Marshal.Struct a))
                  This is pretty cumbersome
                  with current decimal number representation.
                  It would be feasible with type-level natural numbers, though.
                  -}
                  globalPtr <- LLVM.arrayMalloc n
                  statePtr <- LLVM.arrayMalloc n
                  Iter.mapM_
                     (\(biPtr,globalIPtr,stateIPtr) -> do
                        (global,state) <- start biPtr
                        flip LLVM.store globalIPtr =<< Memory.compose global
                        flip LLVM.store stateIPtr =<< Memory.compose state)
                     (Iter.take (LLVM.valueOf n) $
                      liftA3 (,,)
                        (Iter.arrayPtrs bPtr)
                        (Iter.arrayPtrs globalPtr)
                        (Iter.arrayPtrs statePtr))
                  return ((bPtr,globalPtr), statePtr))
               (\(bPtr,globalPtr) ->
                  Iter.mapM_
                     (\(biPtr,globalIPtr) -> do
                        stop biPtr =<< Memory.load globalIPtr)
                     (Iter.take (LLVM.valueOf n) $
                      liftA2 (,)
                        (Iter.arrayPtrs bPtr)
                        (Iter.arrayPtrs globalPtr)))


{- |
Run a causal process independently on each stereo channel.
-}
stereoFromMono ::
   (Tuple.Phi a, Tuple.Undefined a, Tuple.Phi b, Tuple.Undefined b) =>
   Causal.T a b -> Causal.T (Stereo.T a) (Stereo.T b)
stereoFromMono proc =
   snd
   ^<<
   Causal.replicateSerial 2
      ((\((x,a),b) -> (Stereo.swap a, Stereo.cons (Stereo.right b) x))
       ^<<
       Arrow.first ((proc <<^ Stereo.left) &&& Cat.id))
   <<^
   (\a -> (a, Tuple.undef))

stereoFromMonoControlled ::
   (Tuple.Phi a, Tuple.Phi b, Tuple.Phi c,
    Tuple.Undefined a, Tuple.Undefined b, Tuple.Undefined c) =>
   Causal.T (c,a) b -> Causal.T (c, Stereo.T a) (Stereo.T b)
stereoFromMonoControlled proc =
   stereoFromMono proc <<^ (\(c,sa) -> (,) c <$> sa)

arrayFromStereo ::
   (Marshal.C a) =>
   Stereo.T (MultiValue.T a) ->
   LLVM.CodeGenFunction r (MultiValue.T (MultiValue.Array TypeNum.D2 a))
arrayFromStereo a =
   MultiValue.insertArrayValue TypeNum.d0 (Stereo.left a) =<<
   MultiValue.insertArrayValue TypeNum.d1 (Stereo.right a) MultiValue.undef

stereoFromMonoParametric ::
   (Marshal.C x,
    Tuple.Phi a, Tuple.Undefined a, Tuple.Phi b, Tuple.Undefined b) =>
   ((TypeNum.D2 TypeNum.:*: LLVM.SizeOf (Marshal.Struct x)) ~ xSize,
    TypeNum.Natural xSize) =>
   (Exp x -> Causal.T a b) ->
   Stereo.T (Exp x) -> Causal.T (Stereo.T a) (Stereo.T b)
stereoFromMonoParametric f sx =
   snd
   ^<<
   replicateControlledParam
      (\x ->
         (\((y,a),b) -> (Stereo.swap a, Stereo.cons (Stereo.right b) y))
         ^<<
         Arrow.first ((f x <<^ Stereo.left) &&& Cat.id)
         <<^
         snd)
      (Expr.liftM arrayFromStereo sx)
   <<^
   (\a -> ((),(a,Tuple.undef)))


mapAccum ::
   (Expr.Aggregate state statel, Memory.C statel,
    Expr.Aggregate a al, Expr.Aggregate b bl) =>
   (a -> state -> (b, state)) -> state -> Causal.T al bl
mapAccum next start =
   Causal.mapAccum
      (\a s -> Expr.bundle $ next (Expr.dissect a) (Expr.dissect s))
      (Expr.bundle start)

fromModifier ::
   (Expr.Aggregate ae al,
    Expr.Aggregate be bl,
    Expr.Aggregate ce cl,
    Expr.Aggregate se sl, Memory.C sl) =>
   Modifier.Simple se ce ae be -> Causal.T (cl,al) bl
fromModifier (Modifier.Simple initial step) =
   mapAccum (\(c,a) -> MS.runState (step c a)) initial


delay1 :: (Expr.Aggregate ae a, Memory.C a) => ae -> Causal.T a a
delay1 initial  =  loop initial (arr swap)

differentiate ::
   (A.Additive a, Expr.Aggregate ae a, Memory.C a) => ae -> Causal.T a a
differentiate initial  =  Cat.id - delay1 initial


{- |
Compute the phases from phase distortions and frequencies.

It's like integrate but with wrap-around performed by @fraction@.
For FM synthesis we need also negative phase distortions,
thus we use 'A.addToPhase' which supports that.
-}
osciCore, _osciCore, osciCoreSync ::
   (Memory.C t, A.Fraction t) => Causal.T (t, t) t
_osciCore =
   Causal.zipWith A.addToPhase <<<
   Arrow.second
      (Causal.mapAccum
         (\a s -> do
            b <- A.incPhase a s
            return (s,b))
         (return A.zero))

{-
This could be implemented using a generalized frequencyModulation,
however, osciCoreSync allows for negative phase differences.
-}
osciCoreSync =
   Causal.zipWith A.addToPhase <<<
   Arrow.second
      (Causal.mapAccum
         (\a s -> do
            b <- A.incPhase a s
            return (b,b))
         (return A.zero))

osciCore =
   Causal.zipWith A.addToPhase <<<
   Arrow.second (loopZero (arr snd &&& Causal.zipWith A.incPhase))

osci ::
   (Memory.C t, A.Fraction t) =>
   (forall r. t -> LLVM.CodeGenFunction r y) ->
   Causal.T (t, t) y
osci wave  =  Causal.map wave <<< osciCore

shapeModOsci ::
   (Memory.C t, A.Fraction t) =>
   (forall r. c -> t -> LLVM.CodeGenFunction r y) ->
   Causal.T (c, (t, t)) y
shapeModOsci wave  =  Causal.zipWith wave <<< Arrow.second osciCore


{- |
Feeds a signal into a causal process while holding or skipping signal elements
according to the process input.
The skip happens after a value is passed from the fed signal.

@skip x $* 0@ repeats the first signal value in the output.
@skip x $* 1@ feeds the signal to the output as is.
@skip x $* 2@ feeds the signal to the output with double speed.
-}
skip ::
   (Tuple.Undefined a, Tuple.Phi a, Memory.C a) =>
   Sig.T a -> Causal.T (MultiValue.T Word) a
skip (SigPriv.Cons next start stop) = Causal.Cons
   (\global local n1 (yState0, MultiValue.Cons n0) -> do
      yState1@(y,_) <-
         MaybeCont.fromMaybe $ fmap snd $
         MaybeCont.fixedLengthLoop n0 yState0 $
         next global local . snd
      return (y, (yState1,n1)))
   (mapSnd (\s -> ((Tuple.undef, s), A.one)) <$> start)
   stop

frequencyModulation ::
   (Marshal.C a,
    MultiValue.IntegerConstant a,
    MultiValue.Additive a,
    MultiValue.Comparison a,
    Tuple.Undefined nodes, Tuple.Phi nodes, Memory.C nodes) =>
   (forall r. MultiValue.T a -> nodes -> LLVM.CodeGenFunction r v) ->
   SigPriv.T nodes -> Causal.T (MultiValue.T a) v
frequencyModulation ip (SigPriv.Cons next start stop) = Causal.Cons
   (\global local k yState0 -> do
      ((nodes2,state2), ss2) <-
         MaybeCont.fromBool $
         C.whileLoop
            (LLVM.valueOf True, yState0)
            (\(cont0, (_, ss0)) ->
               LLVM.and cont0 . unbool =<< MultiValue.cmp LLVM.CmpGE ss0 A.one)
            (\(_,((_,state0), ss0)) ->
               MaybeCont.toBool $ liftA2 (,)
                  (next global local state0)
                  (MaybeCont.lift $ A.sub ss0 A.one))

      MaybeCont.lift $ do
         y <- ip ss2 nodes2
         ss3 <- A.add ss2 k
         return (y, ((nodes2, state2), ss3)))
   (fmap (\(global,sa) -> (global, ((Tuple.undef, sa), A.one))) start)
   stop

frequencyModulationLinear ::
   (MultiValue.PseudoRing a, MultiValue.IntegerConstant a,
    MultiValue.Comparison a, Marshal.C a) =>
   Sig.MV a -> MV a a
frequencyModulationLinear sig =
   frequencyModulation Interpolation.linear (Sig.adjacentNodes02 sig)


track ::
   (Expr.Aggregate ae al, Memory.C al) =>
   ae -> Exp Word -> Causal.T al (RingBuffer.T al)
track initial time = Causal.Cons
   (\(size0,ptr) -> noLocalPtr $ \a remain0 -> MaybeCont.lift $ do
      Memory.store a =<< LLVM.getElementPtr ptr (remain0, ())
      cont <- A.cmp LLVM.CmpGT remain0 A.zero
      remain1 <- C.ifThenSelect cont size0 (A.dec remain0)
      size1 <- A.inc size0
      return (RingBuffer.Cons ptr size1 remain0 remain1, remain1))
   (do
      MultiValue.Cons size0 <- Expr.unExp time
      size1 <- A.inc size0
      ptr <- LLVM.arrayMalloc size1
      a <- Expr.bundle initial
      -- cf. LLVM.Storable.Signal.fill
      C.arrayLoop size1 ptr () $ \ ptri () -> Memory.store a ptri
      return ((size0,ptr), size0))
   (LLVM.free . snd)

{- |
Delay time must be non-negative.
-}
delay ::
   (Expr.Aggregate ae al, Memory.C al) =>
   ae -> Exp Word -> Causal.T al al
delay initial time = Causal.map RingBuffer.oldest <<< track initial time

delayZero ::
   (Expr.Aggregate ae al, Additive.C ae, Memory.C al) =>
   Exp Word -> Causal.T al al
delayZero = delay zero

{- |
Delay time must be greater than zero!
-}
comb ::
   (Marshal.C a, MultiValue.PseudoRing a) =>
   Exp a -> Exp Word -> MV a a
comb gain time =
   loopZero (mix >>> (Cat.id &&& (delayZero (time-1) >>> amplify gain)))

combStereo ::
   (Marshal.C a, MultiValue.PseudoRing a, Stereo.T (MultiValue.T a) ~ stereo) =>
   Exp a -> Exp Word -> Causal.T stereo stereo
combStereo gain time =
   loopZero (mix >>> (Cat.id &&& (delayZero (time-1) >>> amplifyStereo gain)))

reverbExplicit ::
   (TypeNum.Natural n, (n TypeNum.:*: LLVM.UnknownSize) ~ paramSize,
    TypeNum.Natural paramSize) =>
   (Marshal.C a,
    MultiValue.Field a, MultiValue.Real a, MultiValue.IntegerConstant a) =>
   Exp (MultiValue.Array n (a,Word)) -> MV a a
reverbExplicit params =
   amplify (Expr.recip $ TypeNum.integralFromProxy $ arraySize params)
   <<<
   replicateControlledParam
      (\p -> Arrow.first (comb (Expr.fst p) (Expr.snd p)) >>> mix)
      params
   <<^
   (\a -> (a,a))

reverbParams ::
   (RandomGen g, TypeNum.Integer n, Random a) =>
   g -> Proxy n -> (a,a) -> (Word, Word) -> MultiValue.Array n (a, Word)
reverbParams rnd Proxy gainRange timeRange =
   flip MS.evalState rnd $
   sequenceA $ pure $
   liftA2 (,)
      (MS.state (randomR gainRange))
      (MS.state (randomR timeRange))


{- |
Delay by a variable amount of samples.
The momentum delay must be between @0@ and @maxTime@, inclusively.
How about automated clipping?
-}
delayControlled ::
   (Expr.Aggregate ae al, Memory.C al) =>
   ae -> Exp Word -> Causal.T (MultiValue.T Word, al) al
delayControlled initial maxTime =
   Causal.zipWith RingBuffer.index
   <<<
   arr (\(MultiValue.Cons i) -> i) *** track initial maxTime

{- |
Delay by a variable fractional amount of samples.
Non-integer delays are achieved by interpolation.
The momentum delay must be between @0@ and @maxTime@, inclusively.
-}
delayControlledInterpolated ::
   (Interpolation.C nodes) =>
   (MultiValue.T a ~ am) =>
   (MultiValue.NativeFloating a ar, MultiValue.Additive a) =>
   (Expr.Aggregate ve v, Memory.C v) =>
   (forall r. Interpolation.T r nodes am v) ->
   ve -> Exp Word -> Causal.T (am, v) v
delayControlledInterpolated ip initial maxTime =
   let margin = Interpolation.toMargin ip
   in Causal.zipWith
         (\del buf -> do
            let offset =
                  A.fromInteger' $ fromIntegral $
                  Interpolation.marginOffset margin
            n <- A.max offset =<< MultiValue.truncateToInt del
            k <- A.sub del =<< MultiValue.fromIntegral n
            ~(MultiValue.Cons m) <- A.sub n (offset :: MultiValue.T Word)
            ip k =<<
               Interpolation.indexNodes (flip RingBuffer.index buf) A.one m)
      <<<
      Arrow.second
         (track initial
             (fromIntegral (Interpolation.marginNumber margin) + maxTime))


{- |
This allows to compute a chain of equal processes efficiently,
if all of these processes can be bundled in one vectorial process.
Applications are an allpass cascade or an FM operator cascade.

The function expects that the vectorial input process
works like parallel scalar processes.
The different pipeline stages may be controlled by different parameters,
but the structure of all pipeline stages must be equal.
Our function feeds the input of the pipelined process
to the zeroth element of the Vector.
The result of processing the i-th element (the i-th channel, so to speak)
is fed to the (i+1)-th element.
The (n-1)-th element of the vectorial process is emitted
as output of the pipelined process.

The pipeline necessarily introduces a delay of (n-1) values.
For simplification we extend this to n values delay.
If you need to combine the resulting signal from the pipeline
with another signal in a 'zip'-like way,
you may delay that signal with @pipeline id@.
The first input values in later stages of the pipeline
are initialized with zero.
If this is not appropriate for your application,
then we may add a more sensible initialization.
-}
pipeline ::
   (TypeNum.Positive n, MultiVector.C x,
    v ~ MultiVector.T n x,
    a ~ MultiValue.T x,
    Tuple.Zero v, Memory.C v) =>
   Causal.T v v -> Causal.T a a
pipeline vectorProcess =
   loopConst MultiVector.zero $
      Causal.map (uncurry MultiVector.shiftUp)
      >>>
      Arrow.second vectorProcess


{-
insert and extract instructions will be in opposite order,
no matter whether we use foldr or foldl
and independent from the order of proc and channel in replaceChannel.
However, LLVM neglects the order anyway.
-}
vectorize ::
   (TypeNum.Positive n,
    MultiVector.C x, MultiValue.T x ~ a, MultiVector.T n x ~ va,
    MultiVector.C y, MultiValue.T y ~ b, MultiVector.T n y ~ vb) =>
   Causal.T a b -> Causal.T va vb
vectorize proc =
   withSize $ \n ->
      foldl
         (\acc i -> replaceChannel i proc acc)
         (arr (const Tuple.undef)) $
      List.take (TypeNum.integralFromSingleton n) [0 ..]

withSize ::
   (TypeNum.Positive n, MultiVector.T n a ~ v) =>
   (TypeNum.Singleton n -> f v) ->
   f v
withSize f = f TypeNum.singleton

{- |
Given a vector process, replace the i-th output by output
that is generated by a scalar process from the i-th input.
-}
replaceChannel ::
   (TypeNum.Positive n,
    MultiVector.C x, MultiValue.T x ~ a, MultiVector.T n x ~ va,
    MultiVector.C y, MultiValue.T y ~ b, MultiVector.T n y ~ vb) =>
   Int -> Causal.T a b -> Causal.T va vb -> Causal.T va vb
replaceChannel i channel proc =
   let li = LLVM.valueOf $ fromIntegral i
   in Causal.zipWith (MultiVector.insert li) <<<
         (channel <<< Causal.map (MultiVector.extract li)) &&&
         proc


{- |
Read the i-th element from each array.
-}
arrayElement ::
   (Marshal.C a, Marshal.Struct a ~ aStruct, LLVM.IsFirstClass aStruct,
    TypeNum.Natural i, TypeNum.Natural n, i :<: n) =>
   Proxy i -> Causal.T (MultiValue.T (MultiValue.Array n a)) (MultiValue.T a)
arrayElement i = Causal.map (MultiValue.extractArrayValue i)


{- |
@trigger fill signal@ sends @signal@ to the output
and restarts it whenever the process input is 'Just'.
Before the Arrow.first occurrence of 'Just'
and between instances of the signal the output is filled with 'Maybe.nothing'.
-}
trigger ::
   (Marshal.C a, Tuple.Undefined b, Tuple.Phi b) =>
   (Exp a -> Sig.T b) ->
   Causal.T (Maybe.T (MultiValue.T a)) (Maybe.T b)
trigger f = Unsafe.performIO $ do
   paramd <-
      Parametric.fromProcess "Causal.trigger" (CausalClass.fromSignal . f)
   return $
      case paramd of
         Parametric.Cons next start stop -> Causal.Cons
            (\globalPtr local ma ms0 -> MaybeCont.lift $ do
               ms1 <-
                  Maybe.run ma
                     (return ms0)
                     (\a -> do
                        stopAndFree stop globalPtr
                        (global2,state2) <- start a
                        Memory.store (Maybe.just (a,global2)) globalPtr
                        return $ Maybe.just state2)
               mc1 <- Memory.load globalPtr
               mcs1 <- Maybe.lift2 (,) mc1 ms1
               as2 <-
                  Maybe.run mcs1 (return Maybe.nothing) $ \((p1,c1),s1) ->
                     MaybeCont.toMaybe $ next p1 c1 local () s1
               return $ FuncHT.unzip as2)
            (do
               globalPtr <- LLVM.malloc
               Memory.store (nothingFromFunc f stop) globalPtr
               return (globalPtr, Maybe.nothing))
            (\globalPtr -> do
               stopAndFree stop globalPtr
               LLVM.free globalPtr)

stopAndFree ::
   (Memory.C global, Memory.C am) =>
   (am -> global -> LLVM.CodeGenFunction r ()) ->
   LLVM.Value (LLVM.Ptr (Memory.Struct (Maybe.T (am, global)))) ->
   LLVM.CodeGenFunction r ()
stopAndFree stop globalPtr = do
   maybeGlobal <- Memory.load globalPtr
   Maybe.for maybeGlobal $ \(a,global) -> stop a global

nothingFromFunc ::
   (MultiValue.C a, Tuple.Undefined global) =>
   (Exp a -> Sig.T b) ->
   (ap -> global -> code) ->
   Maybe.T (MultiValue.T a, global)
nothingFromFunc _ _ = Maybe.nothing
