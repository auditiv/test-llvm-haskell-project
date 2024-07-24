{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Synthesizer.LLVM.Server.Common (
   Real,
   SampleRate(SampleRate), expSampleRate,
   Instrument,
   ($+),
   constant, ($++),
   frequency, time, noiseReference, number,
   Quantity(..), Arg(..), Frequency, Time, Number,
   Input(..), InputArg(..), Parameter, Control, Signal,
   ArgTuple(..),
   Wrapped(..),
   amplitudeFromVelocity,
   ($/),

   piecewiseConstant,
   transposeModulation,

   pioApply,
   pioApplyCont,
   pioApplyToLazyTime,

   controllerAttack, controllerDetune, controllerTimbre0, controllerTimbre1,
   controllerFilterCutoff, controllerFilterResonance,
   controllerVolume,
   ) where

import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Private.Render as Render
import Synthesizer.LLVM.Causal.Process (($*))

import qualified Synthesizer.LLVM.MIDI.BendModulation as BM
import qualified Synthesizer.LLVM.ConstantPiece as Const
import qualified Synthesizer.MIDI.Storable as MidiSt
import qualified Synthesizer.MIDI.EventList as Ev
import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Generic.Signal as SigG

import qualified Sound.MIDI.Controller as Ctrl
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import qualified LLVM.DSL.Render.Argument as Arg
import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp)

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import qualified Numeric.NonNegative.Chunky as NonNegChunky
import qualified Numeric.NonNegative.Wrapper as NonNegW

import Control.Applicative (Applicative, liftA2, pure, (<*>), (<$>))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import qualified System.Unsafe as Unsafe

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



type Real = Float

type Instrument a sig = SampleRate a -> MidiSt.Instrument a sig


newtype SampleRate a = SampleRate a
   deriving (Show)

instance Functor SampleRate where
   fmap f (SampleRate sr) = SampleRate (f sr)

instance Fold.Foldable SampleRate where
   foldMap f (SampleRate sr) = f sr

instance Trav.Traversable SampleRate where
   traverse f (SampleRate sr) = SampleRate <$> f sr

instance Applicative SampleRate where
   pure = SampleRate
   SampleRate f <*> SampleRate sr = SampleRate $ f sr


instance (Render.RunArg a) => Render.RunArg (SampleRate a) where
   type DSLArg (SampleRate a) = SampleRate (Render.DSLArg a)
   buildArg =
      case Render.buildArg of
         Arg.Cons pass create ->
            Arg.Cons
               (SampleRate . pass)
               (\(SampleRate sr) -> create sr)

instance (MultiValue.C a) => MultiValue.C (SampleRate a) where
   type Repr (SampleRate a) = MultiValue.Repr a
   cons = multiValueSampleRate . fmap MultiValue.cons
   undef = multiValueSampleRate $ pure MultiValue.undef
   zero = multiValueSampleRate $ pure MultiValue.zero
   phi bb =
      fmap multiValueSampleRate .
      Trav.traverse (MultiValue.phi bb) . unMultiValueSampleRate
   addPhi bb a b =
      Fold.sequence_ $
      liftA2 (MultiValue.addPhi bb)
         (unMultiValueSampleRate a) (unMultiValueSampleRate b)

instance (Marshal.C a) => Marshal.C (SampleRate a) where
   pack (SampleRate a) = Marshal.pack a
   unpack = SampleRate . Marshal.unpack

multiValueSampleRate ::
   SampleRate (MultiValue.T a) -> MultiValue.T (SampleRate a)
multiValueSampleRate (SampleRate (MultiValue.Cons a)) = MultiValue.Cons a

unMultiValueSampleRate ::
   MultiValue.T (SampleRate a) -> SampleRate (MultiValue.T a)
unMultiValueSampleRate (MultiValue.Cons a) = SampleRate (MultiValue.Cons a)


expSampleRate :: Exp (SampleRate a) -> SampleRate (Exp a)
expSampleRate = SampleRate . Expr.lift1 MultiValue.cast



($/) :: (Functor f) => f (a -> b) -> a -> f b
f $/ x = fmap ($ x) f


infixr 0 $+, $++

($+) ::
   (SampleRate a -> b -> c) ->
   (c -> SampleRate a -> d) ->
   SampleRate a -> b -> d
(p$+f) sampleRate param = f (p sampleRate param) sampleRate

($++) ::
   (SampleRate a -> b -> c, b) ->
   (c -> SampleRate a -> d) ->
   SampleRate a -> d
((p,param)$++f) sampleRate = f (p sampleRate param) sampleRate

constant ::
   (SampleRate a -> b -> c) -> b ->
   (c -> SampleRate a -> d) ->
   SampleRate a -> d
constant p param f sampleRate = f (p sampleRate param) sampleRate


frequency :: (Field.C a) => SampleRate a -> a -> a
frequency (SampleRate sr) param = param / sr

time :: (Ring.C a) => SampleRate a -> a -> a
time (SampleRate sr) param = param * sr

noiseReference :: (Field.C a) => SampleRate a -> a -> a
noiseReference (SampleRate sr) freq = sr/freq

number :: SampleRate a -> a -> a
number = flip const


data Number
data Frequency
data Time
data NoiseReference

class Quantity quantity a where
   data Arg quantity a
   eval :: SampleRate a -> a -> Arg quantity a

instance Quantity Number a where
   data Arg Number a = Number a
   eval sampleRate a = Number $ number sampleRate a

instance (Field.C a) => Quantity Frequency a where
   data Arg Frequency a = Frequency a
   eval sampleRate a = Frequency $ frequency sampleRate a

instance (Ring.C a) => Quantity Time a where
   data Arg Time a = Time a
   eval sampleRate a = Time $ time sampleRate a

instance (Field.C a) => Quantity NoiseReference a where
   data Arg NoiseReference a = NoiseReference a
   eval sampleRate a = NoiseReference $ noiseReference sampleRate a


class Input signal a where
   data InputArg signal a
   type InputSource signal a
   evalInput :: SampleRate a -> InputSource signal a -> InputArg signal a

data Parameter b

instance Input (Parameter b) a where
   data InputArg (Parameter b) a = Parameter b
   type InputSource (Parameter b) a = b
   evalInput _sr = Parameter

data Control b

instance Input (Control b) a where
   data InputArg (Control b) a = Control (Sig.T b)
   type InputSource (Control b) a = Sig.T b
   evalInput _sr = Control

data Signal b

instance Input (Signal b) a where
   data InputArg (Signal b) a = Signal (Sig.T b)
   type InputSource (Signal b) a = Sig.T b
   evalInput _sr = Signal


class ArgTuple a tuple where
   type ArgPlain tuple
   evalTuple :: SampleRate a -> ArgPlain tuple -> tuple

instance (Quantity quantity b, a ~ b) => ArgTuple a (Arg quantity b) where
   type ArgPlain (Arg quantity b) = b
   evalTuple = eval

instance (Input signal b, a ~ b) => ArgTuple a (InputArg signal b) where
   type ArgPlain (InputArg signal b) = InputSource signal b
   evalTuple = evalInput

instance (ArgTuple a b, ArgTuple a c) => ArgTuple a (b,c) where
   type ArgPlain (b,c) = (ArgPlain b, ArgPlain c)
   evalTuple sampleRate (b,c) = (evalTuple sampleRate b, evalTuple sampleRate c)

instance (ArgTuple a b, ArgTuple a c, ArgTuple a d) => ArgTuple a (b,c,d) where
   type ArgPlain (b,c,d) = (ArgPlain b, ArgPlain c, ArgPlain d)
   evalTuple sampleRate (b,c,d) =
      (evalTuple sampleRate b, evalTuple sampleRate c, evalTuple sampleRate d)



class Wrapped a f where
   type Unwrapped f
   wrapped :: f -> SampleRate a -> Unwrapped f

instance (a ~ b) => Wrapped a (SampleRate b -> f) where
   type Unwrapped (SampleRate b -> f) = f
   wrapped f = f

instance
   (a ~ b, Quantity quantity b, Wrapped a f) =>
      Wrapped a (Arg quantity b -> f) where
   type Unwrapped (Arg quantity b -> f) = b -> Unwrapped f
   wrapped f sampleRate arg =
      wrapped (f (eval sampleRate arg)) sampleRate

instance
   (a ~ b, Input signal b, Wrapped a f) =>
      Wrapped a (InputArg signal b -> f) where
   type Unwrapped (InputArg signal b -> f) =
         InputSource signal b -> Unwrapped f
   wrapped f sampleRate arg =
      wrapped (f (evalInput sampleRate arg)) sampleRate

instance
   (ArgTuple a b, ArgTuple a c, Wrapped a f) =>
      Wrapped a ((b,c) -> f) where
   type Unwrapped ((b,c) -> f) = (ArgPlain b, ArgPlain c) -> Unwrapped f
   wrapped f sampleRate arg =
      wrapped (f (evalTuple sampleRate arg)) sampleRate

instance
   (ArgTuple a b, ArgTuple a c, ArgTuple a d, Wrapped a f) =>
      Wrapped a ((b,c,d) -> f) where
   type Unwrapped ((b,c,d) -> f) =
         (ArgPlain b, ArgPlain c, ArgPlain d) -> Unwrapped f
   wrapped f sampleRate arg =
      wrapped (f (evalTuple sampleRate arg)) sampleRate


{-# INLINE amplitudeFromVelocity #-}
amplitudeFromVelocity :: (Trans.C a) => a -> a
amplitudeFromVelocity vel = fromInteger 4 ^? vel


piecewiseConstant :: (Memory.C a) => Sig.T (Const.T a) -> Sig.T a
piecewiseConstant = Const.flatten

transposeModulation :: (Field.C a, Expr.Aggregate a am) =>
   SampleRate a -> a -> Sig.T (Const.T (BM.T am)) -> Sig.T (Const.T (BM.T am))
transposeModulation (SampleRate sampleRate) freq xs =
   Const.causalMap (BM.shift (freq/sampleRate)) $* xs



pioApply ::
   (Storable a, Storable b) =>
   PIO.T (SV.Vector a) (SV.Vector b) -> SVL.Vector a -> SVL.Vector b
pioApply = pioApplyCont (const SVL.empty)

pioApplyCont ::
   (Storable a, Storable b) =>
   (SVL.Vector a -> SVL.Vector b) ->
   PIO.T (SV.Vector a) (SV.Vector b) -> SVL.Vector a -> SVL.Vector b
pioApplyCont cont proc sig = Unsafe.performIO $ do
   act <- PIO.runStorableChunkyCont proc
   return $ act cont sig

pioApplyToLazyTime ::
   (Storable b) =>
   PIO.T SigG.LazySize (SV.Vector b) -> Ev.LazyTime -> SVL.Vector b
pioApplyToLazyTime proc sig = Unsafe.performIO $ do
   act <- PIO.runCont proc
   return $ SVL.fromChunks $ act (const []) $
      map (SigG.LazySize . NonNegW.toNumber) $
      concatMap PC.chopLongTime $ NonNegChunky.toChunks sig



controllerAttack, controllerDetune, controllerTimbre0, controllerTimbre1,
   controllerFilterCutoff, controllerFilterResonance,
   controllerVolume :: VoiceMsg.Controller
controllerAttack = Ctrl.attackTime
controllerDetune = Ctrl.chorusDepth   -- Ctrl.effect3Depth
controllerTimbre0 = Ctrl.soundVariation
controllerTimbre1 = Ctrl.timbre
controllerFilterCutoff = Ctrl.effect4Depth
controllerFilterResonance = Ctrl.effect5Depth
controllerVolume = Ctrl.volume
