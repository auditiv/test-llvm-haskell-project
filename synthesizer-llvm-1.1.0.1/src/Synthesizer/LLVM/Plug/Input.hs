{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Plug.Input (
   T(..),
   Default(..),
   rmap,
   split,
   fanout,
   lazySize,
   ignore,
   storableVector,
   piecewiseConstant,
   controllerSet,
   ) where

import qualified Synthesizer.Zip as Zip

import qualified Synthesizer.LLVM.ConstantPiece as Const

import qualified LLVM.Extra.Multi.Value.Storable as Storable
import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Control as C

import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal ((:*:))
import Type.Base.Proxy (Proxy)

import qualified Synthesizer.MIDI.PiecewiseConstant.ControllerSet as PCS
import qualified Synthesizer.Generic.Signal as SigG
import qualified Data.EventList.Relative.BodyTime as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeTime as EventListTT

import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Synthesizer.LLVM.Storable.Vector as SVU
import qualified Data.StorableVector as SV

import qualified Foreign.Marshal.Array as Array
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.ForeignPtr as FPtr
import Foreign.Storable (pokeElemOff)

import qualified Control.Functor.HT as FuncHT
import Control.Applicative (liftA2, (<$>))

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Tuple.Strict (mapFst, zipPair)
import Data.Word (Word)

import Prelude hiding (map)


{-
This datatype does not provide an early exit option, e.g. by Maybe.T,
since we warrant that the driver function will always
read only as much data as is available.
To this end you must provide a @length@ function
via an instance of 'Synthesizer.Generic.Cut.Read'.
-}
data T a b =
   forall state ioContext parameters.
      (Marshal.C parameters, Memory.C state) =>
   Cons
      (forall r.
       MultiValue.T parameters ->
       state -> LLVM.CodeGenFunction r (b, state))
         -- compute next value
      (forall r.
       MultiValue.T parameters ->
       LLVM.CodeGenFunction r state)
         -- initial state
      (a -> IO (ioContext, parameters))
         {- initialization from IO monad
         This is called once input chunk.
         This will be run within Unsafe.performIO,
         so no observable In/Out actions please!
         -}
      (ioContext -> IO ())
         {-
         finalization from IO monad, also run within Unsafe.performIO
         -}


instance Functor (T a) where
   fmap f (Cons next start create delete) =
      Cons (\p s -> fmap (mapFst f) $ next p s) start create delete

map :: (forall r. a -> LLVM.CodeGenFunction r b) -> T inp a -> T inp b
map f (Cons next start create delete) =
   Cons (\p s -> FuncHT.mapFst f =<< next p s) start create delete



class Default a where
   type Element a
   deflt :: T a (Element a)


rmap :: (a -> b) -> T b c -> T a c
rmap f (Cons next start create delete) =
   Cons next start (create . f) delete

fanout :: T a b -> T a c -> T a (b,c)
fanout f g = rmap (\a -> Zip.Cons a a) $ split f g


instance (Default a, Default b) => Default (Zip.T a b) where
   type Element (Zip.T a b) = (Element a, Element b)
   deflt = split deflt deflt

split :: T a c -> T b d -> T (Zip.T a b) (c,d)
split (Cons nextA startA createA deleteA)
      (Cons nextB startB createB deleteB) = Cons
   (MultiValue.uncurry $ \parameterA parameterB (sa,sb) ->
      liftA2 zipPair (nextA parameterA sa) (nextB parameterB sb))
   (MultiValue.uncurry $ \parameterA parameterB ->
      liftA2 (,) (startA parameterA) (startB parameterB))
   (\(Zip.Cons a b) ->
      liftA2 zipPair (createA a) (createB b))
   (\(ca,cb) -> deleteA ca >> deleteB cb)


instance Default SigG.LazySize where
   type Element SigG.LazySize = ()
   deflt = lazySize

lazySize :: T SigG.LazySize ()
lazySize = ignore

ignore :: T a ()
ignore =
   Cons
      (\ _ unit -> return ((), unit))
      return
      (\ _a -> return ((), ()))
      (const $ return ())

instance (Storable.C a) => Default (SV.Vector a) where
   type Element (SV.Vector a) = MultiValue.T a
   deflt = storableVector

storableVector :: (Storable.C a) => T (SV.Vector a) (MultiValue.T a)
storableVector =
   Cons
      (\ _ (MultiValue.Cons p) ->
         liftA2 (,)
            (Storable.load p)
            (MultiValue.Cons <$> Storable.incrementPtr p))
      return
      (\vec ->
         let (fp,ptr,_l) = SVU.unsafeToPointers vec
         in  return (fp,ptr))
      -- keep the foreign ptr alive
      FPtr.touchForeignPtr


{-
This is intentionally restricted to NonNegW.Int aka StrictTimeShort,
since chunks must fit into memory.
If you have good reasons to allow other types,
see the versioning history for an according hack.
-}
instance
   (Marshal.C a, time ~ NonNegW.Int) =>
      Default (EventListBT.T time a) where
   type Element (EventListBT.T time a) = MultiValue.T a
   deflt = piecewiseConstant

{-
I would like to re-use code from ConstantPiece here.
Unfortunately, it is based on the LLVM-Maybe-Monad,
but here we do not accept early exit.
-}
piecewiseConstant ::
   (Marshal.C a) => T (EventListBT.T NonNegW.Int a) (MultiValue.T a)
piecewiseConstant =
   expandConstantPieces $
   rmap
      (SV.pack .
       List.map
         (\(a,t) -> EE.Stored $ LLVM.Struct
            (fromIntegral $ NonNegW.toNumber t :: Word, (Marshal.pack a, ()))) .
       EventListBT.toPairList) $
   map
      (\(MultiValue.Cons s) -> do
         t <- LLVM.extractvalue s TypeNum.d0
         a <- LLVM.extractvalue s TypeNum.d1
         Const.Cons t . MultiValue.Cons <$> Memory.decompose a) $
   storableVector

expandConstantPieces ::
   (Memory.C value) => T events (Const.T value) -> T events value
expandConstantPieces (Cons next start create delete) = Cons
   (\param state0 -> do
      (Const.Cons length1 y1, s1) <-
         C.whileLoopShared state0
            (\(Const.Cons len _y, s) ->
               (A.cmp LLVM.CmpEQ len Tuple.zero,
                next param s))
      length2 <- A.dec length1
      return (y1, (Const.Cons length2 y1, s1)))
   (\param -> (,) (Const.Cons Tuple.zero Tuple.undef) <$> start param)
   create delete


{- |
Return an Array and not a pointer to an array,
in order to forbid writing to the array.
-}
controllerSet ::
   (Marshal.C a, Marshal.Struct a ~ aStruct, LLVM.IsSized aStruct,
    TypeNum.Natural n,
    (n:*:LLVM.SizeOf aStruct) ~ arrSize, TypeNum.Natural arrSize) =>
   Proxy n -> T (PCS.T Int a) (MultiValue.T (MultiValue.Array n a))
controllerSet pn =
   controllerSetFromSV pn $
   map
      (\(MultiValue.Cons s) -> do
         len <- LLVM.extractvalue s TypeNum.d0
         i   <- LLVM.extractvalue s TypeNum.d1
         a   <- Memory.decompose =<< LLVM.extractvalue s TypeNum.d2
         return (len,(i,a))) $
   storableVector

controllerSetFromSV ::
   (Marshal.C a, Marshal.Struct a ~ aStruct, LLVM.IsSized aStruct,
    TypeNum.Natural n,
    (n:*:LLVM.SizeOf aStruct) ~ arrSize, TypeNum.Natural arrSize) =>
   Proxy n ->
   T (SV.Vector (EE.Stored (Marshal.Struct (Word,Word,a))))
     (LLVM.Value Word, (LLVM.Value Word, MultiValue.T a)) ->
   T (PCS.T Int a) (MultiValue.T (MultiValue.Array n a))
controllerSetFromSV pn (Cons next start create delete) = Cons
   (MultiValue.uncurry $ \(MultiValue.Cons (arrPtr, _)) param state0 -> do
      (length2, s2) <-
         C.whileLoopShared state0
            (\(len0, s0) ->
               (A.cmp LLVM.CmpEQ len0 Tuple.zero,
                do ((len1, (i,a)), s1) <- next param s0
                   Memory.store a =<< LLVM.getElementPtr arrPtr (i, ())
                   return (len1, s1)))
      length3 <- A.dec length2
      arr <- Memory.load =<< LLVM.bitcast arrPtr
      return (arr, (length3, s2)))
   (MultiValue.uncurry $ \(MultiValue.Cons (_, initialTime)) param -> do
      state <- start param
      return (initialTime, state))

   (\pcs ->
      EventListMT.switchTimeL
         (\initialTime bt -> do
            (context, param) <-
               create
                  (SV.pack .
                   List.map
                     (\((i,a),len) ->
                        EE.Stored $
                        Marshal.pack
                           (fromIntegral len :: Word,
                            fromIntegral i :: Word,
                            a)) .
                   EventListBT.toPairList $
                   bt)

            -- FIXME: handle memory exhaustion
            let n = TypeNum.integralFromProxy pn
            arr <- Array.mallocArray n
            flip mapM_ (Map.toList $ PCS.initial pcs) $ \(i,a) ->
               if i >= n
                 then error "Plug.Input.controllerSet: array too small"
                 else pokeElemOff arr i $ EE.Stored $ Marshal.pack a

            return
               ((arr, context),
                ((EE.castFromStoredPtr arr, fromIntegral initialTime :: Word),
                  param)))
            {-
            It would be more elegant,
            if we could pass Arrays around just like Vectors.

            return (context, ((sampleArray (\i -> maybe Tuple.undef Tuple.valueOf $ Map.lookup i (PCS.initial pcs)), time), param)))
            -}
         (EventListTT.flatten (PCS.stream pcs)))
   (\(arr, context) ->
      Alloc.free arr >> delete context)

{-
We might provide a plug that maps from a sequence of time-stamped controller events
to a stream of (Array Controller Value).
This way, we could select controllers more easily from within an causal arrow.
The disadvantage is, that MIDI controller numbers are then hard-wired into the arrow.
Instead we could use a stream of (Array Index Value)
and a global mapping (Array Controller (Maybe Index)).
This way would both save memory and make the controller numbers exchangeable.
We also have to cope with initialization of values
and have to assert that the exponential function
is computed only once per constant piece in controllerExponential.
-}
