{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Synthesizer.LLVM.Causal.Functional (
   T,
   lift, fromSignal,
   ($&), (&|&),
   compile,
   compileSignal,
   withArgs, MakeArguments, Arguments, makeArgs,
   AnyArg(..),

   Ground(Ground),
   withGroundArgs, MakeGroundArguments, GroundArguments,
   makeGroundArgs,

   Atom(..), atom,
   withGuidedArgs, MakeGuidedArguments, GuidedArguments, PatternArguments,
   makeGuidedArgs,

   PrepareArguments(PrepareArguments), withPreparedArgs, withPreparedArgs2,
   atomArg, stereoArgs, pairArgs, tripleArgs,
   ) where

import qualified Synthesizer.LLVM.Causal.Private as CausalCore
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Signal
import qualified Synthesizer.LLVM.Frame.SerialVector.Class as Serial
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.Causal.Class as CausalClass
import Synthesizer.LLVM.Private (getPairPtrs, noLocalPtr)

import qualified LLVM.Extra.Multi.Value as MultiValue
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Memory as Memory
import qualified LLVM.Extra.Arithmetic as A

import LLVM.Core (CodeGenFunction)
import qualified LLVM.Core as LLVM

import qualified Number.Ratio as Ratio
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.State (StateT)

import qualified Data.Vault.Lazy as Vault
import Data.Vault.Lazy (Vault)
import qualified Control.Category as Cat
import Control.Arrow (Arrow, (>>^), (&&&), arr, first)
import Control.Category (Category, (.))
import Control.Applicative (Applicative, (<*>), pure, liftA2)

import Data.Tuple.Strict (zipPair)
import Data.Tuple.HT (fst3, snd3, thd3)

import qualified System.Unsafe as Unsafe

import Prelude hiding ((.))


newtype T inp out = Cons (Code inp out)


-- | similar to @Causal.T a b@
data Code a b =
   forall global local state.
      (Memory.C global, LLVM.IsSized local, Memory.C state) =>
      Code (forall r c.
            (Tuple.Phi c) =>
            global -> LLVM.Value (LLVM.Ptr local) -> a -> state ->
            StateT Vault (MaybeCont.T r c) (b, state))
               -- compute next value
           (forall r. CodeGenFunction r (global, state))
               -- initial state
           (forall r. global -> CodeGenFunction r ())
               -- cleanup


instance Category Code where
   id = arr id
   Code nextB startB stopB . Code nextA startA stopA = Code
      (\(globalA, globalB) local a (sa0,sb0) -> do
         (localA,localB) <- MT.lift $ getPairPtrs local
         (b,sa1) <- nextA globalA localA a sa0
         (c,sb1) <- nextB globalB localB b sb0
         return (c, (sa1,sb1)))
      (liftA2 zipPair startA startB)
      (\(globalA, globalB) -> stopA globalA >> stopB globalB)


instance Arrow Code where
   arr f = Code
      (\() -> noLocalPtr $ \a () -> return (f a, ()))
      (return ((),()))
      (\() -> return ())
   first (Code next start stop) = Code (CausalCore.firstNext next) start stop


{-
We must not define Category and Arrow instances
because in osci***osci the result of osci would be shared,
although it depends on the particular input.

instance Category T where
   id = tagUnique Cat.id
   Cons a . Cons b = tagUnique (a . b)

instance Arrow T where
   arr f = tagUnique $ arr f
   first (Cons a) = tagUnique $ first a
-}

instance Functor (T inp) where
   fmap f (Cons x) =
      tagUnique $ x >>^ f

instance Applicative (T inp) where
   pure a = tagUnique $ arr (const a)
   f <*> x = fmap (uncurry ($))  $  f &|& x


lift0 :: (forall r. CodeGenFunction r out) -> T inp out
lift0 f = lift (CausalCore.map (const f))

lift1 :: (forall r. a -> CodeGenFunction r out) -> T inp a -> T inp out
lift1 f x = CausalCore.map f $& x

lift2 ::
   (forall r. a -> b -> CodeGenFunction r out) ->
   T inp a -> T inp b -> T inp out
lift2 f x y = CausalCore.zipWith f $& x&|&y


instance (A.PseudoRing b, A.Real b, A.IntegerConstant b) => Num (T a b) where
   fromInteger n = pure (A.fromInteger' n)
   (+) = lift2 A.add
   (-) = lift2 A.sub
   (*) = lift2 A.mul
   abs = lift1 A.abs
   signum = lift1 A.signum

instance (A.Field b, A.Real b, A.RationalConstant b) => Fractional (T a b) where
   fromRational x = pure (A.fromRational' x)
   (/) = lift2 A.fdiv


instance (A.Additive b) => Additive.C (T a b) where
   zero = pure A.zero
   (+) = lift2 A.add
   (-) = lift2 A.sub
   negate = lift1 A.neg

instance (A.PseudoRing b, A.IntegerConstant b) => Ring.C (T a b) where
   one = pure A.one
   fromInteger n = pure (A.fromInteger' n)
   (*) = lift2 A.mul

instance (A.Field b, A.RationalConstant b) => Field.C (T a b) where
   fromRational' x = pure (A.fromRational' $ Ratio.toRational98 x)
   (/) = lift2 A.fdiv

instance (A.Transcendental b, A.RationalConstant b) => Algebraic.C (T a b) where
   sqrt = lift1 A.sqrt
   root n x = lift2 A.pow x (Field.recip $ Ring.fromInteger n)
   x^/r = lift2 A.pow x (Field.fromRational' r)

instance (A.Transcendental b, A.RationalConstant b) => Trans.C (T a b) where
   pi = lift0 A.pi
   sin = lift1 A.sin
   cos = lift1 A.cos
   (**) = lift2 A.pow
   exp = lift1 A.exp
   log = lift1 A.log

   asin _ = error "LLVM missing intrinsic: asin"
   acos _ = error "LLVM missing intrinsic: acos"
   atan _ = error "LLVM missing intrinsic: atan"


infixr 0 $&

($&) :: Causal.T b c -> T a b -> T a c
f $& (Cons b) =
   tagUnique $  liftCode f . b


infixr 3 &|&

(&|&) :: T a b -> T a c -> T a (b,c)
Cons b &|& Cons c =
   tagUnique $  b &&& c


liftCode :: Causal.T inp out -> Code inp out
liftCode (CausalCore.Cons next start stop) =
   Code
      (\p l a state -> MT.lift (next p l a state))
      start stop

lift :: Causal.T inp out -> T inp out
lift = tagUnique . liftCode

fromSignal :: Signal.T out -> T inp out
fromSignal = lift . CausalClass.fromSignal

tag :: Vault.Key out -> Code inp out -> T inp out
tag key (Code next start stop) =
   Cons $
   Code
      (\p l a s0 -> do
         mb <- State.gets (Vault.lookup key)
         case mb of
            Just b -> return (b,s0)
            Nothing -> do
               bs@(b,_) <- next p l a s0
               State.modify (Vault.insert key b)
               return bs)
      start stop

-- dummy for debugging
_tag :: Vault.Key out -> Code inp out -> T inp out
_tag _ = Cons

tagUnique :: Code inp out -> T inp out
tagUnique code =
   Unsafe.performIO $
   fmap (flip tag code) Vault.newKey

initialize :: Code inp out -> Causal.T inp out
initialize (Code next start stop) =
   CausalCore.Cons
      (\p l a state -> State.evalStateT (next p l a state) Vault.empty)
      start stop

compile :: T inp out -> Causal.T inp out
compile (Cons code) = initialize code

compileSignal :: T () out -> Signal.T out
compileSignal f = CausalClass.toSignal $ compile f


{- |
Using 'withArgs' you can simplify

> let x = F.lift (arr fst)
>     y = F.lift (arr (fst.snd))
>     z = F.lift (arr (snd.snd))
> in  F.compile (f x y z)

to

> withArgs $ \(x,(y,z)) -> f x y z
-}
withArgs ::
   (MakeArguments inp) =>
   (Arguments (T inp) inp -> T inp out) -> Causal.T inp out
withArgs f = withId $ f . makeArgs

withId :: (T inp inp -> T inp out) -> Causal.T inp out
withId f = compile $ f $ lift Cat.id


type family Arguments (f :: * -> *) arg

class MakeArguments arg where
   makeArgs :: Functor f => f arg -> Arguments f arg


{-
I have thought about an Arg type, that marks where to stop descending.
This way we can throw away all of these FlexibleContext instances
and the user can freely choose the granularity of arguments.
However this does not work so easily,
because we would need a functional depedency from, say,
@(Arg a, Arg b)@ to @(a,b)@.
This is the opposite direction to the dependency we use currently.
The 'AnyArg' type provides a solution in this spirit.
-}
type instance Arguments f (LLVM.Value a) = f (LLVM.Value a)
instance MakeArguments (LLVM.Value a) where
   makeArgs = id

type instance Arguments f (MultiValue.T a) = f (MultiValue.T a)
instance MakeArguments (MultiValue.T a) where
   makeArgs = id

{- |
Consistent with pair instance.
You may use 'AnyArg' or 'withGuidedArgs'
to stop descending into the stereo channels.
-}
type instance Arguments f (Stereo.T a) = Stereo.T (Arguments f a)
instance (MakeArguments a) => MakeArguments (Stereo.T a) where
   makeArgs = fmap makeArgs . Stereo.sequence

type instance Arguments f (Serial.Constant n a) = f (Serial.Constant n a)
instance MakeArguments (Serial.Constant n a) where
   makeArgs = id

type instance Arguments f () = f ()
instance MakeArguments () where
   makeArgs = id

type instance Arguments f (a,b) = (Arguments f a, Arguments f b)
instance (MakeArguments a, MakeArguments b) =>
      MakeArguments (a,b) where
   makeArgs f = (makeArgs $ fmap fst f, makeArgs $ fmap snd f)

type instance Arguments f (a,b,c) =
                  (Arguments f a, Arguments f b, Arguments f c)
instance (MakeArguments a, MakeArguments b, MakeArguments c) =>
      MakeArguments (a,b,c) where
   makeArgs f =
      (makeArgs $ fmap fst3 f, makeArgs $ fmap snd3 f, makeArgs $ fmap thd3 f)


{- |
You can use this to explicitly stop breaking of composed data types.
It might be more comfortable to do this using 'withGuidedArgs'.
-}
newtype AnyArg a = AnyArg {getAnyArg :: a}

type instance Arguments f (AnyArg a) = f a
instance MakeArguments (AnyArg a) where
   makeArgs = fmap getAnyArg



{- |
This is similar to 'withArgs'
but it requires to specify the decomposition depth
using constructors in the arguments.
-}
withGroundArgs ::
   (MakeGroundArguments (T inp) args,
    GroundArguments args ~ inp) =>
   (args -> T inp out) -> Causal.T inp out
withGroundArgs f = withId $ f . makeGroundArgs


newtype Ground f a = Ground (f a)


type family GroundArguments args

class (Functor f) => MakeGroundArguments f args where
   makeGroundArgs :: f (GroundArguments args) -> args


type instance GroundArguments (Ground f a) = a
instance (Functor f, f ~ g) => MakeGroundArguments f (Ground g a) where
   makeGroundArgs = Ground

type instance GroundArguments (Stereo.T a) = Stereo.T (GroundArguments a)
instance MakeGroundArguments f a => MakeGroundArguments f (Stereo.T a) where
   makeGroundArgs f =
      Stereo.cons
         (makeGroundArgs $ fmap Stereo.left f)
         (makeGroundArgs $ fmap Stereo.right f)

type instance GroundArguments () = ()
instance (Functor f) => MakeGroundArguments f () where
   makeGroundArgs _ = ()


type instance
   GroundArguments (a,b) =
      (GroundArguments a, GroundArguments b)
instance
   (MakeGroundArguments f a, MakeGroundArguments f b) =>
      MakeGroundArguments f (a,b) where
   makeGroundArgs f =
      (makeGroundArgs $ fmap fst f,
       makeGroundArgs $ fmap snd f)

type instance
   GroundArguments (a,b,c) =
      (GroundArguments a, GroundArguments b, GroundArguments c)
instance
   (MakeGroundArguments f a, MakeGroundArguments f b,
    MakeGroundArguments f c) =>
      MakeGroundArguments f (a,b,c) where
   makeGroundArgs f =
      (makeGroundArgs $ fmap fst3 f,
       makeGroundArgs $ fmap snd3 f,
       makeGroundArgs $ fmap thd3 f)



{- |
This is similar to 'withArgs'
but it allows to specify the decomposition depth using a pattern.
-}
withGuidedArgs ::
   (MakeGuidedArguments pat, PatternArguments pat ~ inp) =>
   pat ->
   (GuidedArguments (T inp) pat -> T inp out) -> Causal.T inp out
withGuidedArgs p f = withId $ f . makeGuidedArgs p


data Atom a = Atom

atom :: Atom a
atom = Atom


type family GuidedArguments (f :: * -> *) pat
type family PatternArguments pat

class MakeGuidedArguments pat where
   makeGuidedArgs ::
      Functor f =>
      pat -> f (PatternArguments pat) -> GuidedArguments f pat


type instance GuidedArguments f (Atom a) = f a
type instance PatternArguments (Atom a) = a
instance MakeGuidedArguments (Atom a) where
   makeGuidedArgs Atom = id

type instance GuidedArguments f (Stereo.T a) = Stereo.T (GuidedArguments f a)
type instance PatternArguments (Stereo.T a) = Stereo.T (PatternArguments a)
instance MakeGuidedArguments a => MakeGuidedArguments (Stereo.T a) where
   makeGuidedArgs pat f =
      Stereo.cons
         (makeGuidedArgs (Stereo.left  pat) $ fmap Stereo.left f)
         (makeGuidedArgs (Stereo.right pat) $ fmap Stereo.right f)

type instance GuidedArguments f () = f ()
type instance PatternArguments () = ()
instance MakeGuidedArguments () where
   makeGuidedArgs () = id

type instance
   GuidedArguments f (a,b) =
      (GuidedArguments f a, GuidedArguments f b)
type instance
   PatternArguments (a,b) =
      (PatternArguments a, PatternArguments b)
instance (MakeGuidedArguments a, MakeGuidedArguments b) =>
      MakeGuidedArguments (a,b) where
   makeGuidedArgs (pa,pb) f =
      (makeGuidedArgs pa $ fmap fst f,
       makeGuidedArgs pb $ fmap snd f)

type instance
   GuidedArguments f (a,b,c) =
      (GuidedArguments f a, GuidedArguments f b, GuidedArguments f c)
type instance
   PatternArguments (a,b,c) =
      (PatternArguments a, PatternArguments b, PatternArguments c)
instance
   (MakeGuidedArguments a, MakeGuidedArguments b, MakeGuidedArguments c) =>
      MakeGuidedArguments (a,b,c) where
   makeGuidedArgs (pa,pb,pc) f =
      (makeGuidedArgs pa $ fmap fst3 f,
       makeGuidedArgs pb $ fmap snd3 f,
       makeGuidedArgs pc $ fmap thd3 f)



{- |
Alternative to withGuidedArgs.
This way of pattern construction is even Haskell 98.
-}
withPreparedArgs ::
   PrepareArguments (T inp) inp a ->
   (a -> T inp out) -> Causal.T inp out
withPreparedArgs (PrepareArguments prepare) f = withId $ f . prepare

withPreparedArgs2 ::
   PrepareArguments (T (inp0, inp1)) inp0 a ->
   PrepareArguments (T (inp0, inp1)) inp1 b ->
   (a -> b -> T (inp0, inp1) out) ->
   Causal.T (inp0, inp1) out
withPreparedArgs2 prepareA prepareB f =
   withPreparedArgs (pairArgs prepareA prepareB) (uncurry f)

newtype PrepareArguments f merged separated =
   PrepareArguments (f merged -> separated)

atomArg :: PrepareArguments f a (f a)
atomArg = PrepareArguments id

stereoArgs ::
   (Functor f) =>
   PrepareArguments f a b ->
   PrepareArguments f (Stereo.T a) (Stereo.T b)
stereoArgs (PrepareArguments p) =
   PrepareArguments $ fmap p . Stereo.sequence

pairArgs ::
   (Functor f) =>
   PrepareArguments f a0 b0 ->
   PrepareArguments f a1 b1 ->
   PrepareArguments f (a0,a1) (b0,b1)
pairArgs (PrepareArguments p0) (PrepareArguments p1) =
   PrepareArguments $ \f -> (p0 $ fmap fst f, p1 $ fmap snd f)

tripleArgs ::
   (Functor f) =>
   PrepareArguments f a0 b0 ->
   PrepareArguments f a1 b1 ->
   PrepareArguments f a2 b2 ->
   PrepareArguments f (a0,a1,a2) (b0,b1,b2)
tripleArgs (PrepareArguments p0) (PrepareArguments p1) (PrepareArguments p2) =
   PrepareArguments $ \f ->
      (p0 $ fmap fst3 f, p1 $ fmap snd3 f, p2 $ fmap thd3 f)
