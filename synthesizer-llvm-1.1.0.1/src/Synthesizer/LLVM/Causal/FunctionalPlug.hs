{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.LLVM.Causal.FunctionalPlug (
   T,
   ($&), (&|&),
   run, runPlugOut,
   fromSignal, plug, askParameter, Input,
   withArgs, withArgsPlugOut,
   MakeArguments, Arguments, makeArgs,
   ) where

import qualified Synthesizer.LLVM.Plug.Input as PIn
import qualified Synthesizer.LLVM.Plug.Output as POut

import qualified Synthesizer.LLVM.Causal.Parametric as Parametric
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Private as CausalPriv
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Sig

import qualified Synthesizer.Causal.Class as CausalClass
import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Zip as Zip

import qualified Data.EventList.Relative.BodyTime as EventListBT
import qualified Data.StorableVector as SV

import LLVM.DSL.Expression (Exp(Exp))

import qualified LLVM.Extra.Multi.Value.Marshal as Marshal
import qualified LLVM.Extra.MaybeContinuation as MaybeCont
import qualified LLVM.Extra.Arithmetic as A
import LLVM.Core (CodeGenFunction)

import Data.IORef (newIORef, readIORef)

import qualified Number.Ratio as Ratio
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.State as MS
import Control.Monad.IO.Class (liftIO)

import qualified Data.Set as Set
import qualified Data.Vault.Lazy as Vault
import Data.Vault.Lazy (Vault)
import Data.Unique (Unique, newUnique)
import Data.Maybe (fromMaybe)

import Control.Arrow ((^<<), (<<^), arr, first, second)
import Control.Category (id, (.))
import Control.Applicative (Applicative, (<*>), pure, liftA2, liftA3)

import qualified System.Unsafe as Unsafe

import Prelude hiding (id, (.))


{- |
This data type detects sharing.
-}
{-
There are two levels of the use of keys.
At the top level, in T's State monad,
we store an object id in order to check,
whether we have already seen a certain object.
If we encounter a known object
then we use the Simple constructor
and fetch the stored CausalP output
within the causal process enclosed in Simple.
This and the causal process in the Plugged constructor
are the second level.
These arrows handle a Vault like a state monad
and insert all values they produce into the Vault.
-}
newtype T pp inp out =
   Cons (MS.State (Set.Set Unique) (Core pp inp out))

{-
We need to hide the x and y types
since these types grow when combining Cores,
and then we could not define numeric instances.
-}
data Core pp inp out =
   forall x y. CutG.Read x =>
   Plugged
      (pp -> inp -> x)
      (PIn.T x y)
      (Causal.T (y, Vault) (out, Vault))
   |
   {-
   The Simple constructor is needed for reusing shared CausalP processes
   and for input without external representation. (a Plug.Input)
   -}
   Simple (Causal.T Vault (out, Vault))


applyCore ::
   Causal.T (a, Vault) (b, Vault) ->
   Core pp inp a ->
   Core pp inp b
applyCore f core =
   case core of
      Plugged prep plg process -> Plugged prep plg (f . process)
      Simple process -> Simple (f . process)

combineCore ::
   Core pp inp a ->
   Core pp inp b ->
   Core pp inp (a,b)
combineCore (Plugged prepA plugA processA) (Plugged prepB plugB processB) =
   Plugged
      (\p -> Zip.arrowFanout (prepA p) (prepB p))
      (PIn.split plugA plugB)
      ((\(a,(b,v)) -> ((a,b), v)) ^<< second processB
       . arr (\((a,v),b) -> (a,(b,v))) .
       first processA <<^ (\((a,b),v) -> ((a,v),b)))
combineCore (Simple processA) (Plugged prepB plugB processB) =
   Plugged prepB plugB
      ((\(b,(a,v)) -> ((a,b), v)) ^<< second processA . processB)
combineCore (Plugged prepA plugA processA) (Simple processB) =
   Plugged prepA plugA
      ((\(a,(b,v)) -> ((a,b), v)) ^<< second processB . processA)
combineCore (Simple processA) (Simple processB) =
   Simple ((\(a,(b,v)) -> ((a,b), v)) ^<< second processB . processA)


reuseCore :: Vault.Key out -> Core pp inp out
reuseCore key =
   Simple $ arr $ \vault ->
      (fromMaybe (error "key must have been lost") $ Vault.lookup key vault,
       vault)


tag ::
   Unique -> Vault.Key out ->
   MS.State (Set.Set Unique) (Core pp inp out) ->
   T pp inp out
tag unique key stateCore = Cons $ do
   alreadySeen <- MS.gets (Set.member unique)
   if alreadySeen
      then return $ reuseCore key
      else do
         MS.modify (Set.insert unique)
         fmap (applyCore (arr $ \(a,v) -> (a, Vault.insert key a v))) stateCore

tagUnique ::
   MS.State (Set.Set Unique) (Core pp inp out) ->
   T pp inp out
tagUnique core =
   Unsafe.performIO $
   liftA3 tag newUnique Vault.newKey (pure core)


infixr 0 $&

($&) ::
   Causal.T a b ->
   T pp inp a ->
   T pp inp b
f  $&  Cons core =
   tagUnique $ fmap (applyCore $ first f) core


infixr 3 &|&

(&|&) ::
   T pp inp a ->
   T pp inp b ->
   T pp inp (a,b)
Cons coreA  &|&  Cons coreB =
   tagUnique $ liftA2 combineCore coreA coreB


instance Functor (Core pp inp) where
   fmap f (Simple process) = Simple (fmap (first f) process)
   fmap f (Plugged prep plg process) = Plugged prep plg (fmap (first f) process)

instance Applicative (Core pp inp) where
   pure a = lift0Core $ pure a
   f <*> x = fmap (uncurry ($))  $  combineCore f x

lift0Core :: (forall r. CodeGenFunction r out) -> Core pp inp out
lift0Core f = Simple (CausalPriv.map (\v -> fmap (flip (,) v) f))


instance Functor (T pp inp) where
   fmap f (Cons x) = tagUnique $ fmap (fmap f) x

instance Applicative (T pp inp) where
   pure a = tagUnique $ pure $ pure a
   f <*> x = fmap (uncurry ($))  $  f &|& x


lift0 :: (forall r. CodeGenFunction r out) -> T pp inp out
lift0 f = tagUnique $ pure $ lift0Core f

lift1 ::
   (forall r. a -> CodeGenFunction r out) ->
   T pp inp a -> T pp inp out
lift1 f x = CausalPriv.map f $& x

lift2 ::
   (forall r. a -> b -> CodeGenFunction r out) ->
   T pp inp a -> T pp inp b -> T pp inp out
lift2 f x y = CausalPriv.zipWith f $& x&|&y


instance
   (A.PseudoRing b, A.Real b, A.IntegerConstant b) =>
      Num (T pp a b) where
   fromInteger n = pure (A.fromInteger' n)
   (+) = lift2 A.add
   (-) = lift2 A.sub
   (*) = lift2 A.mul
   abs = lift1 A.abs
   signum = lift1 A.signum

instance
   (A.Field b, A.Real b, A.RationalConstant b) =>
      Fractional (T pp a b) where
   fromRational x = pure (A.fromRational' x)
   (/) = lift2 A.fdiv


instance (A.Additive b) => Additive.C (T pp a b) where
   zero = pure A.zero
   (+) = lift2 A.add
   (-) = lift2 A.sub
   negate = lift1 A.neg

instance (A.PseudoRing b, A.IntegerConstant b) => Ring.C (T pp a b) where
   one = pure A.one
   fromInteger n = pure (A.fromInteger' n)
   (*) = lift2 A.mul

instance (A.Field b, A.RationalConstant b) => Field.C (T pp a b) where
   fromRational' x = pure (A.fromRational' $ Ratio.toRational98 x)
   (/) = lift2 A.fdiv

instance
   (A.Transcendental b, A.RationalConstant b) =>
      Algebraic.C (T pp a b) where
   sqrt = lift1 A.sqrt
   root n x = lift2 A.pow x (Field.recip $ Ring.fromInteger n)
   x^/r = lift2 A.pow x (Field.fromRational' r)

instance
   (A.Transcendental b, A.RationalConstant b) =>
      Trans.C (T pp a b) where
   pi = lift0 A.pi
   sin = lift1 A.sin
   cos = lift1 A.cos
   (**) = lift2 A.pow
   exp = lift1 A.exp
   log = lift1 A.log

   asin _ = error "LLVM missing intrinsic: asin"
   acos _ = error "LLVM missing intrinsic: acos"
   atan _ = error "LLVM missing intrinsic: atan"



fromSignal :: Sig.T a -> T pp inp a
fromSignal sig =
   tagUnique $ pure $ Simple (CausalClass.feedFst sig)



type Input pp a = MR.Reader (pp, a)

plug ::
   (CutG.Read b, PIn.Default b) =>
   Input pp a b ->
   T pp a (PIn.Element b)
plug accessor =
   tagUnique $ pure $
   Plugged
      (curry $ MR.runReader accessor)
      PIn.deflt
      id

askParameter :: Input pp a pp
askParameter = MR.asks fst


runPlugOut ::
   (Marshal.C pl) =>
   (Exp pl -> T pp a x) -> POut.T x b ->
   IO (pp -> pl -> PIO.T a b)
runPlugOut func pout = do
   let name = "FunctionalPlug.runPlugOut"
   ref <- newIORef $ error $ name ++ ": uninitialized parameter reference"
   case func (Exp (liftIO (readIORef ref))) of
      Cons core ->
         case MS.evalState core Set.empty of
            Simple _ -> error $ name ++ ": no substantial input available"
               -- Simple process ->
               --    CausalRender.processIOCore pin process pout
            Plugged prep pin process ->
               fmap (\f pp pl -> f (return (pl, return ())) <<^ prep pp) $
               case fst ^<< process <<^ flip (,) Vault.empty of
                  CausalPriv.Cons next start stop ->
                     (\paramd ->
                        CausalRender.processIOParametric pin paramd pout) $
                     Parametric.Cons
                        (\p global local a state ->
                           MaybeCont.lift (Parametric.loadParam ref p) >>
                           next global local a state)
                        (\p ->
                           Parametric.loadParam ref p >> start)
                        (\p global ->
                           Parametric.loadParam ref p >> stop global)

run ::
   (Marshal.C pl) =>
   (POut.Default b) =>
   (Exp pl -> T pp a (POut.Element b)) ->
   IO (pp -> pl -> PIO.T a b)
run f = runPlugOut f POut.deflt


{- |
Cf. 'F.withArgs'.
-}
withArgs ::
   (Marshal.C pl) =>
   (MakeArguments a, POut.Default b) =>
   (Arguments (Input pp a) a -> Exp pl -> T pp a (POut.Element b)) ->
   IO (pp -> pl -> PIO.T a b)
withArgs f = withArgsPlugOut f POut.deflt

withArgsPlugOut ::
   (Marshal.C pl) =>
   (MakeArguments a) =>
   (Arguments (Input pp a) a -> Exp pl -> T pp a x) ->
   POut.T x b ->
   IO (pp -> pl -> PIO.T a b)
withArgsPlugOut = withArgsPlugOutStart (MR.asks snd)

withArgsPlugOutStart ::
   (Marshal.C pl) =>
   (MakeArguments a) =>
   Input pp a a ->
   (Arguments (Input pp a) a -> Exp pl -> T pp a x) ->
   POut.T x b ->
   IO (pp -> pl -> PIO.T a b)
withArgsPlugOutStart fid f = runPlugOut (f (makeArgs fid))



type family Arguments (f :: * -> *) arg

class MakeArguments arg where
   makeArgs :: Functor f => f arg -> Arguments f arg


type instance Arguments f (EventListBT.T i a) = f (EventListBT.T i a)
instance MakeArguments (EventListBT.T i a) where
   makeArgs = id

type instance Arguments f (SV.Vector a) = f (SV.Vector a)
instance MakeArguments (SV.Vector a) where
   makeArgs = id

type instance Arguments f (Zip.T a b) = (Arguments f a, Arguments f b)
instance (MakeArguments a, MakeArguments b) =>
      MakeArguments (Zip.T a b) where
   makeArgs f = (makeArgs $ fmap Zip.first f, makeArgs $ fmap Zip.second f)
