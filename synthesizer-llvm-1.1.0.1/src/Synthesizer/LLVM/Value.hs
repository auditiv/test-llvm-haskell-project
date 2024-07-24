{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Synthesizer.LLVM.Value (
   T, decons,
   tau, square, sqrt,
   max, min, limit, fraction,

   (%==), (%/=), (%<), (%<=), (%>), (%>=), not,
   (%&&), (%||),
   (?), (??),

   lift0, lift1, lift2, lift3,
   unlift0, unlift1, unlift2, unlift3, unlift4, unlift5,
   constantValue, constant,
   fromInteger', fromRational',

   Flatten(flattenCode, unfoldCode), Registers,
   flatten, unfold,
   flattenCodeTraversable, unfoldCodeTraversable,
   flattenFunction,
   ) where

import LLVM.DSL.Value

import qualified Synthesizer.LLVM.Frame.Stereo as Stereo ()
import qualified Synthesizer.Basic.Phase as Phase

import qualified Algebra.RealRing as RealRing

import qualified Prelude as P ()
import NumericPrelude.Base hiding (min, max, unzip, unzip3, not)


instance (RealRing.C a, Flatten a) => Flatten (Phase.T a) where
   type Registers (Phase.T a) = Registers a
   flattenCode s = flattenCode $ Phase.toRepresentative s
   unfoldCode s =
      -- could also be unsafeFromRepresentative
      Phase.fromRepresentative $ unfoldCode s
