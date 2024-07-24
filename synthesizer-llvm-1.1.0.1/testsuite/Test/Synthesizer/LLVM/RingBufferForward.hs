{-# LANGUAGE NoImplicitPrelude #-}
module Test.Synthesizer.LLVM.RingBufferForward (tests) where

import qualified Synthesizer.LLVM.Causal.RingBufferForward as RingBuffer
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import Synthesizer.LLVM.Causal.Process (($*))

import qualified Data.StorableVector.Lazy as SVL

import qualified Test.Synthesizer.LLVM.Generator as Gen
import Test.Synthesizer.LLVM.Generator
         (Test, checkWithParam, arg, pair, triple, withGenArgs)
import Test.Synthesizer.LLVM.Utility
         (CheckEquality, CheckEquality2, checkEquality, checkEquality2,
          genRandomVectorParam, randomStorableVectorLoop)

import qualified Control.Arrow as Arrow
import Control.Arrow ((<<^))
import Control.Applicative ((<$>))

import qualified LLVM.DSL.Expression as Expr

import qualified LLVM.Extra.Multi.Value as MultiValue

import Foreign.Storable (Storable)

import qualified System.Random as Rnd
import Data.Word (Word, Word32)

import qualified Test.QuickCheck as QC

import NumericPrelude.Numeric
import NumericPrelude.Base


type EquFloat = CheckEquality Float

signalLength :: Int
signalLength = 10000


limitFloat :: (Storable a) => SVL.Vector a -> SVL.Vector a
limitFloat = SVL.take signalLength


trackId :: Test (Word, Word32) EquFloat
trackId =
   withGenArgs (pair (Gen.choose (1,1000)) Gen.arbitrary) $
     let noise seed = Sig.noise seed 1
     in checkEquality limitFloat
            (\(_bufferSize, seed) -> noise seed)
            (\(bufferSize, seed) ->
               RingBuffer.mapIndex zero
                  $* RingBuffer.track bufferSize (noise seed))

trackTail :: Test (Word, Word32) EquFloat
trackTail =
   withGenArgs (pair (Gen.choose (2,1000)) Gen.arbitrary) $
     let noise seed = Sig.noise seed 1
     in checkEquality limitFloat
            (\(_bufferSize, seed) -> Sig.tail $ noise seed)
            (\(bufferSize, seed) ->
               RingBuffer.mapIndex one
                  $* RingBuffer.track bufferSize (noise seed))

trackDrop :: Test (Word, Word32) EquFloat
trackDrop =
   withGenArgs (pair (Gen.choose (0,1000)) Gen.arbitrary) $
     let noise seed = Sig.noise seed 1
     in checkEquality limitFloat
          (\(n, seed) -> Sig.drop n $ noise seed)
          (\(n, seed) ->
             RingBuffer.mapIndex n $* RingBuffer.track (n+1) (noise seed))

randomSkips :: (Int, Rnd.StdGen) -> SVL.Vector Word
randomSkips = randomStorableVectorLoop (0,10)

trackSkip :: Test ((Int, Rnd.StdGen), Word32) EquFloat
trackSkip =
   withGenArgs (pair (arg genRandomVectorParam) Gen.arbitrary) $
   let noise seed = Sig.noise seed 1
   in (\f chunkSize (sk, seed) -> f chunkSize (randomSkips sk, seed))
      <$>
      checkEquality limitFloat
         (\(skips, seed) -> Causal.skip (noise seed) $* skips)
         (\(skips, seed) ->
            RingBuffer.mapIndex one
               $* (RingBuffer.trackSkip 1 (noise seed) $* skips))

trackSkip1 :: Test (Word, Word32) EquFloat
trackSkip1 =
   let bufferSize = 1000
   in  withGenArgs
         (pair
            (Gen.choose (0, fromIntegral bufferSize - 1))
            Gen.arbitrary) $

            let noise seed = Sig.noise seed 1
            in  checkEquality limitFloat
                  (\(k, seed) ->
                     RingBuffer.mapIndex k $*
                     RingBuffer.track (Expr.cons bufferSize) (noise seed))
                  (\(k, seed) ->
                     RingBuffer.mapIndex k $*
                     (RingBuffer.trackSkip (Expr.cons bufferSize) (noise seed)
                        $* 1))

trackSkipHold ::
   Test ((Int, Rnd.StdGen), Word, Word32) (CheckEquality2 Bool Float)
trackSkipHold =
   let bufferSize = 1000
   in  withGenArgs
         (triple
            (arg genRandomVectorParam)
            (Gen.choose (0, fromIntegral bufferSize - 1))
            Gen.arbitrary) $

            let noise seed = Sig.noise seed 1
            in (\f chunkSize (sk, k, seed) ->
                  f chunkSize (randomSkips sk, k, seed))
               <$>
               checkEquality2 limitFloat limitFloat
                  (\(skips, k, seed) ->
                   (,) (MultiValue.cons True) <$>
                   (RingBuffer.mapIndex k $*
                    (RingBuffer.trackSkip (Expr.cons bufferSize) (noise seed)
                        $* skips)))
                  (\(skips, k, seed) ->
                   (Arrow.second (RingBuffer.mapIndex k)
                        <<^ (\((b,_s),buf) -> (b,buf)))
                   $*
                   (RingBuffer.trackSkipHold (Expr.cons bufferSize) (noise seed)
                        $* skips))

{-
To do:

test that trackSkipHold returns False forever after it has returned False once.
-}


tests :: [(String, IO QC.Property)]
tests =
   ("trackId", checkWithParam trackId) :
   ("trackTail", checkWithParam trackTail) :
   ("trackDrop", checkWithParam trackDrop) :
   ("trackSkip", checkWithParam trackSkip) :
   ("trackSkip1", checkWithParam trackSkip1) :
   ("trackSkipHold", checkWithParam trackSkipHold) :
   []
