{-# LANGUAGE NoImplicitPrelude #-}
module Main where
-- module Synthesizer.LLVM.Server.SampledSoundAnalysis where

import qualified Synthesizer.LLVM.Server.Default as Default
import qualified Synthesizer.LLVM.Server.SampledSound as Sample

import Synthesizer.LLVM.Server.Common (Real)

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Synthesizer.Generic.Fourier as Fourier
import qualified Synthesizer.Generic.Signal  as SigG

import qualified Synthesizer.State.Signal         as SigS
import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified System.Path as Path

import qualified Data.Foldable as Fold
import Control.Functor.HT (void)
import Control.Monad (when)
import Data.Tuple.HT (snd3)
import Data.Monoid ((<>))
import Data.Ord.HT (comparing)

import qualified Number.Complex as Complex
import qualified Algebra.Field as Field
import qualified Algebra.Additive as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (id)
import Prelude ()

{-
I want to find the maximum of a peak with sub-sample precision.
Model: Lay parabola through maximum point and its left and right neighbors.

Parabola through three points: (-1,a), (0,b), (1,c):

Lagrange polynomial L_i:

f(t) = a*L_-1(t) + b*L_0(t) + c*L_1(t)
  = a*t*(t-1)/2 - b*(t+1)*(t-1) + c*t*(t+1)/2
  = a*(t^2-t)/2 + b*(1-t^2) + c*(t^2+t)/2

0 =!= f'(t)
  = a*L_-1'(t) + b*L_0'(t) + c*L_1'(t)
  = a*(2*t-1)/2 - b*2*t + c*(2*t+1)/2
0 = a*(2*t-1) - b*4*t + c*(2*t+1)
  = c-a + (2*a - 4*b + 2*c)*t
t = (a-c) / (2*a - 4*b + 2*c)
  = (c-a) / (2 * (2*b - a - c))

t is always between -0.5 and 0.5.
An SMT solver at least is convinced of it:

Prelude Data.SBV> prove $ \(a::SReal) (b::SReal) (c::SReal) -> a.<b &&& c.<b ==> abs ((a-c) / (a - 2*b + c)) .<= 1
Q.E.D.

Precondition: a>=0, b>=0, c>=0, a<b, c<b:

2*c  <  2*b
c  <  2*b - c
c-a  <  2*b - a - c

=> t<0.5

p = b-a
q = b-c

t = (p-q)/(2*(p+q))

Parabola maximum is invariant with respect to vertical shifts.

For non-negative data like in the absolute spectrum
this would not be a good model.
In this case we could model a peak with a Gaussian.
Essentially this means to apply the parabola model
to the logarithms of the non-negative values.

However, autocorrelation data can contain negative values.
-}
{- |
The three numbers must not be equal,
and the center value must be the largest one.
-}
peakMaximum :: (Field.C a) => (a,a,a) -> a
peakMaximum (a,b,c) =
   (c-a) / (2 * (2*b - a - c))


{-
weight autocorrelation coefficients
since the later ones are computed from less overlapped signal parts.

However, this emphasises later coefficients
and in one case the wrong maximum is chosen this way.
-}
weight :: Int -> SVL.Vector Real -> SVL.Vector Real
weight len =
   SigG.zipWithState (\w c -> c / fromIntegral w)
      (SigS.takeWhile (>0) $
       SigS.iterate (subtract 1) len)

autocorrelation :: SVL.Vector Real -> SVL.Vector Real
autocorrelation xs =
   SigSt.map Complex.real $
   Fourier.transformForward $
   SigSt.map (Complex.fromReal . Complex.magnitudeSqr) $
   Fourier.transformBackward $
   SigSt.map Complex.fromReal xs <> SigSt.map (const Additive.zero) xs

argMax :: (Ord a) => [a] -> Int
argMax =
   fst . Fold.maximumBy (comparing snd) .
   zip (iterate succ 0)

main :: IO ()
main =
   Fold.forM_ [Sample.tomatensalat, Sample.hal, Sample.graphentheorie] $
         \info -> do
      putStrLn $ Path.toString $ Sample.infoName info
      ranges <- Sample.loadRanges Default.sampleDirectory info
      Fold.forM_ ranges $ \smp ->
         let ignoreBeginning = 30
             body = snd3 $ Sample.parts smp
             ac =
                SVL.take (SVL.length body `div` 2) $
                -- weight (SVL.length body) $
                autocorrelation body
             maxi =
                (ignoreBeginning+) $ argMax $ SigSt.toList $
                SigSt.drop ignoreBeginning ac
             a:b:c:_ = SigSt.toList $ SigSt.drop (maxi-1) ac
         in  if SVL.length body < 1000
               then putStrLn "no loop"
               else do
                  when False $ print $ SVL.length body
                  when False $ print $ SVL.length ac
                  when False $ void $ Plot.plotDefault $
                     Plot2D.list Graph2D.listLines $ SigSt.toList ac
                  print $ fromIntegral maxi + peakMaximum (a,b,c)
