{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Synthesizer.LLVM.Server.Common (Real, pioApply)

import qualified Synthesizer.LLVM.Server.SampledSound as Sample
import qualified Sound.Sox.Write as SoxWrite

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.WXT as WXT
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Synthesizer.LLVM.Causal.Controlled as Ctrl
import qualified Synthesizer.LLVM.Causal.Render as CausalRender
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Render as Render
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Filter.Universal as UniFilterL
import qualified Synthesizer.LLVM.Filter.NonRecursive as FiltNR
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filt1
import Synthesizer.LLVM.Causal.Process (($*), ($<))

import qualified LLVM.DSL.Expression as Expr

import qualified Synthesizer.Plain.Filter.Recursive.Universal as UniFilter
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as FirstOrder
import Synthesizer.Plain.Filter.Recursive (Pole(Pole))

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Generic.Fourier as Fourier
import qualified Synthesizer.Generic.Analysis as Analysis
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Piece as Piece
import qualified Synthesizer.Causal.Filter.NonRecursive as FiltNRC
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.State.Signal as SigS
import Synthesizer.Piecewise ((#|-), (-|#), (#|), (|#))

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import Control.Arrow ((<<<), (^<<))
import Control.Category ((.))
import Control.Applicative ((<$>))

import Control.Functor.HT (void)

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Foldable (forM_)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (catMaybes)
import Data.Tuple.HT (mapSnd)
import Data.Ord.HT (comparing)
import Data.Semigroup ((<>))
import Data.Monoid (mempty)
import Data.Word (Word)

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path
import System.Path ((</>), (<.>))

import qualified Number.Complex as Complex

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (id, (.))


sampleRateInt :: Int
sampleRateInt = 44100

sampleRate :: Real
sampleRate = fromIntegral sampleRateInt

spectrum :: SVL.Vector Real -> SVL.Vector Real
spectrum xs =
   SVL.map Complex.magnitude $
   SVL.take (div (SVL.length xs) 2) $
   Fourier.transformBackward $
   SVL.map Complex.fromReal xs

timeDomain :: SVL.Vector Real -> SVL.Vector (Complex.T Real)
timeDomain xs =
   Fourier.transformForward $
   SVL.append
      (SVL.map Complex.fromReal xs)
      (SVL.replicate SVL.defaultChunkSize (SVL.length xs) 0)

chop :: Int -> SVL.Vector Real -> [SVL.Vector Real]
chop n =
   map (SVL.take n) .
   takeWhile (not . SVL.null) .
   iterate (SVL.drop n)

spectrumPlot :: SVL.Vector Real -> Plot2D.T Real Real
spectrumPlot xs =
   let k = sampleRate / fromIntegral (SVL.length xs)
       step = 16
       avg chunk = SVL.foldl (+) zero chunk / fromIntegral step
   in  Plot2D.list Graph2D.lines $
       zip (iterate (fromIntegral step * k +) 0) $
       map avg $ chop step $
       spectrum xs

plotSpectrum :: SVL.Vector Real -> IO ()
plotSpectrum xs =
   void $
   Plot.plot WXT.cons $
   spectrumPlot xs

saveSound :: (PathClass.AbsRel ar) => Path.File ar -> SVL.Vector Real -> IO ()
saveSound path xs =
   void $ SoxWrite.simple SVL.hPut mempty (Path.toString path) sampleRateInt xs

tmpWave :: String -> Path.AbsFile
tmpWave name = Path.absDir "/tmp" </> Path.relFile name <.> "wav"

phonemeWave :: String -> Path.RelFile
phonemeWave name = Path.relDir "phoneme" </> Path.relFile name <.> "wav"

maskWave :: String -> Path.RelFile
maskWave name = Path.relDir "mask" </> Path.relFile name <.> "wav"

loadPhoneme :: String -> IO (SVL.Vector Real)
loadPhoneme name = do
   putStrLn name
   Sample.load $ phonemeWave name


-- * modelling formants using bandpass filters

type Formant a = (UniFilter.Result a -> a, Pole Real, Real)

formants_a_noise :: [Formant a]
formants_a_noise =
   (UniFilter.bandpass, Pole 20 900, 1) :
   (UniFilter.bandpass, Pole 20 1200, 0.4) :
   (UniFilter.bandpass, Pole 10 2600, 0.07) :
   []

formants_f :: [Formant a]
formants_f =
   (UniFilter.lowpass, Pole 2 4000, 0.6) :
   (UniFilter.lowpass, Pole 2 11000, 0.3) :
   []

formants_sch :: [Formant a]
formants_sch =
   (UniFilter.bandpass, Pole 5 1500, 1.3) :
   (UniFilter.lowpass, Pole 2 3000, 0.6) :
   []

synthesis :: IO (SVL.ChunkSize -> SVL.Vector Real)
synthesis =
   Render.run $
      (sum (map (\(typ, Pole q f, amp) ->
                   Causal.amplify (Expr.cons amp)
                   <<<
                   typ
                   ^<<
                   Ctrl.process
                   $<
                   (fmap UniFilterL.unMultiValueParameter $ Sig.constant $
                    Expr.cons $ UniFilter.parameter $ Pole q $ f / sampleRate))
                formants_sch)
        $* Sig.noise 174373 0.02)

compareSpec ::IO ()
compareSpec = do
   sampled <- Sample.load (phonemeWave "sch")
   synthesized <- synthesis
   void $ Plot.plot WXT.cons $
      spectrumPlot sampled
      <>
      spectrumPlot
         (SVL.take (SVL.length sampled) $
          synthesized (SVL.chunkSize 4096))

render ::IO ()
render = do
   synthesized <- synthesis
   saveSound (Path.relFile "sch-synth.wav") $
      SVL.take sampleRateInt $
      synthesized (SVL.chunkSize 4096)


-- * purification of sampled periods

-- ** using a comb filter

type Comb = Real -> Word -> SVL.Vector Real -> SVL.Vector Real

makeComb :: IO Comb
makeComb =
   (\proc gain time -> pioApply (proc gain time))
   <$>
   CausalRender.run Causal.comb

makeHighComb :: IO Comb
makeHighComb =
   fmap (\proc gain time -> pioApply (proc gain time))
   $
   CausalRender.run $ \gain time ->
      Causal.comb gain time
      .
      (Filt1.highpassCausal $<
         Sig.constant (FirstOrder.parameter (1000 / Expr.cons sampleRate)))

scorePeriod ::
   Comb -> Real -> Word -> SVL.Vector Real -> (Real, SVL.Vector Real)
scorePeriod comb gain period sig =
   let end = SVL.takeEnd (3 * fromIntegral period) $ comb gain period sig
   in  (Analysis.volumeEuclideanSqr end, end)

vowelNames :: [String]
vowelNames = ["a", "e", "i", "o", "on", "u", "un", "oe", "ue", "ae"]

tonalNames :: [String]
tonalNames = vowelNames ++ ["l", "m", "n", "ng", "r", "j"]

sibilantNames :: [String]
sibilantNames = ["f", "h", "w", "s", "sch", "th", "ich", "ach"]

stopConsonantNames :: [String]
stopConsonantNames = ["p", "k", "t", "b", "g", "d"]

scanPeriods ::IO ()
scanPeriods = do
   comb <- makeComb
   forM_ tonalNames $ \name -> do
      sampled <- loadPhoneme name
      let scores =
             flip map [350 .. 400] $ \period ->
                (period,
                 flip map [0.9, 0.99, 0.999] $ \gain ->
                    fst $ scorePeriod comb gain period sampled)
      -- mapM_ print scores
      putStrLn $
         "maximum: " ++
         show (List.maximumBy (comparing snd) $ map (mapSnd maximum) scores)


normalize :: SVL.Vector Real -> SVL.Vector Real
normalize =
   FiltNRG.normalize ((4*) . Analysis.volumeEuclidean)


{-
We use the zero with the least derivative
in order to reduce jumps at the loop point.
In order to further reduce jumps, we cross-fade two adjacent periods.
It must be @length period3 = 3*len@.
@period3@ must contain a zero in the center chunk of size @len@.
-}
bestRotation :: Int -> SVL.Vector Real -> SVL.Vector Real
bestRotation len period3 =
   let start =
          fst $
          List.minimumBy (comparing snd) $ catMaybes $
          zipWith (fmap . (,)) [0..] $
          ListHT.mapAdjacent
             (\x y -> toMaybe (signum x /= signum y) (abs(x-y))) $
          SVL.unpack $ SVL.take len $ SVL.drop (len-1) period3
   in  Causal.apply
          (Causal.applyFst
              (FiltNRC.crossfade len)
              (SVL.drop (start+len) period3))
          (SVL.drop start period3)

findPeriod :: Comb -> SVL.Vector Real -> SVL.Vector Real
findPeriod comb sampled =
   normalize $
   uncurry (bestRotation . fromIntegral) $
   mapSnd snd $
   List.maximumBy (comparing (fst . snd)) $
   flip map [350 .. 400] $ \period ->
      (period, scorePeriod comb 0.99 period sampled)

extractPeriods ::IO ()
extractPeriods = do
   comb <- makeHighComb
   forM_ tonalNames $ \name ->
      saveSound (maskWave name) . findPeriod comb =<< loadPhoneme name


-- ** using the frequency spectrum

makeFilter :: IO (SV.Vector Real -> SVL.Vector Real -> SVL.Vector Real)
makeFilter =
   (\proc mask -> pioApply (proc (Render.buffer mask)))
   <$>
   CausalRender.run FiltNR.convolve

normalizeMax :: SVL.Vector Real -> SVL.Vector Real
normalizeMax = FiltNRG.normalize Analysis.volumeMaximum

envelope :: Int -> SVL.Vector Real
envelope sizeInt =
   let size = fromIntegral sizeInt
       rampSize = size / 8
   in  Piece.run SigG.defaultLazySize $
          0  |# (rampSize, Piece.cosine) #|-
          1 -|# (size-2*rampSize, Piece.step) #|-
          1 -|# (rampSize, Piece.cosine) #| (0::Float)

data Transfer =
   Transfer {
      transferSpectrum,
      transferShrunkenSpectrum,
      transferEnvelope,
      transferWindow :: SVL.Vector Real
   }

transfer :: SVL.Vector Real -> Transfer
transfer sampled =
   let halfResponseSize = 256
       responseSize = 2*halfResponseSize
       halfShrink = div (SVL.length sampled) (2*responseSize)
       shrink = 2*halfShrink
       spec = spectrum $ SVL.take (shrink*responseSize) sampled
       shrunkenSpec =
          SigG.fromState SigG.defaultLazySize $
          SigS.init $ SigS.cons 0 $ SigS.map SigG.sum $
          SigG.sliceVertical shrink $ SVL.drop halfShrink spec
       env = envelope responseSize
       window =
          FiltNRG.envelope env $
          uncurry (flip SVL.append) $
          SVL.splitAt halfResponseSize $
          normalizeMax $ SVL.map Complex.real $
          timeDomain shrunkenSpec
   in  Transfer {
          transferSpectrum = spec,
          transferShrunkenSpectrum = shrunkenSpec,
          transferEnvelope = env,
          transferWindow = window
       }

testTransfer ::IO ()
testTransfer = do
   trans <- transfer <$> Sample.load (phonemeWave "o-noise")
   filt <- makeFilter
   saveSound (tmpWave "spectrum") $
      normalizeMax $ transferSpectrum trans
   saveSound (tmpWave "shrunkenspectrum") $
      normalizeMax $ transferShrunkenSpectrum trans
   saveSound (tmpWave "envelope") $ transferEnvelope trans
   saveSound (tmpWave "window") $ transferWindow trans
   let window = SV.concat $ SVL.chunks $ transferWindow trans
   saveSound (tmpWave "filtered") $
      filt window $ SVL.concat $ replicate 100 $ SVL.cons 1 $
      SVL.replicate SVL.defaultChunkSize 380 0
   saveSound (tmpWave "chirp") $
      filt window $ SVL.concat $
      map (\n -> SVL.cons 1 $ SVL.replicate SVL.defaultChunkSize n 0) $
      [350..450]

transferMasks ::IO ()
transferMasks = do
--   forM_ (map (++"-noise") vowelNames) $ \name -> do
   forM_ (tonalNames++sibilantNames++stopConsonantNames) $ \name -> do
      trans <- transfer <$> loadPhoneme name
      saveSound (maskWave name) $ normalize $ transferWindow trans
      let spectrumPath = tmpWave $ "spec-" ++ name
      saveSound spectrumPath $ normalizeMax $ transferShrunkenSpectrum trans


main :: IO ()
main = transferMasks
