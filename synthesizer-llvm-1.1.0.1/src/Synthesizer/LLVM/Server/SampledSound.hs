module Synthesizer.LLVM.Server.SampledSound where

import Synthesizer.LLVM.Server.Common (Real)

import qualified Sound.Sox.Read          as SoxRead
import qualified Sound.Sox.Option.Format as SoxOption
import Control.Exception (bracket)

import qualified Synthesizer.Storable.Signal      as SigSt
import qualified Data.StorableVector.Lazy         as SVL

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path
import System.Path ((</>))

import Data.Tuple.HT (mapPair)

import qualified Number.DimensionTerm as DN

import Prelude hiding (Real, length)



data T =
   Cons {
      body :: SigSt.T Real,
      sampleRate :: DN.Frequency Real,
      positions :: Positions
   }

data Positions =
   Positions {
      start, length,
      loopStart, loopLength :: Int,
      period :: Real
   }


-- ToDo: flag failure if files cannot be found, or just remain silent
load :: (PathClass.AbsRel ar) => Path.File ar -> IO (SVL.Vector Real)
load path =
   bracket (SoxRead.open SoxOption.none (Path.toString path)) SoxRead.close $
   SoxRead.withHandle1 (SVL.hGetContentsSync SVL.defaultChunkSize)

loadRanges :: (PathClass.AbsRel ar) => Path.Dir ar -> Info -> IO [T]
loadRanges dir (Info file sr poss) =
   fmap
      (\smp -> map (Cons smp (DN.frequency sr)) poss)
      (load (dir </> file))


data
   Info =
      Info {
         infoName :: Path.RelFile,
         infoRate :: Real,
         infoPositions :: [Positions]
      }

info :: FilePath -> Real -> [Positions] -> Info
info path = Info (Path.relFile path)


parts :: T -> (SigSt.T Real, SigSt.T Real, SigSt.T Real)
parts smp =
   let pos = positions smp
       (attack,sustain) =
          mapPair
             (SigSt.drop (start pos),
              SigSt.take (loopLength pos)) $
          SigSt.splitAt (loopStart pos) $
          body smp
       release =
          SigSt.drop (loopStart pos + loopLength pos) $
          SigSt.take (start     pos + length     pos) $
          body smp
   in  (attack, sustain, release)



-- * examples

tomatensalatPositions :: [Positions]
tomatensalatPositions =
   Positions      0 29499  12501 15073 321.4 :
   Positions  29499 31672  38163 17312 320.6 :
   Positions  67379 28610  81811 10667 323.2 :
   Positions  95989 31253 106058 16111 323.7 :
   {-
   vor dem 't' kommt noch das Ende vom 'a'
   wir bräuchten eine weitere Positionsangabe,
   um am Ende etwas überspringen zu können.
   Ein Smart-Konstruktor wie 'positions'
   könnte das bisherige Verhalten nachmachen.
   -}
   Positions 127242 38596 136689 11514 319.3 :
   []


tomatensalat :: Info
tomatensalat =
   info "tomatensalat2.wav" 44100 tomatensalatPositions


halPositions :: [Positions]
halPositions =
--   Positions   2371 25957   7362  6321 :
   Positions   2371 25957 (2371+25957) 1 320 :
   Positions  40546 34460  63540  9546 317.4 :
   Positions  79128 32348  94367 14016 317.8 :
   Positions 112027 21227 125880  5500 322.5 :
   Positions 146057 23235 168941   352 320 :
   []

hal :: Info
hal =
   info "haskell-in-leipzig2.wav" 44100 halPositions


graphentheoriePositions :: [Positions]
graphentheoriePositions =
   Positions      0 29524  13267 14768 301.1 :
   Positions  29524 35333  47624  9968 301.6 :
   Positions  64857 31189  73818 16408 297.3 :
   Positions  96046 31312 106206 18504 302.9 :
   Positions 127358 32127 132469 16530 299.4 :
   []

graphentheorie :: Info
graphentheorie =
   info "graphentheorie0.wav" 44100 graphentheoriePositions
