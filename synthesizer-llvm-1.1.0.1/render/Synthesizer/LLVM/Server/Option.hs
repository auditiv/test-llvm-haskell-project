module Synthesizer.LLVM.Server.Option (
   T(..),
   get,
   ) where

import qualified Synthesizer.LLVM.Server.OptionCommon as Option
import qualified Synthesizer.LLVM.Server.Default as Default
import Synthesizer.LLVM.Server.Common (SampleRate)
import qualified Data.StorableVector.Lazy as SVL

import qualified Sound.MIDI.Message.Channel as ChannelMsg

import qualified System.Path as Path

import qualified Options.Applicative as OP
import Control.Applicative (pure, (<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Prelude hiding (Real)


data T =
   Cons {
      channel, extraChannel :: ChannelMsg.Channel,
      sampleDirectory :: Path.AbsRelDir,
      sampleRate :: SampleRate Int,
      chunkSize :: SVL.ChunkSize
--      volume :: Float
   }
   deriving (Show)



options :: OP.Parser T
options =
   pure Cons
   <*> Option.channel
   <*> Option.extraChannel
   <*> Option.sampleDirectory
   <*> fmap (fromMaybe Default.sampleRate) Option.sampleRate
   <*> Option.blockSize (SVL.chunkSize (128*1024))
{-
   <*>
      (OP.option (parseNumber "volume" (const True) "any")
         (OP.short 'v' <>
          OP.long "volume" <>
          OP.metavar "FACTOR" <>
          OP.help "global volume")
-}


parser :: OP.Parser (T, String, Maybe String)
parser =
   pure (,,)
   <*> options
   <*> OP.strArgument (OP.metavar "infile.mid")
   <*> OP.argument (Just <$> OP.str)
         (OP.metavar "outfile.wav" <> OP.value Nothing)


get :: IO (T, String, Maybe String)
get = Option.get parser "Render MIDI to audio files using LLVM and SoX"
