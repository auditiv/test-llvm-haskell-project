module Synthesizer.LLVM.Server.Option (
   T(..),
   get,
   SampleRate(SampleRate),
   ) where

import qualified Synthesizer.LLVM.Server.OptionCommon as Option
import Synthesizer.LLVM.Server.Common (SampleRate(..))

import qualified Synthesizer.ALSA.Storable.Play as Play
import qualified Data.StorableVector.Lazy       as SVL
import Synthesizer.ALSA.EventList (ClientName(ClientName))

import qualified Sound.MIDI.Message.Channel as ChannelMsg

import qualified System.Path as Path
import qualified Options.Applicative as OP
import Control.Applicative (pure, (<$>), (<*>))
import Data.Monoid ((<>))

import Prelude hiding (Real)


data T =
   Cons {
      device :: Play.Device,
      clientName :: ClientName,
      channel, extraChannel :: ChannelMsg.Channel,
      sampleDirectory :: Path.AbsRelDir,
      sampleRate :: Maybe (SampleRate Int),
      chunkSize :: SVL.ChunkSize,
      latency :: Int
   }
   deriving (Show)


defaultLatency :: Int
defaultLatency =
   -- 0
   -- 256
   1024



options :: OP.Parser T
options =
   pure Cons
   <*> OP.strOption
      (OP.short 'd' <>
       OP.long "device" <>
       OP.metavar "NAME" <>
       OP.value Play.defaultDevice <>
       OP.help "select ALSA output device")
   <*> fmap
         (\(Option.ClientName name) -> ClientName name)
         (Option.clientName "Name of the ALSA client")
   <*> Option.channel
   <*> Option.extraChannel
   <*> Option.sampleDirectory
   <*> Option.sampleRate
   <*> Option.blockSize Play.defaultChunkSize
   <*> OP.option
         (fromInteger <$>
          Option.parseNumber "latency" (\n -> 0<=n && n<=Option.maxInt) "non-negative")
         (OP.long "latency" <>
          OP.metavar "SIZE" <>
          OP.value defaultLatency <>
          OP.help "latency as number of sample-frames")


get :: IO T
get = Option.get options "Live software synthesizer using LLVM and ALSA"
