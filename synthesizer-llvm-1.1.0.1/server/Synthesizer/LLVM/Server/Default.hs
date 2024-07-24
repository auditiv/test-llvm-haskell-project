module Synthesizer.LLVM.Server.Default where

import Synthesizer.LLVM.Server.Common (SampleRate(SampleRate))

import qualified Sound.MIDI.Message.Channel as ChannelMsg

import qualified System.Path as Path


sampleRate :: Num a => SampleRate a
sampleRate =
   SampleRate
      44100
      -- 24000
      -- 48000


newtype ClientName = ClientName String
   deriving (Show)

clientName :: ClientName
clientName = ClientName "Haskell-LLVM-Synthesizer"


channel :: ChannelMsg.Channel
channel = ChannelMsg.toChannel 0

sampleDirectory :: Path.AbsRelDir
sampleDirectory = Path.absRel "speech"
