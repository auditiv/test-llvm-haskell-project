module Synthesizer.LLVM.Server.Option (
   T(..),
   Option.ClientName(ClientName),
   get,
   ) where

import qualified Synthesizer.LLVM.Server.OptionCommon as Option
import qualified Sound.MIDI.Message.Channel as ChannelMsg

import qualified System.Path as Path
import qualified Options.Applicative as OP
import Control.Applicative (pure, (<*>))

import Prelude hiding (Real)


data T =
   Cons {
      clientName :: Option.ClientName,
      channel, extraChannel :: ChannelMsg.Channel,
      sampleDirectory :: Path.AbsRelDir
   }
   deriving (Show)



options :: OP.Parser T
options =
   pure Cons
   <*> Option.clientName "Name of the JACK client"
   <*> Option.channel
   <*> Option.extraChannel
   <*> Option.sampleDirectory


get :: IO T
get = Option.get options "Live software synthesizer using LLVM and JACK"
