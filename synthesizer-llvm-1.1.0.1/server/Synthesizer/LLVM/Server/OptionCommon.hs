{-
Guide for common Linux/Unix command-line options:
  http://www.faqs.org/docs/artu/ch10s05.html
-}
module Synthesizer.LLVM.Server.OptionCommon (
   module Synthesizer.LLVM.Server.OptionCommon,
   ClientName(ClientName),
   ) where

import qualified Synthesizer.LLVM.Server.Default as Default
import Synthesizer.LLVM.Server.Default (ClientName(ClientName))
import Synthesizer.LLVM.Server.Common (SampleRate(SampleRate))

import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Data.StorableVector.Lazy as SVL

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path

import qualified Shell.Utility.ParseArgument as ParseArg
import qualified Options.Applicative as OP
import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))

import Prelude hiding (Real)


clientName :: String -> OP.Parser ClientName
clientName help =
   OP.option (fmap ClientName OP.str)
      (OP.long "clientname" <>
       OP.metavar "NAME" <>
       OP.help help <>
       OP.value Default.clientName)


parseChannel :: OP.ReadM ChannelMsg.Channel
parseChannel =
   OP.eitherReader $ \str ->
   case reads str of
      [(chan, "")] ->
         if 0<=chan && chan<16
           then return $ ChannelMsg.toChannel chan
           else Left "MIDI channel must a number from 0..15"
      _ -> Left $ "channel must be a number, but is '" ++ str ++ "'"

channel, extraChannel :: OP.Parser ChannelMsg.Channel
channel =
   OP.option parseChannel
      (OP.short 'c' <>
       OP.long "channel" <>
       OP.metavar "CHANNEL" <>
       OP.help "Select MIDI input channel (0-based)" <>
       OP.value Default.channel)

extraChannel =
   OP.option parseChannel
      (OP.long "extra-channel" <>
       OP.metavar "CHANNEL" <>
       OP.help "Select MIDI channel with effects" <>
       OP.value (ChannelMsg.toChannel 1))

path :: (PathClass.FileDir fd) => OP.ReadM (Path.AbsRel fd)
path = OP.eitherReader Path.parse

sampleDirectory :: OP.Parser Path.AbsRelDir
sampleDirectory =
   OP.option path
      (OP.short 'I' <>
       OP.long "sample-directory" <>
       OP.metavar "DIR" <>
       OP.help "Directory for sound samples" <>
       OP.value Default.sampleDirectory)


maxInt :: Integer
maxInt = fromIntegral (maxBound :: Int)

parseNumber ::
   (Read a) =>
   String -> (a -> Bool) -> String -> OP.ReadM a
parseNumber name constraint constraintName =
   OP.eitherReader $ ParseArg.parseNumber name constraint constraintName

sampleRate :: OP.Parser (Maybe (SampleRate Int))
sampleRate =
   OP.option
      (Just . SampleRate . fromInteger <$>
       parseNumber "sample-rate" (\n -> 0<n && n<=maxInt) "positive")
      (OP.short 'r' <>
       OP.long "samplerate" <>
       OP.metavar "RATE" <>
       OP.value Nothing <>
       OP.help "Sample-rate in samples per second")

blockSize :: SVL.ChunkSize -> OP.Parser SVL.ChunkSize
blockSize deflt =
   OP.option
      (SVL.ChunkSize . fromInteger <$>
       parseNumber "blocksize" (\n -> 0<n && n<=maxInt) "positive")
      (OP.short 'b' <>
       OP.long "blocksize" <>
       OP.metavar "SIZE" <>
       OP.value deflt <>
       OP.help "Block size as number of sample-frames")


get :: OP.Parser a -> String -> IO a
get parser descr =
   OP.execParser $
   OP.info
      (OP.helper <*> parser)
      (OP.fullDesc <> OP.progDesc descr)
