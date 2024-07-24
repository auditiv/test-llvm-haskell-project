module Synthesizer.LLVM.ExampleUtility where

import qualified Synthesizer.LLVM.Frame.SerialVector as Serial
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo

import Type.Data.Num.Decimal (D4, D16)

import Data.Word (Word32)


type Id a = a -> a

asMono :: Id (vector Float)
asMono = id

asStereo :: Id (vector (Stereo.T Float))
asStereo = id

asMonoPacked :: Id (vector (Serial.T D4 Float))
asMonoPacked = id

asMonoPacked16 :: Id (vector (Serial.T D16 Float))
asMonoPacked16 = id

asWord32 :: Id (vector Word32)
asWord32 = id

asWord32Packed :: Id (vector (Serial.T D4 Word32))
asWord32Packed = id
