module Main where

import qualified Synthesizer.LLVM.Server.CausalPacked.Test as ServerCausalTest
import qualified Synthesizer.LLVM.Server.CausalPacked.Run  as ServerCausal
import qualified Synthesizer.LLVM.Server.Packed.Test as ServerPackedTest
import qualified Synthesizer.LLVM.Server.Packed.Run  as ServerPacked
import qualified Synthesizer.LLVM.Server.Scalar.Test as ServerScalarTest
import qualified Synthesizer.LLVM.Server.Scalar.Run  as ServerScalar

import qualified LLVM.Core as LLVM


part :: Int
part = 106

main :: IO ()
main =
   LLVM.initializeNativeTarget >>
   case part of
      000 -> ServerScalar.pitchBend
      001 -> ServerScalar.frequencyModulation
      002 -> ServerScalar.keyboard
      003 -> ServerScalar.keyboardStereo
      004 -> ServerScalar.keyboardMulti
      005 -> ServerScalar.keyboardStereoMulti
      100 -> ServerPacked.frequencyModulation
      101 -> ServerPacked.keyboard
      102 -> ServerPacked.keyboardStereo
      103 -> ServerPacked.keyboardFM
      104 -> ServerPacked.keyboardFMMulti
      105 -> ServerPacked.keyboardDetuneFM
      106 -> ServerPacked.keyboardFilter -- there is still a leak when playing for a long time with arcStrings
      150 -> ServerCausal.keyboard
      151 -> ServerCausal.keyboardFM
      152 -> ServerCausal.keyboardDetuneFM
      153 -> ServerCausal.keyboardMultiChannel
      154 -> ServerCausal.voderBand
      155 -> ServerCausal.formant
      156 -> ServerCausal.voderMask
      157 -> ServerCausal.voderMaskEnv
      158 -> ServerCausal.voderMaskSeparated
      159 -> ServerCausal.voderMaskMulti
      200 -> ServerScalarTest.pitchBend0
      201 -> ServerScalarTest.pitchBend1
      202 -> ServerScalarTest.pitchBend2
      203 -> ServerScalarTest.sequencePress
      300 -> ServerPackedTest.adsr
      301 -> ServerPackedTest.sequencePlain
      302 -> ServerPackedTest.sequenceLLVM
      303 -> ServerPackedTest.sequencePitchBendCycle
      304 -> ServerPackedTest.sequencePitchBendSimple
      305 -> ServerPackedTest.sequencePitchBend
      306 -> ServerPackedTest.sequenceModulated
      307 -> ServerPackedTest.sequencePress
      308 -> ServerPackedTest.sequenceModulatedLong
      309 -> ServerPackedTest.sequenceModulatedLongFM
      310 -> ServerPackedTest.sequenceModulatedRepeat
      311 -> ServerPackedTest.sequenceSample
      312 -> ServerPackedTest.sequenceSample1 -- leak
--      313 -> ServerPackedTest.testSequenceSample1a -- leak
      320 -> ServerPackedTest.sequenceSample2 -- leak
      321 -> ServerPackedTest.sequenceSample3 -- leak
      322 -> ServerPackedTest.sequenceSample4 -- leak
      323 -> ServerPackedTest.sequenceFM1 -- leak
      324 -> ServerPackedTest.bellNoiseStereoTest
      400 -> ServerCausalTest.render
      401 -> ServerCausalTest.sequenceNothing
      402 -> ServerCausalTest.sequenceSingleLong
      403 -> ServerCausalTest.sequenceSingleShort
      404 -> ServerCausalTest.sequenceLoop
      405 -> ServerCausalTest.sequenceStaccato
      406 -> ServerCausalTest.sequenceControlled
      407 -> ServerCausalTest.sequenceControlledModulated
      409 -> ServerCausalTest.functional
      410 -> ServerCausalTest.functionalPlug
      411 -> ServerCausalTest.functionalTine >>
             ServerCausalTest.functionalPlugTine
      412 -> ServerCausalTest.sampledSound
      413 -> ServerCausalTest.sampledSoundCrash
      414 -> ServerCausalTest.sampledSoundMono
      415 -> ServerCausalTest.frequencyModulation
      416 -> ServerCausalTest.frequencyModulationIO
      417 -> ServerCausalTest.frequencyModulationStrictIO
      418 -> ServerCausalTest.frequencyModulationSawIO
      419 -> ServerCausalTest.envelopeIO
      _   -> error "not implemented server part"
