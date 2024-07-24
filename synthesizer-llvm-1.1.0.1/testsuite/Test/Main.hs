module Main where

import qualified Test.Synthesizer.LLVM.RingBufferForward as RingBufferForward
import qualified Test.Synthesizer.LLVM.Helix as Helix
import qualified Test.Synthesizer.LLVM.Filter as Filter
import qualified Test.Synthesizer.LLVM.Packed as Packed

import qualified LLVM.Core as LLVM

import Control.Monad.IO.Class (liftIO)

import Data.Tuple.HT (mapFst)

import qualified Test.DocTest.Driver as DocTest


prefix :: String -> [(String, prop)] -> [(String, prop)]
prefix msg =
   map (mapFst (\str -> msg ++ "." ++ str))

main :: IO ()
main = do
   LLVM.initializeNativeTarget
   DocTest.run $ mapM_
      (\(name,prop) -> do
         DocTest.printPrefix (name++": ")
         DocTest.property =<< liftIO prop) $
      prefix "Helix" Helix.tests ++
      prefix "RingBufferForward" RingBufferForward.tests ++
      prefix "Filter" Filter.tests ++
      prefix "Packed" Packed.tests ++
      []
