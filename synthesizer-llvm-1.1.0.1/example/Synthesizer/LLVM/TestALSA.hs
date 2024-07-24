module Main where

import qualified Synthesizer.LLVM.LNdW2011 as LNdW

import Control.Monad (when)


main :: IO ()
main = do
   when True LNdW.flyPacked
   when False LNdW.modulation
   when False LNdW.bubblesPacked
