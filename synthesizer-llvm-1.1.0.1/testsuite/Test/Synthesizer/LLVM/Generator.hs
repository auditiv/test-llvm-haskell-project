module Test.Synthesizer.LLVM.Generator where


import Data.StorableVector.Lazy (ChunkSize)

import System.Random (Random)

import Control.Category (id)
import Control.Applicative (liftA2, liftA3)

import qualified Test.QuickCheck as QC

import Prelude hiding (id)


type T f p a = QC.Gen p

type Param p = (->) p

arg :: QC.Gen a -> QC.Gen a
arg = id

arbitrary :: (QC.Arbitrary a) => QC.Gen a
arbitrary = QC.arbitrary

choose :: (Random a) => (a,a) -> QC.Gen a
choose = QC.choose


pair :: QC.Gen a -> QC.Gen b -> QC.Gen (a,b)
pair = liftA2 (,)

triple :: QC.Gen a -> QC.Gen b -> QC.Gen c -> QC.Gen (a,b,c)
triple = liftA3 (,,)

withGenArgs :: QC.Gen p -> (IO (ChunkSize -> p -> test)) -> Test p test
withGenArgs = (,)


type Test p test = (QC.Gen p, IO (ChunkSize -> p -> test))

checkWithParam :: (Show p, QC.Testable test) => Test p test -> IO QC.Property
checkWithParam (gen, test) = do
   f <- test
   return $ QC.property (QC.forAll gen $ flip f)
