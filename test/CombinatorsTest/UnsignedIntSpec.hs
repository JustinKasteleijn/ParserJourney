{-# LANGUAGE TypeApplications #-}

module CombinatorsTest.UnsignedIntSpec (spec) where

import           Generic
import           Data.Word
import           Data.Data
import           ParserJourney.Combinators
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           ParserJourney.ParserError
import           Test.Hspec

spec :: Spec
spec = describe "Test all Unsigned Int Parser Combinator" $
         runTestCases u8 testCasesu8


testCasesu8 :: [TestCase String Word8]
testCasesu8 = [
    TestCase {
        testInput   = "256",
        expected    = Left $ greatherThanMaxBound (Proxy @Word8) 256,
        description = "Test whether u8 correctly rejects 256"
      },
    TestCase {
        testInput   = "-1",
        expected    = Left $ lesserThanMinbound (Proxy @Word8) (-1),
        description = "Test whether u8 fails on negative"
      },
    TestCase {
        testInput   = "1",
        expected    = Right (1, ParserState "" (Position 2 1)),
        description = "Test whether u8 correctly parses and converts boundary value 1"
      },
      TestCase {
          testInput   = "255",
          expected    = Right (255, ParserState "" (Position 4 1)),
          description = "Test whether u8 correctly parses boundary value 255"
        }
  ]
