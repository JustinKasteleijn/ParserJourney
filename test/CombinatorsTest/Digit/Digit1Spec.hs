module CombinatorsTest.Digit.Digit1Spec (spec) where

import           Generic
import           ParserJourney.Combinators.CharCombinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Digit1 Parser Combinator" $
         runTestCases digit1 testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "123",
        expected    = Right ("123", ParserState "" (Position 4 1)),
        description = "Test whether digit1 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: digit, but got: " ++ unexpectedEOF (Position 1  1),
        description = "Test whether digit1 correctly rejects EOF"
      },
      TestCase {
        testInput   = "123a123",
        expected    = Right ("123", ParserState "a123" (Position 4 1)),
        description = "Test whether digit1 correctly rejects non digit characters and returns input after non digits"
      }
  ]
