module CombinatorsTest.Alpha.Alpha1Spec (spec) where

import           Generic
import           ParserJourney.Combinators.CharCombinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Letter1 Parser Combinator" $
         runTestCases letter1 testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "aaa",
        expected    = Right ("aaa", ParserState "" (Position 4 1)),
        description = "Test whether letter1 correctly consumes alpha characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: alpha, but got: " ++ unexpectedEOF (Position 1 1),
        description = "Test whether letter1 correctly rejects EOF"
      },
      TestCase {
        testInput   = "aaa1aaa",
        expected    = Right ("aaa", ParserState "1aaa" (Position 4 1)),
        description = "Test whether letter1 correctly rejects non alpha characters and returns input after non alpha"
      }
  ]
