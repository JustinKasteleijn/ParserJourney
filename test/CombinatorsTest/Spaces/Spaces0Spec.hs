module CombinatorsTest.Spaces.Spaces0Spec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Spaces0 Parser Combinator" $
         runTestCases spaces0 testCases


testCases :: [TestCase String ()]
testCases = [
    TestCase {
        testInput   = "   ",
        expected    = Right ((), ParserState "" (Position 4 1)),
        description = "Test whether space0 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Right ((), ParserState "" (Position 1 1)),
        description = "Test whether space0 correctly accepts EOF"
      },
    TestCase {
        testInput   = "aaa",
        expected    = Right ((), ParserState "aaa" (Position 1 1)),
        description = "Test whether space0 correctly ignores non digit characters"
      },
    TestCase {
        testInput   = "   a123",
        expected    = Right ((), ParserState "a123" (Position 4 1)),
        description = "Test whether space0 correctly rejects non digit characters and returns input after non digits"
     }
  ]
