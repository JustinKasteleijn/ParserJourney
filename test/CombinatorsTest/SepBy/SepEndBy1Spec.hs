module CombinatorsTest.SepBy.SepEndBy1Spec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test SepEndBy1 Parser Combinator" $
         runTestCases (sepEndBy1 (char ',') digit) testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "1,2,3,4,5,6,7,8,9",
        expected    = Right ("123456789", ParserState "" (Position 18 1)),
        description = "Test whether sepEndBy1 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: digit, but got: " ++ unexpectedEOF,
        description = "Test whether sepEndBy1 correctly accepts EOF"
      },
    TestCase {
        testInput   = "1,2,3,4,,5",
        expected    = Right ("1234", ParserState ",5" (Position 9 1)),
        description = "Test whether sepEndBy1 correctly ignores non digit characters"
      },
    TestCase {
        testInput   = "1,2,3,4,5,",
        expected    = Right ("12345", ParserState "" (Position 11 1)),
        description = "Test whether sepEndBy1 correctly ignores non digit characters"
      }
  ]
