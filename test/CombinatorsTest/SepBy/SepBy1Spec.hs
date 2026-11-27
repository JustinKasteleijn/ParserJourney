module CombinatorsTest.SepBy.SepBy1Spec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test SepBy1 Parser Combinator" $
         runTestCases (digit `sepBy1` char ',') testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "1,2,3,4,5,6,7,8,9",
        expected    = Right ("123456789", ParserState "" (Position 18 1)),
        description = "Test whether sepBy1 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: digit, but got: " ++ unexpectedEOF (Position 1 1),
        description = "Test whether sepBy1 correctly accepts EOF"
      },
    TestCase {
        testInput   = "1,2,3,4,,5",
        expected    = Right ("1234", ParserState ",,5" (Position 8 1)),
        description = "Test whether sepBy1 correctly ignores non digit characters"
      }
  ]
