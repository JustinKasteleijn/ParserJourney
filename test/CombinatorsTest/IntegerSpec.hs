module CombinatorsTest.IntegerSpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           ParserJourney.ParserError
import           Test.Hspec

spec :: Spec
spec = describe "Test Integer Parser Combinator" $
         runTestCases integer testCases


testCases :: [TestCase String Integer]
testCases = [
    TestCase {
        testInput   = "12345",
        expected    = Right (12345, ParserState "" (Position 6 1)),
        description = "Test whether integer correctly consumes multiple digits and build the represented number"
      },
    TestCase {
        testInput   = "ignore else",
        expected    = Left $ "Expected: digit, but got: " ++ unexpectedInput "'i'",
        description = "Test whether integer fails on non integer input"
      },
    TestCase {
        testInput   = "1",
        expected    = Right (1, ParserState "" (Position 2 1)),
        description = "Test whether integer correctly parses and converts single digit"
      }
  ]
