module CombinatorsTest.DecimalSpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Decimal Parser Combinator" $
         runTestCases decimal testCases


testCases :: [TestCase String Double]
testCases = [
    TestCase {
        testInput   = "0.0",
        expected    = Right $ (0, ParserState "" (Position 4 1)),
        description = "Test whether decimal correctly parses decimal separator '.' with comma and fraction value 0"
      },
    TestCase {
        testInput   = "1.79284",
        expected    = Right $ (1.79284, ParserState "" (Position 8 1)),
        description = "Test whether decimal correctly parses decimal separator '.' with multiple fraction values"
      },
    TestCase {
        testInput   = "1;0",
        expected    = Left "Expected: decimal, but got: Unexpected input: ';'", -- Hardcoded for now change in the future
        description = "Test whether nat correctly fails on negative value"
      }
  ]
