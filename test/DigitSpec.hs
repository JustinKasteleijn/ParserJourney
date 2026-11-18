module DigitSpec (spec) where

import           Generic
import           Parser      (digit)
import           ParserError
import           ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Digit Parser Combinator" $
         runTestCases digit testCases


testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "123",
        expected    = Right ('1', ParserState "23" (Position 2 1)),
        description = "Test whether digit correctly consumes digit character"
      },
    TestCase {
        testInput   = "aaa",
        expected    = Left $ "Expected: digit, but got: " ++ unexpectedInput "'a'",
        description = "Test whether digit correctly rejects non digit character"
      }
  ]
