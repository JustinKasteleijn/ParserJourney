module CombinatorsTest.Alpha.AlphaSpec (spec) where

import           Combinators.Alpha
import           Generic
import           ParserError
import           ParserInput       (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Alpha Parser Combinator" $
         runTestCases alpha testCases


testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "aaa",
        expected    = Right ('a', ParserState "aa" (Position 2 1)),
        description = "Test whether alpha correctly consumes alpha character"
      },
    TestCase {
        testInput   = "111",
        expected    = Left $ "Expected: alpha, but got: " ++ unexpectedInput "'1'",
        description = "Test whether alpha correctly rejects non alpha character"
      }
  ]
