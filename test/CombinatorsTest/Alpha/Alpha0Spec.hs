module CombinatorsTest.Alpha.Alpha0Spec (spec) where

import           Combinators.Alpha
import           Generic
import           ParserInput       (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Alpha0 Parser Combinator" $
         runTestCases alpha0 testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "aaa",
        expected    = Right ("aaa", ParserState "" (Position 4 1)),
        description = "Test whether alpha0 correctly consumes alpha characters"
      },
    TestCase {
        testInput   = "",
        expected    = Right ("", ParserState "" (Position 1 1)),
        description = "Test whether alpha0 correctly accepts EOF"
      },
    TestCase {
        testInput   = "111",
        expected    = Right ("", ParserState "111" (Position 1 1)),
        description = "Test whether alpha0 correctly ignores non alpha characters"
      },
    TestCase {
        testInput   = "aaa1aaa",
        expected    = Right ("aaa", ParserState "1aaa" (Position 4 1)),
        description = "Test whether alpha0 correctly rejects non alpha characters and returns input after non alpha character"
     }
  ]
