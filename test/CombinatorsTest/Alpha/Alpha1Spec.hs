module CombinatorsTest.Alpha.Alpha1Spec (spec) where

import           Combinators.Alpha
import           Generic
import           ParserError
import           ParserInput       (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Alpha1 Parser Combinator" $
         runTestCases alpha1 testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "aaa",
        expected    = Right ("aaa", ParserState "" (Position 4 1)),
        description = "Test whether alpha1 correctly consumes alpha characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: alpha, but got: " ++ unexpectedEOF,
        description = "Test whether alpha1 correctly rejects EOF"
      },
      TestCase {
        testInput   = "aaa1aaa",
        expected    = Right ("aaa", ParserState "1aaa" (Position 4 1)),
        description = "Test whether alpha1 correctly rejects non alpha characters and returns input after non alpha"
      }
  ]
