module GetPositionSpec (spec) where

import Test.Hspec
import Generic
import ParserInput (ParserState(..), Position(..))
import Parser (item, getPosition)

spec :: Spec
spec = describe "Test getPosition returns right position" $
         runTestCases (item >> getPosition) testCases


testCases :: [TestCase String Position]
testCases = [
    TestCase {
        testInput   = "hello",
        expected    = Right
          (Position 2 1, ParserState "ello" (Position 2 1)),
        description = "Test whether getPosition returns correct position after consuming one none newline character"
    },
    TestCase {
        testInput   = "\nhello",
        expected    = Right
          (Position 1 2, ParserState "hello" (Position 1 2)),
        description = "Test whether getPosition returns correct position after consuming one newline character"
    }
  ]
