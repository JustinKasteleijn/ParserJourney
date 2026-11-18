module GetPositionSpec (spec) where

import Test.Hspec
import Generic
import ParserInput (ParserState(..), Position(..))
import Parser (item, getPosition)
import Control.Applicative (many)

spec :: Spec
spec = describe "Test getPosition returns right position" $
         runTestCases (item >> getPosition) singleStepTestCases >>
         runTestCases (many item >> getPosition) multipleStepTestCases


singleStepTestCases :: [TestCase String Position]
singleStepTestCases = [
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

multipleStepTestCases :: [TestCase String Position]
multipleStepTestCases = [
    TestCase {
        testInput   = "11111",
        expected    = Right
          (Position 6 1, ParserState "" (Position 6 1)),
        description = "Test whether getPosition returns correct position after consuming multiple none newline character"
    },
    TestCase {
        testInput   = "11111\n",
        expected    = Right
          (Position 1 2, ParserState "" (Position 1 2)),
        description = "Test whether getPosition returns correct position after consuming multiple characters before one newline character"
    },
    TestCase {
        testInput   = "11111\n1111",
        expected    = Right
          (Position 5 2, ParserState "" (Position 5 2)),
        description = "Test whether getPosition returns correct position after consuming multiple characters before one newline character followed by more characters"
    }
  ]
