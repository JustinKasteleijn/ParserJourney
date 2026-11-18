module Digit0Spec (spec) where

import Test.Hspec
import Generic
import ParserInput (ParserState(..), Position(..))
import Parser (digit0)
import ParserError

spec :: Spec
spec = describe "Test Digit1 Parser Combinator" $
         runTestCases digit0 testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "123",
        expected    = Right ("123", ParserState "" (Position 4 1)),
        description = "Test whether digit0 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Right ("", ParserState "" (Position 1 1)),
        description = "Test whether digit0 correctly accepts EOF"
      },
    TestCase {
       testInput   = "123a123",
       expected    = Right ("123", ParserState "a123" (Position 4 1)),
       description = "Test whether digit correctly rejects non digit characters and returns input after digits"
     }
  ]
