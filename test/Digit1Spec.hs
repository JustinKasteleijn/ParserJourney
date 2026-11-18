module Digit1Spec (spec) where

import Test.Hspec
import Generic
import ParserInput (ParserState(..), Position(..))
import Parser (digit1)
import ParserError

spec :: Spec
spec = describe "Test Digit1 Parser Combinator" $
         runTestCases digit1 testCases


testCases :: [TestCase String String]
testCases = [
    TestCase {
        testInput   = "123",
        expected    = Right ("123", ParserState "" (Position 4 1)),
        description = "Test whether digit1 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: digit, but got: " ++ unexpectedEOF,
        description = "Test whether digit1 correctly rejects EOF"
      },
      TestCase {
        testInput   = "123a123",
        expected    = Right ("123", ParserState "a123" (Position 4 1)),
        description = "Test whether digit1 correctly rejects non digit characters and returns input after digits"
      }
  ]
