module SatisfySpec (spec) where

import Test.Hspec
import Generic
import Data.Char (isDigit)
import ParserInput (ParserState(..), Position(..))
import Parser (satisfy)

spec :: Spec
spec = describe "Test Satisfy Parser Combinator" $
         runTestCases (satisfy isDigit) testCases

testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "12",
        expected    = Right ('1', ParserState "2" (Position 2 1)),
        description = "Test if satisfy correctly consumes the character if the predicate is True"
      },
    TestCase {
        testInput   = "a2",
        expected    = Left "Unexpected input: 'a'",
        description = "Test if satisfy correctly fails if the predicate is False"
      }
  ]
