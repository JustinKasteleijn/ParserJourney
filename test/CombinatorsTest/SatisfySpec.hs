module CombinatorsTest.SatisfySpec (spec) where

import           Data.Char                                  (isDigit)
import           Generic
import           ParserJourney.Combinators.BasicCombinators
import           ParserJourney.ParserError                  (ParserError (unexpectedInput))
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           Test.Hspec

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
        expected    = Left $ unexpectedInput (Position 1 1) "'a'",
        description = "Test if satisfy correctly fails if the predicate is False"
      }
  ]
