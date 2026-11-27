module CombinatorsTest.NatSpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           ParserJourney.ParserError
import           Test.Hspec

spec :: Spec
spec = describe "Test Nat Parser Combinator" $
         runTestCases nat testCases


testCases :: [TestCase String Integer]
testCases = [
    TestCase {
        testInput   = "0",
        expected    = Right $ (0, ParserState "" (Position 2 1)),
        description = "Test whether nat correctly parses boundary value 0"
      },
    TestCase {
        testInput   = "1",
        expected    = Right $ (1, ParserState "" (Position 2 1)),
        description = "Test whether nat correctly parses boundary value 1"
      },
    TestCase {
        testInput   = "-1",
        expected    = Left $ "Expected: nat, but got: Unexpected input: -1", -- Hardcoded change in the future :) (famous last words)
        description = "Test whether nat correctly fails on negative value"
      }
  ]
