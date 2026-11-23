module CombinatorsTest.Spaces.SpaceSpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Space Parser Combinator" $
         runTestCases space testCases


testCases :: [TestCase String ()]
testCases = [
    TestCase {
        testInput   = " 23",
        expected    = Right ((), ParserState "23" (Position 2 1)),
        description = "Test whether space correctly consumes digit character"
      },
    TestCase {
        testInput   = "aaa",
        expected    = Left $ "Expected: space, but got: " ++ unexpectedInput "'a'",
        description = "Test whether space correctly rejects non digit character"
      }
  ]
