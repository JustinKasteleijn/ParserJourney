module CombinatorsTest.NewlineSpec (spec) where

import           Generic
import           ParserJourney.Combinators.CharCombinators
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           ParserJourney.ParserError
import           Test.Hspec

spec :: Spec
spec = describe "Test Newline Parser Combinator" $
         runTestCases newline testCases


testCases :: [TestCase String ()]
testCases = [
    TestCase {
        testInput   = "\n",
        expected    = Right ((), ParserState "" (Position 1 2)),
        description = "Test whether newline correctly parses '\n' newline"
      },
    TestCase {
        testInput   = "\r",
        expected    = Right ((), ParserState "" (Position 1 2)),
        description = "Test whether newline correctly parses '\r' newline"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: newline, but got: " ++ unexpectedEOF,
        description = "Test whether newline fails on newline"
     },
     TestCase {
         testInput   = "a",
         expected    = Left $ "Expected: newline, but got: " ++ unexpectedInput "'a'",
         description = "Test whether newline fails on non newline character"
     }
  ]
