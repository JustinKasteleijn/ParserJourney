module CombinatorsTest.Alpha.AlphaSpec (spec) where

import           Generic
import           ParserJourney.Combinators.CharCombinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Letter Parser Combinator" $
         runTestCases letter testCases


testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "aaa",
        expected    = Right ('a', ParserState "aa" (Position 2 1)),
        description = "Test whether letter correctly consumes alpha character"
      },
    TestCase {
        testInput   = "111",
        expected    = Left $ "Expected: alpha, but got: " ++ unexpectedInput (Position 1 1) "'1'",
        description = "Test whether letter correctly rejects non alpha character"
      }
  ]
