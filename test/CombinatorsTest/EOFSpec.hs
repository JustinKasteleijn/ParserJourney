module CombinatorsTest.EOFSpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           ParserJourney.ParserError
import           Test.Hspec

spec :: Spec
spec = describe "Test EOF Parser Combinator" $
         runTestCases eof testCases


testCases :: [TestCase String ()]
testCases = [
    TestCase {
        testInput   = "hello",
        expected    = Left $ "Expected: eof, but got: " ++ unexpectedMatch "'h'",
        description = "Test whether eof correctly fails on non eof"
      },
    TestCase {
        testInput   = "",
        expected    = Right ((), ParserState "" (Position 1 1)),
        description = "Test whether eof correctly allows for EOF"
      }
  ]
