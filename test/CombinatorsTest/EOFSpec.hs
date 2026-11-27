module CombinatorsTest.EOFSpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test EOF Parser Combinator" $
         runTestCases eof testCases


testCases :: [TestCase String ()]
testCases = [
    TestCase {
        testInput   = "hello",
        expected    = Left $ "Expected: eof, but got: " ++ unexpectedMatch (Position 1 1) "'h'",
        description = "Test whether eof correctly fails on non eof"
      },
    TestCase {
        testInput   = "",
        expected    = Right ((), ParserState "" (Position 1 1)),
        description = "Test whether eof correctly allows for EOF"
      }
  ]
