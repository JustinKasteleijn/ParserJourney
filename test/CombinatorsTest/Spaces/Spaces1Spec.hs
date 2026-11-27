module CombinatorsTest.Spaces.Spaces1Spec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Spaces1 Parser Combinator" $
         runTestCases spaces1 testCases


testCases :: [TestCase String ()]
testCases = [
    TestCase {
        testInput   = "    1",
        expected    = Right ((), ParserState "1" (Position 5 1)),
        description = "Test whether space1 correctly consumes digit characters"
      },
    TestCase {
        testInput   = "",
        expected    = Left $ "Expected: space, but got: " ++ unexpectedEOF (Position 1 1),
        description = "Test whether space1 correctly rejects EOF"
      },
      TestCase {
        testInput   = "   a123",
        expected    = Right ((), ParserState "a123" (Position 4 1)),
        description = "Test whether space1 correctly rejects non digit characters and returns input after non digits"
      }
  ]
