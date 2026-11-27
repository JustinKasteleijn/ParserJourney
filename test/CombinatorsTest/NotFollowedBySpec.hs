module CombinatorsTest.NotFollowedBySpec (spec) where

import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test NotFollowedBy Parser Combinator" $
         runTestCases (notFollowedBy (char 'h')) testCasesChar


testCasesChar :: [TestCase String ()]
testCasesChar = [
    TestCase {
        testInput   = "hello",
        expected    = Left $ unexpectedMatch (Position 1 1) "'h'",
        description = "Test whether notFollowedBy fails when followed by the parser for single character"
      },
    TestCase {
        testInput   = "elle",
        expected    = Right ((), ParserState "elle" (Position 1 1)),
        description = "Test whether notFollowedBy does not change the input state nor position and returns ()"
      },
    TestCase {
        testInput   = "",
        expected    = Right ((), ParserState "" (Position 1 1)),
        description = "Test whether item correctly reports unexpected end of file"
      }
  ]
