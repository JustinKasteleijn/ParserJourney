module ItemSpec (spec) where

import           Generic
import           ParserJourney.Combinators.BasicCombinators
import           ParserJourney.ParserInput                  (ParserState (..),
                                                             Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Item Parser Combinator" $
         runTestCases item testCases


testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "hello",
        expected    = Right ('h', ParserState "ello" (Position 2 1)),
        description = "Test whether the parser correctly consumes a single character and increments the column position accordingly"
      },
    TestCase {
        testInput   = "\n",
        expected    = Right ('\n', ParserState "" (Position 1 2)),
        description = "Test whether item correctly consumes the newline character and increments the line position accordingly"
      },
    TestCase {
        testInput   = "",
        expected    = Left "Unexpected EOF",
        description = "Test whether item correctly reports unexpected end of file"
      }
  ]
