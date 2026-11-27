module CombinatorsTest.TokenSpec (spec) where

import           Generic
import           ParserJourney.Combinators.CharCombinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput                 (ParserState (..),
                                                            Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test Token Parser Combinator" $
         runTestCases (token 'h') testCases


testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "hello",
        expected    = Right ('h', ParserState "ello" (Position 2 1)),
        description = "Test whether token correctly consumes a single character and increments the column position accordingly"
      },
    TestCase {
        testInput   = "bello",
        expected    = Left $ "Expected: 'h', but got: " ++ unexpectedInput (Position 1 1) "'b'",
        description = "Test whether token rejects the wrong character"
      }
  ]
