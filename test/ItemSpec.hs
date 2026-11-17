module ItemSpec (spec) where

import Test.Hspec
import Generic
import Data.Functor.Identity (runIdentity)
import Parser (Parser, item, parse)
import ParserInput (ParserState(..), Position(..))

spec :: Spec
spec = describe "Test Item Parser Combinator" $
         runTestCases item testCases


testCases :: [TestCase String Char]
testCases = [
    TestCase {
        testInput   = "hello",
        expected    = Right ('h', ParserState "ello" (Position 2 1)),
        description = "Test wether the parser correctly consumes a single character and increments the column position accordingly"
      }
  ]
