{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeApplications #-}

module CombinatorsTest.SignedIntSpec (spec) where

import           Data.Data
import           Data.Int
import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test all Signed Int Parser Combinator" $
         runTestCases i8 testCasesi8 >>
         runTestCases i16 testCasesi16 >>
         runTestCases i32 testCasesi32 >>
         runTestCases i64 testCasesi64


testCasesi8 :: [TestCase String Int8]
testCasesi8 = [
    TestCase {
        testInput   = "128",
        expected    = Left $ greatherThanMaxBound (Proxy @Int8) (Position 1 1) 128,
        description = "Test whether i8 correctly rejects 256"
      },
    TestCase {
        testInput   = "-129",
        expected    = Left $ lesserThanMinbound (Proxy @Int8) (Position 1 1) (-129),
        description = "Test whether i8 fails rejects -129"
      },
    TestCase {
        testInput   = "-128",
        expected    = Right (-128, ParserState "" (Position 5 1)),
        description = "Test whether i8 correctly parses and converts boundary value 1128"
      },
      TestCase {
          testInput   = "127",
          expected    = Right (127, ParserState "" (Position 4 1)),
          description = "Test whether i8 correctly parses boundary value 127"
        }
  ]

testCasesi16 :: [TestCase String Int16]
testCasesi16 = [
    TestCase {
        testInput   = "32768",
        expected    = Left $ greatherThanMaxBound (Proxy @Int16) (Position 1 1) 32768,
        description = "Test whether i16 correctly rejects 32768"
      },
    TestCase {
        testInput   = "-32769",
        expected    = Left $ lesserThanMinbound (Proxy @Int16) (Position 1 1) (-32769),
        description = "Test whether i16 correctly rejects -32769"
      },
    TestCase {
        testInput   = "-32768",
        expected    = Right (-32768, ParserState "" (Position 7 1)),
        description = "Test whether i16 correctly parses and converts boundary value -32768"
      },
      TestCase {
          testInput   = "32767",
          expected    = Right (32767, ParserState "" (Position 6 1)),
          description = "Test whether i16 correctly parses boundary value 32767"
        }
  ]

testCasesi32 :: [TestCase String Int32]
testCasesi32 = [
    TestCase {
        testInput   = "2147483648",
        expected    = Left $ greatherThanMaxBound (Proxy @Int32) (Position 1 1) 2147483648,
        description = "Test whether i32 correctly rejects 2147483648"
      },
    TestCase {
        testInput   = "-2147483649",
        expected    = Left $ lesserThanMinbound (Proxy @Int32) (Position 1 1) (-2147483649),
        description = "Test whether i32 correctly rejects -2147483649"
      },
    TestCase {
        testInput   = "2147483647",
        expected    = Right (2147483647, ParserState "" (Position 11 1)),
        description = "Test whether i32 correctly parses and converts boundary value 2147483647"
      },
      TestCase {
          testInput   = "-2147483648",
          expected    = Right (-2147483648, ParserState "" (Position 12 1)),
          description = "Test whether i32 correctly parses boundary value -2147483648"
        }
  ]

testCasesi64 :: [TestCase String Int64]
testCasesi64 = [
    TestCase {
        testInput   = "9223372036854775808",
        expected    = Left $ greatherThanMaxBound (Proxy @Int64) (Position 1 1) 9223372036854775808,
        description = "Test whether i64 correctly rejects 9223372036854775808"
      },
    TestCase {
        testInput   = "-9223372036854775809",
        expected    = Left $ lesserThanMinbound (Proxy @Int64) (Position 1 1) (-9223372036854775809),
        description = "Test whether i64 correctly rejects -9223372036854775809"
      },
    TestCase {
        testInput   = "9223372036854775807",
        expected    = Right (9223372036854775807, ParserState "" (Position 20 1)),
        description = "Test whether i64 correctly parses and converts boundary value 9223372036854775807"
      },
      TestCase {
          testInput   = "-9223372036854775808",
          expected    = Right (-9223372036854775808, ParserState "" (Position 21 1)),
          description = "Test whether i64 correctly parses boundary value 9223372036854775808"
        }
  ]
