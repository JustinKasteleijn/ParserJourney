{-# LANGUAGE TypeApplications #-}

module CombinatorsTest.UnsignedIntSpec (spec) where

import           Data.Data
import           Data.Word
import           Generic
import           ParserJourney.Combinators
import           ParserJourney.ParserError
import           ParserJourney.ParserInput (ParserState (..), Position (..))
import           Test.Hspec

spec :: Spec
spec = describe "Test all Unsigned Int Parser Combinator" $
         runTestCases u8 testCasesu8 >>
         runTestCases u16 testCasesu16 >>
         runTestCases u32 testCasesu32 >>
         runTestCases u64 testCasesu64


testCasesu8 :: [TestCase String Word8]
testCasesu8 = [
    TestCase {
        testInput   = "256",
        expected    = Left $ greatherThanMaxBound (Proxy @Word8) (Position 1 1) 256,
        description = "Test whether u8 correctly rejects 256"
      },
    TestCase {
        testInput   = "-1",
        expected    = Left $ lesserThanMinbound (Proxy @Word8) (Position 1 1) (-1),
        description = "Test whether u8 fails on negative"
      },
    TestCase {
        testInput   = "1",
        expected    = Right (1, ParserState "" (Position 2 1)),
        description = "Test whether u8 correctly parses and converts boundary value 1"
      },
      TestCase {
          testInput   = "255",
          expected    = Right (255, ParserState "" (Position 4 1)),
          description = "Test whether u8 correctly parses boundary value 255"
        },
      TestCase {
            testInput   = "0",
            expected    = Right (0, ParserState "" (Position 2 1)),
            description = "Test whether u8 correctly parses boundary value 0"
          }
  ]

testCasesu16 :: [TestCase String Word16]
testCasesu16 = [
    TestCase {
        testInput   = "65536",
        expected    = Left $ greatherThanMaxBound (Proxy @Word16) (Position 1 1) 65536,
        description = "Test whether u16 correctly rejects 65536"
      },
    TestCase {
        testInput   = "-1",
        expected    = Left $ lesserThanMinbound (Proxy @Word16) (Position 1 1) (-1),
        description = "Test whether u16 fails on negative"
      },
    TestCase {
        testInput   = "1",
        expected    = Right (1, ParserState "" (Position 2 1)),
        description = "Test whether u16 correctly parses and converts boundary value 1"
      },
      TestCase {
          testInput   = "65535",
          expected    = Right (65535, ParserState "" (Position 6 1)),
          description = "Test whether u16 correctly parses boundary value 65535"
        },
      TestCase {
          testInput   = "0",
          expected    = Right (0, ParserState "" (Position 2 1)),
          description = "Test whether u16 correctly parses boundary value 0"
        }
  ]

testCasesu32 :: [TestCase String Word32]
testCasesu32 = [
    TestCase {
        testInput   = "4294967296",
        expected    = Left $ greatherThanMaxBound (Proxy @Word32) (Position 1 1) 4294967296,
        description = "Test whether u32 correctly rejects 4294967296"
      },
    TestCase {
        testInput   = "-1",
        expected    = Left $ lesserThanMinbound (Proxy @Word32) (Position 1 1) (-1),
        description = "Test whether u32 fails on negative"
      },
    TestCase {
        testInput   = "1",
        expected    = Right (1, ParserState "" (Position 2 1)),
        description = "Test whether u32 correctly parses and converts boundary value 1"
      },
      TestCase {
          testInput   = "4294967295",
          expected    = Right (4294967295, ParserState "" (Position 11 1)),
          description = "Test whether u32 correctly parses boundary value 4294967295"
        },
      TestCase {
          testInput   = "0",
          expected    = Right (0, ParserState "" (Position 2 1)),
          description = "Test whether u32 correctly parses boundary value 0"
        }
  ]

testCasesu64 :: [TestCase String Word64]
testCasesu64 = [
    TestCase {
        testInput   = "18446744073709551616",
        expected    = Left $ greatherThanMaxBound (Proxy @Word64) (Position 1 1) 18446744073709551616,
        description = "Test whether u64 correctly rejects 4294967296"
      },
    TestCase {
        testInput   = "-1",
        expected    = Left $ lesserThanMinbound (Proxy @Word64) (Position 1 1) (-1),
        description = "Test whether u64 fails on negative"
      },
    TestCase {
        testInput   = "1",
        expected    = Right (1, ParserState "" (Position 2 1)),
        description = "Test whether u64 correctly parses and converts boundary value 1"
      },
      TestCase {
          testInput   = "18446744073709551615",
          expected    = Right (18446744073709551615, ParserState "" (Position 21 1)),
          description = "Test whether u64 correctly parses boundary value 4294967295"
        },
      TestCase {
          testInput   = "0",
          expected    = Right (0, ParserState "" (Position 2 1)),
          description = "Test whether u64 correctly parses boundary value 0"
        }
  ]
