{-# LANGUAGE OverloadedRecordDot #-}

module Generic where

import Test.Hspec
import Control.Monad (forM_)
import Parser (parse, Parser)
import Data.Functor.Identity (Identity(runIdentity))
import ParserInput (ParserState)

data TestCase s a = TestCase {
    description :: String,
    expected    :: Either String (a, ParserState s),
    testInput   :: s
  }

runTestCases
  :: (Eq a, Show a, Eq s, Show s)
  => Parser s a
  -> [TestCase s a]
  -> Spec
runTestCases p cases
  = forM_ cases $ \tcase ->
      it tcase.description $
        runIdentity (parse p tcase.testInput)
          `shouldBe` tcase.expected
