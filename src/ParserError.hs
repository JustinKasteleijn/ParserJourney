{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module ParserError where

class ParserError e where
  unexpectedEOF :: e
  satisfyError :: String -> e

instance ParserError String where
  unexpectedEOF :: String
  unexpectedEOF = "UnexpectedEOF"

  satisfyError :: String -> String
  satisfyError = ("Could not satisfy predicate on: " ++)
