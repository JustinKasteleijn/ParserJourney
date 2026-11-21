{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module ParserJourney.ParserError where

class ParserError e where
  unexpectedEOF :: e
  unexpectedInput :: String -> e
  unexpectedMatch :: String -> e
  addLabel :: String -> e -> e

instance ParserError String where
  unexpectedEOF :: String
  unexpectedEOF = "Unexpected EOF"

  unexpectedInput :: String -> String
  unexpectedInput actual = "Unexpected input: " ++ actual

  unexpectedMatch :: String -> String
  unexpectedMatch match = "Unexpected match: " ++ match

  addLabel :: String -> String -> String
  addLabel label err = "Expected: " ++ label ++ ", but got: " ++ err
