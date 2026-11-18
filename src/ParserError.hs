{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module ParserError where

class ParserError e where
  unexpectedEOF :: e
  unexpectedInput :: String -> e
  addLabel :: String -> e -> e

instance ParserError String where
  unexpectedEOF :: String
  unexpectedEOF = "Unexpected EOF"

  unexpectedInput :: String -> String
  unexpectedInput actual = "Unexpected input: " ++ actual

  addLabel :: String -> String -> String
  addLabel label err = "Expected: " ++ label ++ ", but got: " ++ err
