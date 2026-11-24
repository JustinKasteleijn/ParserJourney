{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserJourney.ParserError where

import           Data.Proxy

class ParserError e where
  unexpectedEOF :: e
  unexpectedInput :: String -> e
  unexpectedMatch :: String -> e
  lesserThanMinbound :: forall a. (Bounded a, Show a) => Proxy a -> Integer -> e
  greatherThanMaxBound :: forall a. (Bounded a, Show a) => Proxy a -> Integer -> e
  addLabel :: String -> e -> e

instance ParserError String where
  unexpectedEOF :: String
  unexpectedEOF = "Unexpected EOF"

  unexpectedInput :: String -> String
  unexpectedInput actual = "Unexpected input: " ++ actual

  unexpectedMatch :: String -> String
  unexpectedMatch match = "Unexpected match: " ++ match

  lesserThanMinbound :: forall a. (Bounded a, Show a) => Proxy a -> Integer -> String
  lesserThanMinbound _ actual = "Expected: greather than " ++ show (minBound :: a) ++ " got: " ++ show actual

  greatherThanMaxBound :: forall a. (Bounded a, Show a) => Proxy a -> Integer -> String
  greatherThanMaxBound _ actual = "Expected: lesser than " ++ show (maxBound :: a) ++ " got: " ++ show actual

  addLabel :: String -> String -> String
  addLabel label err = "Expected: " ++ label ++ ", but got: " ++ err
