{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserJourney.ParserError where

import           Data.Proxy
import           ParserJourney.ParserInput (Position)

class ParserError e where
  unexpectedEOF :: Position ->  e
  unexpectedInput :: Position -> String -> e
  unexpectedMatch :: Position -> String -> e
  lesserThanMinbound :: forall a. (Bounded a, Show a) => Proxy a -> Position -> Integer -> e
  greatherThanMaxBound :: forall a. (Bounded a, Show a) => Proxy a -> Position -> Integer -> e
  addLabel :: String -> e -> e

instance ParserError String where
  unexpectedEOF :: Position -> String
  unexpectedEOF pos = "Unexpected EOF at: " ++ show pos

  unexpectedInput :: Position -> String -> String
  unexpectedInput pos actual = "Unexpected input: " ++ actual ++ "  at: " ++ show pos

  unexpectedMatch :: Position -> String -> String
  unexpectedMatch pos match = "Unexpected match: " ++ match ++ " at: " ++ show pos

  lesserThanMinbound :: forall a. (Bounded a, Show a) => Proxy a -> Position -> Integer -> String
  lesserThanMinbound _ pos actual = "Expected: greather than " ++ show (minBound :: a) ++ " got: " ++ show actual ++ " at: " ++ show pos

  greatherThanMaxBound :: forall a. (Bounded a, Show a) => Proxy a -> Position ->  Integer -> String
  greatherThanMaxBound _ pos actual = "Expected: lesser than " ++ show (maxBound :: a) ++ " got: " ++ show actual ++ " at: " ++ show pos

  addLabel :: String -> String -> String
  addLabel label err = "Expected: " ++ label ++ ", but got: " ++ err
