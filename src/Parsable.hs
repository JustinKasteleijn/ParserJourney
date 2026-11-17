{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Parsable where

class Parsable s where
  type Elem s
  uncons :: s -> Maybe (Elem s, s)

instance Parsable [a] where
  type Elem [a] = a

  uncons :: [a] -> Maybe (a, [a])
  uncons []     = Nothing
  uncons (x:xs) = Just (x, xs)
