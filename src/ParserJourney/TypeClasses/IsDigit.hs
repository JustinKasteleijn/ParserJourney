{-# LANGUAGE InstanceSigs #-}

module ParserJourney.TypeClasses.IsDigit where

import qualified Data.Char

class IsDigit a where
  isDigit :: a -> Bool
  isMinus :: a -> Bool

instance IsDigit Char where
  isDigit :: Char -> Bool
  isDigit = Data.Char.isDigit

  isMinus :: Char -> Bool
  isMinus =  (== '-')
