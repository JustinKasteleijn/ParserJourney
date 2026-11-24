{-# LANGUAGE InstanceSigs #-}

module ParserJourney.TypeClasses.ToDigit where

import qualified Data.Char
import           ParserJourney.TypeClasses.IsDigit (IsDigit)

class IsDigit a => ToDigit a where
  toDigit :: a -> Int

instance ToDigit Char where
  toDigit :: Char -> Int
  toDigit = Data.Char.digitToInt
