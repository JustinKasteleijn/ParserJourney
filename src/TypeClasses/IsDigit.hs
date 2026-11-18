{-# LANGUAGE InstanceSigs #-}

module TypeClasses.IsDigit where

import qualified Data.Char

class IsDigit a where
  isDigit :: a -> Bool

instance IsDigit Char where
  isDigit :: Char -> Bool
  isDigit = Data.Char.isDigit
