{-# LANGUAGE InstanceSigs #-}

module ParserJourney.TypeClasses.IsDecimalSeparator where

class IsDecimalSeparator a where
  isDecimalSeparator :: a -> Bool


instance {-# OVERLAPPABLE #-} IsDecimalSeparator Char where
  isDecimalSeparator :: Char -> Bool
  isDecimalSeparator = (== '.')
