{-# LANGUAGE InstanceSigs #-}

module ParserJourney.TypeClasses.IsSpace where

import qualified Data.Char

class IsSpace a where
  isSpace :: a -> Bool

instance IsSpace Char where
  isSpace :: Char -> Bool
  isSpace = Data.Char.isSpace
