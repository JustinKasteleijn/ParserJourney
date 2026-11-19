{-# LANGUAGE InstanceSigs #-}

module TypeClasses.IsAlpha where

import qualified Data.Char

class IsAlpha a where
  isAlpha :: a -> Bool

instance IsAlpha Char where
  isAlpha :: Char -> Bool
  isAlpha = Data.Char.isAlpha
