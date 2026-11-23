{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.NumberCombinators where

import           Data.List                                 (foldl')
import           ParserJourney.Combinators.CharCombinators (digit1)
import           ParserJourney.Parsable                    (Parsable (..))
import           ParserJourney.Parser                      (ParserT)
import           ParserJourney.ParserError                 (ParserError)
import           ParserJourney.ParserInput                 (Positioned)
import           ParserJourney.TypeClasses.ToDigit         (ToDigit (toDigit))

integer
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Integer
integer =
  foldl' (\n d -> n * 10 + fromIntegral (toDigit d)) 0
  <$> digit1
