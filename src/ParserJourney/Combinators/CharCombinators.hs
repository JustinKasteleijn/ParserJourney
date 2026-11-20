{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.CharCombinators where

import           Control.Applicative                        (Alternative (many, some))
import           ParserJourney.Combinators.BasicCombinators (satisfy, (<?>))
import           ParserJourney.Parsable                     (Parsable (Elem))
import           ParserJourney.Parser                       (ParserT)
import           ParserJourney.ParserError                  (ParserError)
import           ParserJourney.ParserInput                  (Positioned)
import           ParserJourney.TypeClasses.IsAlpha          (IsAlpha (isAlpha))
import           ParserJourney.TypeClasses.IsDigit          (IsDigit (isDigit))

letter
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsAlpha (Elem s)
  ) => ParserT s e m (Elem s)
letter = satisfy isAlpha <?> "alpha"

letter0
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsAlpha (Elem s)
  ) => ParserT s e m [Elem s]
letter0 = many letter

letter1
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsAlpha (Elem s)
  ) => ParserT s e m [Elem s]
letter1 = some letter

digit
  :: (Parsable s,
      ParserError e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsDigit (Elem s)
   ) => ParserT s e m (Elem s)
digit = satisfy isDigit <?> "digit"

digit0
 :: (Parsable s,
     ParserError e,
     Monoid e,
     Positioned (Elem s),
     Monad m,
     Show (Elem s),
     IsDigit (Elem s)
 ) => ParserT s e m [Elem s]
digit0 = many digit

digit1
 :: (Parsable s,
     ParserError e,
     Monoid e,
     Positioned (Elem s),
     Monad m,
     Show (Elem s),
     IsDigit (Elem s)
 ) => ParserT s e m [Elem s]
digit1 = some digit
