{-# LANGUAGE FlexibleContexts #-}

module Combinators.Digit where

import           Control.Applicative (Alternative (many, some))
import           Parsable            (Parsable (Elem))
import           Parser              (ParserT, satisfy, (<?>))
import           ParserError         (ParserError)
import           ParserInput         (Positioned)
import           TypeClasses.IsDigit (IsDigit (isDigit))

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
