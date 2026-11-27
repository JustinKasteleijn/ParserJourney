{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module ParserJourney.Combinators.StringConvenienceCombinators where

import           ParserJourney.Combinators.BasicCombinators
import           ParserJourney.Combinators.CharCombinators  (char)
import           ParserJourney.Parsable                     (Parsable (Elem))
import           ParserJourney.Parser                       (ParserT)
import           ParserJourney.ParserError                  (ParserError)

parens
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Elem s ~ Char
  ) => ParserT s e m a
    -> ParserT s e m a
parens = between (char '(') (char ')')

bracket
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Elem s ~ Char
  ) => ParserT s e m a
    -> ParserT s e m a
bracket = between (char '[') (char ']')

brace
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Elem s ~ Char
  ) => ParserT s e m a
    -> ParserT s e m a
brace = between (char '{') (char '}')
