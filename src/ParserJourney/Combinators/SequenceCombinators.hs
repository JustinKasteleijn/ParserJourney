{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module ParserJourney.Combinators.SequenceCombinators where

import           ParserJourney.Combinators.CharCombinators (char, token)
import           ParserJourney.Parsable                    (Parsable (Elem))
import           ParserJourney.Parser                      (ParserT)
import           ParserJourney.ParserError                 (ParserError)
import           ParserJourney.ParserInput                 (Positioned)

tokens
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Eq (Elem s),
      Positioned (Elem s),
      Monad m,
      Show (Elem s)
  ) => [Elem s]
    -> ParserT s e m [Elem s]
tokens = mapM token

string
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Elem s ~ Char,
      Positioned (Elem s),
      Monad m,
      Show (Elem s)
  ) => String
    -> ParserT s e m [Elem s]
string = mapM char
