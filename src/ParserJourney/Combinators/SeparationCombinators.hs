{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.SeparationCombinators where

import           Control.Applicative                       (Alternative (..),
                                                            optional)
import           ParserJourney.Combinators.CharCombinators (newline)
import           ParserJourney.Parsable                    (Parsable (Elem))
import           ParserJourney.Parser                      (ParserT)
import           ParserJourney.ParserError                 (ParserError)
import           ParserJourney.ParserInput                 (Positioned)

sepBy1
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s)
  ) => ParserT s e m a
    -> ParserT s e m sep
    -> ParserT s e m [a]
sepBy1 p psep = (:) <$> p <*> many (psep >> p)

sepBy0
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s)
  ) => ParserT s e m a
    -> ParserT s e m sep
    -> ParserT s e m [a]
sepBy0 p psep = sepBy1 p psep <|> pure []

sepEndBy1
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s)
  ) => ParserT s e m a
    -> ParserT s e m sep
    -> ParserT s e m [a]
sepEndBy1 p psep = sepBy1 p psep <* optional psep

sepEndBy0
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s)
  ) => ParserT s e m a
    -> ParserT s e m sep
    -> ParserT s e m [a]
sepEndBy0 p psep = sepBy0 p psep <* optional psep

lines
  :: (Parsable s,
      ParserError e ,
      Monoid e,
      Monad m,
      Show (Elem s),
      Positioned (Elem s)
  ) => ParserT s e m a
    -> ParserT s e m [a]
lines line = line `sepEndBy1` newline
