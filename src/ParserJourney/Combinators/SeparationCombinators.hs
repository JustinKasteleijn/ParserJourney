{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.SeparationCombinators where

import           Control.Applicative       (Alternative (..))
import           ParserJourney.Parsable    (Parsable (Elem))
import           ParserJourney.Parser      (ParserT)
import           ParserJourney.ParserError (ParserError)

sepBy1
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s)
  ) => ParserT s e m sep
    -> ParserT s e m (Elem s)
    -> ParserT s e m [Elem s]
sepBy1 psep p = (:) <$> p <*> many (psep >> p)

sepBy0
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s)
  ) => ParserT s e m sep
    -> ParserT s e m (Elem s)
    -> ParserT s e m [Elem s]
sepBy0 psep p = sepBy1 psep p <|> pure []
