{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.RepetitionCombinators where
import qualified Control.Applicative       as ContrlAppl
import           ParserJourney.Parsable    (Parsable)
import           ParserJourney.Parser      (ParserT)
import           ParserJourney.ParserError (ParserError)

many
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m
  ) => ParserT s e m a
    -> ParserT s e m [a]
many = ContrlAppl.many

some
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m
  ) => ParserT s e m a
    -> ParserT s e m [a]
some = ContrlAppl.some
