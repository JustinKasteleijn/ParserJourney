{-# LANGUAGE FlexibleInstances #-}

module ParserJourney.Combinators.LookAheadCombinators where

import           ParserJourney.Parsable    (Parsable)
import           ParserJourney.Parser      (ParserT)
import           ParserJourney.ParserError (ParserError)

notFollowedBy
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m
  ) => ParserT s e m ()
notFollowedBy = undefined
