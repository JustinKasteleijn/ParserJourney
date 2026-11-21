{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.LookAheadCombinators where

import           ParserJourney.Parsable    (Parsable)
import           ParserJourney.Parser      (ParserT (..), runParser)
import           ParserJourney.ParserError (ParserError (unexpectedMatch))

notFollowedBy
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show a
  ) => ParserT s e m a
    -> ParserT s e m ()
notFollowedBy p = ParserT $ \s -> do
                     res <- runParser p s
                     case res of
                          Left  _      -> pure $ Right ((), s)
                          Right (x, _) -> pure $ Left $ unexpectedMatch (show x)
