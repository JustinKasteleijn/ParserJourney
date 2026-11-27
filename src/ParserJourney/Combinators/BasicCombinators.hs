{-# LANGUAGE FlexibleContexts #-}

module ParserJourney.Combinators.BasicCombinators (
    (<?>),
    item,
    satisfy,
    between,
    optional,
    choice,
    eof
) where

import qualified Control.Applicative                            as ControlApplicative
import           ParserJourney.Combinators.LookAheadCombinators (notFollowedBy)
import           ParserJourney.Parsable                         (Parsable (Elem, uncons))
import           ParserJourney.Parser                           (ParserT (..),
                                                                 getPosition,
                                                                 throwError)
import           ParserJourney.ParserError                      (ParserError (addLabel, unexpectedEOF, unexpectedInput))
import           ParserJourney.ParserInput                      (ParserState (ParserState),
                                                                 Positioned (advancePos))

infixr 0 <?>
(<?>)
  :: (ParserError e,
      Monad m)
  => ParserT s e m a
  -> String
  -> ParserT s e m a
p <?> label = ParserT $ \inp -> do
                 res <- runParser p inp
                 case res of
                   Left err -> pure $ Left $ addLabel label err
                   Right x  -> pure $ Right x

item
  :: (Parsable s,
      ParserError e,
      Positioned (Elem s),
      Applicative f
     )
  => ParserT s e f (Elem s)
item = ParserT $ \(ParserState s p)
  -> case uncons s of
       Nothing      -> pure $ Left $ unexpectedEOF p
       Just (x, s') ->
         let pos' = advancePos p x
          in pure $ Right (x, ParserState s' pos')

satisfy
  :: (Parsable s,
      ParserError e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s)
     )
  => (Elem s -> Bool)
  -> ParserT s e m (Elem s)
satisfy f = do
  p <- getPosition
  x <- item
  if f x
     then pure x
     else throwError $ unexpectedInput p (show x)

optional
  :: (Monoid e,
      Monad m
  ) => ParserT s e m a
    -> ParserT s e m (Maybe a)
optional = ControlApplicative.optional

choice
  :: (Monoid e,
      Monad m
  ) => [ParserT s e m a]
    -> ParserT s e m a
choice = ControlApplicative.asum

between
  :: (Monad m)
  => ParserT s e m open
  -> ParserT s e m close
  -> ParserT s e m a
  -> ParserT s e m a
between pOpen pClose p = pOpen *> p <* pClose

eof
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Show (Elem s),
      Positioned (Elem s)
  ) => ParserT s e m ()
eof = notFollowedBy item <?> "eof"
