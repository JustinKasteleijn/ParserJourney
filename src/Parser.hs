{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser
  ( ParserT
  , Parser
  , parse
  , item
  , satisfy
  , throwError
  ) where

import Parsable (Parsable(..))
import ParserError (ParserError (..))
import ParserInput (ParserState(..), Positioned (advancePos), parserState)
import Data.Kind (Type)
import Data.Functor.Identity (Identity)

newtype ParserT s e (m :: Type -> Type) a
  = ParserT { runParser :: ParserState s -> m (Either e (a, ParserState s)) }

type Parser s a = ParserT s String Identity a

instance (Monad f) => Functor (ParserT s e f) where
  fmap :: (a -> b) -> ParserT s e f a -> ParserT s e f b
  fmap f p = pure f <*> p

instance (Monad f) => Applicative (ParserT s e f) where
  pure :: a -> ParserT s e f a
  pure x = ParserT $ \s -> pure $ Right (x, s)

  (<*>) :: ParserT s e f (a -> b) -> ParserT s e f a -> ParserT s e f b
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Monad f => Monad (ParserT s e f) where
  (>>=) :: ParserT s e f a -> (a -> ParserT s e f b) -> ParserT s e f b
  px >>= f = ParserT $ \s -> do
              res <- runParser px s
              case res of
                Right (x, s') -> runParser (f x) s'
                Left err      -> pure $ Left err

throwError
  :: (ParserError e, Applicative f)
  => e
  -> ParserT s e f (Elem s)
throwError err = ParserT $  \_ -> pure $ Left err

item
  :: (Parsable s,
      ParserError e,
      Positioned (Elem s),
      Applicative f
     )
  => ParserT s e f (Elem s)
item = ParserT $ \(ParserState s p)
  -> case uncons s of
       Nothing      -> pure $ Left unexpectedEOF
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
  x <- item
  if f x
     then pure x
     else throwError $ satisfyError (show x)

parse
  :: ParserT s e m a
  -> s
  -> m (Either e (a, ParserState s))
parse p inp = runParser p (parserState inp)
