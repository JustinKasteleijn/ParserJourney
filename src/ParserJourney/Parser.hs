{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE KindSignatures   #-}

module ParserJourney.Parser
  ( ParserT(ParserT)
  , runParser
  , Parser
  , parse
  , throwError
  , getPosition
  ) where

import           Control.Applicative       (Alternative (..))
import           Data.Functor.Identity     (Identity)
import           Data.Kind                 (Type)
import           ParserJourney.ParserInput (ParserState (..), Position,
                                            parserState)

newtype ParserT s e (m :: Type -> Type) a
  = ParserT { runParser :: ParserState s -> m (Either e (a, ParserState s)) }

type Parser s a = ParserT s String Identity a

instance (Monad f) => Functor (ParserT s e f) where
  fmap :: (a -> b) -> ParserT s e f a -> ParserT s e f b
  fmap f p = ParserT $ \s -> do
                 res <- runParser p s
                 case res of
                   Right (x, ps) -> pure $ Right (f x, ps)
                   Left err      -> pure $ Left err

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

instance (Monad f, Monoid e) => Alternative (ParserT s e f) where
  empty :: ParserT s e f a
  empty = ParserT $ \_ -> pure $ Left mempty

  (<|>) :: ParserT s e f a -> ParserT s e f a -> ParserT s e f a
  px <|> py = ParserT $ \inp -> do
    rx <- runParser px inp
    case rx of
      Right x  -> pure (Right x)
      Left err -> do
        ry <- runParser py inp
        case ry of
          Right y   -> pure (Right y)
          Left err' -> pure $ Left $ err <> err'

throwError
  :: (Applicative f)
  => e
  -> ParserT s e f a
throwError err = ParserT $  \_ -> pure $ Left err

getPosition
  :: (Applicative f)
  => ParserT s e f Position
getPosition = ParserT $ \(ParserState st p)
                -> pure $ Right (p, ParserState st p)

parse
  :: ParserT s e m a
  -> s
  -> m (Either e (a, ParserState s))
parse p inp = runParser p (parserState inp)
