{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE KindSignatures   #-}

module Parser
  ( ParserT
  , Parser
  , parse
  , item
  , satisfy
  , throwError
  , getPosition
  , (<?>)
  ) where

import           Control.Applicative   (Alternative (..))
import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           Parsable              (Parsable (..))
import           ParserError           (ParserError (..))
import           ParserInput           (ParserState (..), Position,
                                        Positioned (advancePos), parserState)

import           TypeClasses.IsAlpha   (IsAlpha (isAlpha))
import           TypeClasses.IsDigit   (IsDigit (isDigit))

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
     else throwError $ unexpectedInput (show x)
