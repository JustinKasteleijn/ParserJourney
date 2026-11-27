{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ParserJourney.Combinators.NumberCombinators (
    integer,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    parseBounded,
    decimal,
    nat
) where

import           Control.Applicative                          ((<|>))
import           Data.Data                                    (Proxy (Proxy))
import           Data.Int                                     (Int16, Int32,
                                                               Int64, Int8)
import           Data.List                                    (foldl')
import           Data.Word                                    (Word16, Word32,
                                                               Word64, Word8)
import           ParserJourney.Combinators.BasicCombinators   (optional,
                                                               satisfy, (<?>))
import           ParserJourney.Combinators.CharCombinators    (digit0, digit1,
                                                               minus)
import           ParserJourney.Parsable                       (Parsable (..))
import           ParserJourney.Parser                         (ParserT,
                                                               getPosition,
                                                               throwError)
import           ParserJourney.ParserError                    (ParserError (greatherThanMaxBound, lesserThanMinbound, unexpectedInput))
import           ParserJourney.ParserInput                    (Positioned)
import           ParserJourney.TypeClasses.IsDecimalSeparator (IsDecimalSeparator (isDecimalSeparator))
import           ParserJourney.TypeClasses.ToDigit            (ToDigit (toDigit))

integer
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Integer
integer = do
  sign <- negate <$ minus <|> pure id
  sign <$> _foldDigitsParser

parseBounded
  :: forall s e m a.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s),
      Bounded  a,
      Integral a,
      Show a
  ) => ParserT s e m a
parseBounded = do
  n <- integer
  let max' = toInteger (maxBound :: a)
      min' = toInteger (minBound :: a)
  case (n > max', n < min') of
    (True, _) -> throwError $ greatherThanMaxBound (Proxy @a) n
    (_, True) -> throwError $ lesserThanMinbound (Proxy @a) n
    _         -> pure (fromInteger n)

u8
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Word8
u8 = parseBounded

u16
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Word16
u16 = parseBounded

u32
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Word32
u32 = parseBounded

u64
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Word64
u64 = parseBounded

i8
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Int8
i8 = parseBounded

i16
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Int16
i16 = parseBounded

i32
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Int32
i32 = parseBounded

i64
  :: forall s e m.
     (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Int64
i64 = parseBounded

nat
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Integer
nat = do
  p <- getPosition
  i <- integer
  if i < 0
     then throwError (unexpectedInput p (show i)) <?> "nat"
     else pure i

decimal
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s),
      IsDecimalSeparator (Elem s)
  ) => ParserT s e m Double
decimal = (do
  sign    <- negate <$ minus <|> pure id
  whole   <- nat
  mDigits <- satisfy isDecimalSeparator
                *> optional digit0
  let fraction = case mDigits of
         Just digits -> fromIntegral (_foldDigits digits) / (10 ^ length digits)
         Nothing     -> 0
      value    = fromIntegral whole + fraction
  pure (sign value)) <?> "decimal"

_foldDigits :: ToDigit a => [a] -> Integer
_foldDigits = foldl' (\n d -> n * 10 + fromIntegral (toDigit d)) 0

_foldDigitsParser
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Monad m,
      Positioned (Elem s),
      Show (Elem s),
      ToDigit (Elem s)
  ) => ParserT s e m Integer
_foldDigitsParser
  = _foldDigits <$> digit1
