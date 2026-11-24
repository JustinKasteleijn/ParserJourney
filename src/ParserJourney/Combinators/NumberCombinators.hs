{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ParserJourney.Combinators.NumberCombinators where

import           Control.Applicative                       ((<|>))
import           Data.Data                                 (Proxy (Proxy))
import           Data.Int                                  (Int16, Int32, Int64,
                                                            Int8)
import           Data.List                                 (foldl')
import           Data.Word                                 (Word16, Word32,
                                                            Word64, Word8)
import           ParserJourney.Combinators.CharCombinators (digit1, minus)
import           ParserJourney.Parsable                    (Parsable (..))
import           ParserJourney.Parser                      (ParserT, throwError)
import           ParserJourney.ParserError                 (ParserError (greatherThanMaxBound, lesserThanMinbound))
import           ParserJourney.ParserInput                 (Positioned)
import           ParserJourney.TypeClasses.ToDigit         (ToDigit (toDigit))

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
  f <- negate <$ minus <|> pure id
  f <$> foldDigits
 where
   foldDigits :: ParserT s e m Integer
   foldDigits
     = foldl' (\ n d -> n * 10 + fromIntegral (toDigit d)) 0 <$> digit1

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
