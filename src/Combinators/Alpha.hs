{-# LANGUAGE FlexibleContexts #-}

module Combinators.Alpha where

import           Control.Applicative (Alternative (many, some))
import           Parsable            (Parsable (Elem))
import           Parser              (ParserT, satisfy, (<?>))
import           ParserError         (ParserError)
import           ParserInput         (Positioned)
import           TypeClasses.IsAlpha (IsAlpha (isAlpha))

alpha
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsAlpha (Elem s)
  ) => ParserT s e m (Elem s)
alpha = satisfy isAlpha <?> "alpha"

alpha0
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsAlpha (Elem s)
  ) => ParserT s e m [Elem s]
alpha0 = many alpha

alpha1
  :: (Parsable s,
      ParserError e,
      Monoid e,
      Positioned (Elem s),
      Monad m,
      Show (Elem s),
      IsAlpha (Elem s)
  ) => ParserT s e m [Elem s]
alpha1 = some alpha
