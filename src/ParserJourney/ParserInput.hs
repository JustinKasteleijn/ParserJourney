{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module ParserJourney.ParserInput where

data Position = Position {
    col  :: Int,
    line :: Int
} deriving (Eq)

instance Show Position where
  show :: Position -> String
  show (Position col' line') = show line' ++ ":" ++ show col'

class Positioned a where
  advancePos :: Position -> a -> Position
  isNewline  :: a -> Bool

instance Positioned Char where
  advancePos :: Position -> Char -> Position
  advancePos (Position c l) n
    | isNewline n = Position 1 (l + 1)
    | otherwise   = Position (c + 1) l

  isNewline :: Char -> Bool
  isNewline '\n' = True
  isNewline _    = False

data ParserState s = ParserState {
    input :: s,
    pos   :: Position
} deriving (Show, Eq)

initialParserState :: (Monoid s) => ParserState s
initialParserState = ParserState mempty (Position 1 1)

parserState :: s -> ParserState s
parserState st = ParserState st (Position 1 1)
