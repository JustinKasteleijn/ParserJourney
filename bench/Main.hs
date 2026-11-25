{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Control.Applicative
import           Control.DeepSeq           (NFData (rnf), force)
import           Control.Exception         (evaluate)
import           Criterion.Main
import           Data.Functor.Identity     (runIdentity)
import           ParserJourney.Combinators
import           ParserJourney.Parser
import           ParserJourney.ParserInput
import           Prelude                   hiding (lines)

main :: IO ()
main = do
  input' <- evaluate (force inputt)
  defaultMain
    [ bench "my parser" $
        nf parseExample input'
    ]

inputt :: String
inputt = concat (replicate 10000 "1,2,3,4,5,6,7,8,9")


instance NFData s => NFData (ParserState s) where
  rnf (ParserState inp position) =
    rnf inp `seq` rnf position

instance NFData Position where
  rnf (Position line column) =
    rnf line `seq` rnf column

parseExample :: String -> ([Integer], ParserState String)
parseExample inp =
  case runIdentity (parse (lines integer) inp) of
    Left err          -> error err
    Right (r', state) -> (r', state)
