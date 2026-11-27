{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           Control.Applicative       hiding (some)
import           Control.DeepSeq           (NFData (rnf), force)
import           Control.Exception         (evaluate)
import           Criterion.Main
import           Data.Functor.Identity     (runIdentity)
import           Data.List                 (intercalate)
import           ParserJourney.Combinators
import           ParserJourney.Parser
import           ParserJourney.ParserInput
import           Prelude                   hiding (lines)

main :: IO ()
main = do
  input' <- evaluate (force inputt)
  defaultMain
    [ bench "Array example of 100.000 elements" $
        nf parseExample input'
    ]

inputt :: String
inputt = "[" ++ intercalate "," (replicate 10000 "1.0,2.2,3.31482434241") ++ "]"

instance NFData s => NFData (ParserState s) where
  rnf (ParserState inp position) =
    rnf inp `seq` rnf position

instance NFData Position where
  rnf (Position line column) =
    rnf line `seq` rnf column

parseExample :: String -> ([Double], ParserState String)
parseExample inp =
  case runIdentity (parse (bracket (decimal `sepBy1` char ',')) inp) of
    Left err          -> error err
    Right (r', state) -> (r', state)
