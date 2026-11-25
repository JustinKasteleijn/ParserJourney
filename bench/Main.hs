
module Main where

import Criterion.Main
import ParserJourney.Combinators
import ParserJourney.Parser
import Data.Functor.Identity (runIdentity)
import Control.Applicative
import Control.DeepSeq (force)
import Control.Exception (evaluate)

main :: IO ()
main = do
  input' <- evaluate (force input)
  defaultMain
    [ bench "my parser" $
        nf parseExample input'
    ]

input :: String
input = replicate 100000 'a'

parseExample :: String -> String
parseExample inp =
  case runIdentity (parse (many item) inp) of
    Left err      -> error err
    Right (r', _) -> r'
