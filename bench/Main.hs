
module Main where

import Prelude hiding (lines)
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
input = "12\n13\n14\n15\n"

parseExample :: String -> [Integer]
parseExample inp =
  case runIdentity (parse (lines integer) inp) of
    Left err      -> error err
    Right (r', _) -> r'
