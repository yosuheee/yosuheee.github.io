module Main where

import Text.Parsec
import Text.Parsec.String
import Expression

main :: IO ()
main = do
  input <- getLine
  case parse p_expression "Parser" input of
    Left  err -> print err
    Right val -> print val
