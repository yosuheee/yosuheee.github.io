module Main where

import Text.Parsec
import Lib

main :: IO ()
main = do
  input <- readFile "test/main.cpp"
  case parse p_main "Parser" input of
    Left  err -> print err
    Right val -> print val
