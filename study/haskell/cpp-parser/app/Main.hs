module Main where

import Text.Parsec
import Text.Parsec.String

import Statement

main :: IO ()
main = do
  input <- getLine
  case parse p_statement "" input of
    Left err -> putStrLn $ show err
    Right val -> putStrLn $ show val
