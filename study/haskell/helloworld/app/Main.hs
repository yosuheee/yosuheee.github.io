module Main where

import Text.Parsec (parse)

import Lib (parseInteger)

main :: IO ()
main = do
    input <- getLine
    case parse parseInteger "Parser" input of
        Left  err -> putStrLn $ show err
        Right val -> putStrLn $ show val

