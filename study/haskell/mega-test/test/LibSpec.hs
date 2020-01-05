{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

import Lib

spec :: Spec
spec = do
  it "" $ 1 `shouldBe` 1

type Parser = Parsec Void Text

run p input = parseTest p input

mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)
