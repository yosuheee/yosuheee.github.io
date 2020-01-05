module Primitive.Integer where

import Data.Char (ord, toUpper)
import Text.Parsec
import Text.Parsec.String

import Util

data IntSuffix = ISNone | ISU | ISL | ISLU | ISLL | ISLLU deriving Show

data IntLiteral =
  IBin IntSuffix String |
  IOct IntSuffix String |
  IDec IntSuffix String |
  IHex IntSuffix String
  deriving Show

p_integer :: Parser Integer
p_integer = do
  int <- p_int_literal
  ___
  return $
    case int of
      IBin _ s -> from_bin s
      IOct _ s -> from_oct s
      IDec _ s -> from_dec s
      IHex _ s -> from_hex s

to_int :: Char -> Integer
to_int c = fromIntegral $ ord c

from_bin :: String -> Integer
from_bin = foldl (\a c -> a * 2 + to_int c - 48) 0

from_oct :: String -> Integer
from_oct = foldl (\a c -> a * 8 + to_int c - 48) 0

from_dec :: String -> Integer
from_dec = foldl (\a c -> a * 10 + to_int c - 48) 0

from_hex :: String -> Integer
from_hex = flip foldl 0
  (\a c -> a * 16 + (if toUpper c >= 'A' then to_int (toUpper c) - 55 else to_int c - 48))

p_int_literal :: Parser IntLiteral
p_int_literal = p_bin <|> p_hex <|> p_oct <|> p_dec

p_int_suffix :: Parser IntSuffix
p_int_suffix = do
  let
    target = [
      (ISLLU, [ "llu", "ull", "llU", "ulL", "lLu", "uLl", "lLU", "uLL",
                "Llu", "Ull", "LlU", "UlL", "LLu", "ULl", "LLU", "ULL" ]),
      (ISLU, [ "lu", "ul", "lU", "uL", "Lu", "Ul", "LU", "UL" ]),
      (ISLL, [ "ll", "lL", "Ll", "LL" ]),
      (ISL, [ "l", "L" ]),
      (ISU, [ "u", "U" ]) ]
    make :: (IntSuffix, [String]) -> Parser IntSuffix
    make (typ, lst) = try $ do
      let (x : xs) = map (try . string) lst
      foldl (<|>) x xs
      return typ
  foldr (<|>) (return ISNone) $ 
    map make target

p_int_make :: (IntSuffix -> String -> IntLiteral) -> String -> Parser Char -> Parser IntLiteral
p_int_make typ pre chr = try $ do
  string pre
  rest <- many1 chr
  suff <- p_int_suffix
  return $ typ suff rest

p_bin :: Parser IntLiteral
p_bin = p_int_make IBin "0b" (oneOf "01")

p_hex :: Parser IntLiteral
p_hex = p_int_make IHex "0x" hexDigit

p_oct :: Parser IntLiteral
p_oct = p_int_make IOct "0" octDigit

p_dec :: Parser IntLiteral
p_dec = p_int_make IDec "" digit
