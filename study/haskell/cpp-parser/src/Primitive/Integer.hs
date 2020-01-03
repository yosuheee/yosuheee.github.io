module Primitive.Integer where

import Text.Parsec
import Text.Parsec.String

data IntSuffix = ISNone | ISU | ISL | ISLU | ISLL | ISLLU deriving Show

data IntLiteral =
  IBin IntSuffix String |
  IOct IntSuffix String |
  IDec IntSuffix String |
  IHex IntSuffix String
  deriving Show

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
