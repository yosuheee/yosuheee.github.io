module Primitive (
  ChrLiteral,
  DblLiteral,
  IntLiteral,
  StrLiteral,
  p_chr_literal,
  p_dbl_literal,
  p_int_literal,
  p_str_literal,
  p_identity,
  p_number
) where

import Text.Parsec
import Text.Parsec.String

import Primitive.Char
import Primitive.Double
import Primitive.Identity
import Primitive.Integer
import Primitive.String

p_number :: Parser Integer
p_number = do
  str <- many1 digit
  return $ read str
