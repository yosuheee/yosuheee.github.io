module Primitive (
  p_char,
  DblLiteral,
  IntLiteral,
  StrLiteral,
  p_dbl_literal,
  p_int_literal,
  p_str_literal,
  p_identity
) where

import Text.Parsec
import Text.Parsec.String

import Primitive.Char
import Primitive.Double
import Primitive.Identity
import Primitive.Integer
import Primitive.String
