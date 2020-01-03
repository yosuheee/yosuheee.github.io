module Primitive (
  p_char,
  p_string,
  DblLiteral,
  IntLiteral,
  p_dbl_literal,
  p_int_literal,
  p_identity
) where

import Text.Parsec
import Text.Parsec.String

import Primitive.Char
import Primitive.Double
import Primitive.Identity
import Primitive.Integer
import Primitive.String
