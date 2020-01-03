module Primitive (
  p_char,
  p_double,
  p_integer,
  p_string,
  p_identity
) where

import Text.Parsec
import Text.Parsec.String

import Primitive.Char
import Primitive.Double
import Primitive.Identity
import Primitive.Integer
import Primitive.String
