module Primitive.Boolean where

import Text.Parsec
import Text.Parsec.String

import Primitive.Identity (p_identity_char_and_digit)

p_boolean :: Parser Bool
p_boolean = try $ do
  bol <- string "true" <|> string "false"
  notFollowedBy p_identity_char_and_digit
  return $ if bol == "true" then True else False
