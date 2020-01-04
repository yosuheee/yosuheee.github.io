module Primitive.Boolean where

import Text.Parsec
import Text.Parsec.String

p_boolean :: Parser Bool
p_boolean = do
  bol <- string "true" <|> string "false"
  return $ if bol == "true" then True else False
