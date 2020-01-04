module Primitive.Identity where

import Text.Parsec
import Text.Parsec.String

p_identity :: Parser String
p_identity = do
  fst <- p_identity_char
  snd <- many p_identity_char_and_digit
  return $ [fst] ++ snd

p_underscore :: Parser Char
p_underscore = char '_'

p_identity_char :: Parser Char
p_identity_char = p_underscore <|> letter

p_identity_char_and_digit :: Parser Char
p_identity_char_and_digit = p_identity_char <|> digit
