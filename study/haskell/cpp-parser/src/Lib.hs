module Lib where

import Text.Parsec
import Text.Parsec.String

p_sharp :: Parser Char
p_sharp = char '#'

p_include_string :: Parser String
p_include_string = string "include"

p_lt :: Parser Char
p_lt = char '<'

p_gt :: Parser Char
p_gt = char '>'

p_not_gt_or_blank :: Parser Char
p_not_gt_or_blank = noneOf "> "

p_include :: Parser String
p_include = do
  p_sharp
  spaces
  p_include_string
  spaces
  p_lt
  spaces
  file <- many1 p_not_gt_or_blank
  spaces
  p_gt
  return file
