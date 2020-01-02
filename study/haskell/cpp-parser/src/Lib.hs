module Lib where

import Text.Parsec
import Text.Parsec.String

p_identity :: Parser String
p_identity = do
  fst <- p_identity_char
  snd <- many p_identity_and_digit_char
  return $ [fst] ++ snd

p_underscore :: Parser Char
p_underscore = char '_'

p_identity_char :: Parser Char
p_identity_char = p_underscore <|> letter

p_identity_and_digit_char :: Parser Char
p_identity_and_digit_char = p_identity_char <|> digit

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

p_blank :: Parser Char
p_blank = char ' '

p_lf :: Parser Char
p_lf = char '\n'

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
  skipMany p_blank
  p_lf
  return file

p_return_string :: Parser String
p_return_string = string "return"

p_number :: Parser Integer
p_number = do
  str <- many1 digit
  return $ read str

p_semicolon :: Parser Char
p_semicolon = char ';'

p_return :: Parser Integer
p_return = do
  p_return_string
  spaces
  num <- p_number
  spaces
  p_semicolon
  return num

p_function_name :: Parser String
p_function_name = string "main"

p_block_start :: Parser Char
p_block_start = char '{'

p_block_end :: Parser Char
p_block_end = char '}'

p_block :: Parser Integer
p_block = do
  p_block_start
  spaces
  num <- p_return
  spaces
  p_block_end
  return num

p_bracket_start :: Parser Char
p_bracket_start = char '('

p_bracket_end :: Parser Char
p_bracket_end = char ')'

p_function :: Parser Integer
p_function = do
  p_identity
  spaces
  p_function_name
  spaces
  p_bracket_start
  spaces
  p_bracket_end
  spaces
  p_block

p_using_string :: Parser String
p_using_string = string "using"

p_namespace_string :: Parser String
p_namespace_string = string "namespace"

p_std_string :: Parser String
p_std_string = string "std"

p_namespace :: Parser String
p_namespace = do
  p_using_string
  spaces
  p_namespace_string
  spaces
  name <- p_std_string
  spaces
  p_semicolon
  return name

p_main :: Parser Integer
p_main = do
  p_include
  spaces
  p_namespace
  spaces
  p_function

p_var_defined :: Parser (String, String, Expression)
p_var_defined = do
  typ <- p_identity
  spaces
  name <- p_identity
  spaces
  char '='
  spaces
  value <- p_expression
  spaces
  p_semicolon
  return (typ, name, value)

data Expression = ExTuple (Expression, Char, Expression)
                | ExInteger Integer
                | ExDouble Double
                | ExIdentity String
                  deriving (Show)

p_expression :: Parser Expression
p_expression = do
  try p_expression_tuple
  <|> try p_expression_primitive

p_expression_tuple :: Parser Expression
p_expression_tuple = do
  lft <- p_expression_primitive
  spaces
  sign <- oneOf "+-*/"
  spaces
  rgt <- p_expression_primitive
  return $ ExTuple (lft, sign, rgt)

p_expression_primitive :: Parser Expression
p_expression_primitive = do
  try p_expression_double <|> try p_expression_identity <|> try p_expression_integer

p_expression_integer :: Parser Expression
p_expression_integer = do
  num <- p_number
  return $ ExInteger num

p_expression_identity :: Parser Expression
p_expression_identity = do
  ident <- p_identity
  return $ ExIdentity ident

p_expression_double :: Parser Expression
p_expression_double = do
  fst <- many1 digit
  char '.'
  snd <- many1 digit
  return . ExDouble . read $ fst ++ "." ++ snd

p_func :: Parser (String, [Expression])
p_func = do
  name <- p_identity
  spaces
  char '('
  args <- sepEndBy p_expression p_argument_separate
  return (name, args)

p_argument_separate :: Parser ()
p_argument_separate = do
  char ','
  spaces
  return ()
