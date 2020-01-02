module Expression where

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

p_number :: Parser Integer
p_number = do
  str <- many1 digit
  return $ read str

data Expression =
  ExTuple (Expression, Char, Expression) |
  ExInteger Integer |
  ExDouble Double |
  ExIdentity String |
  ExList (Expression, [(String, Expression)])
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

p_mul_div_rest :: Parser (String, Expression)
p_mul_div_rest = do
  op <- string "*" <|> string "/"
  rgt <- try $ spaces *> p_expression_primitive
  return (op, rgt)

p_mul_div :: Parser Expression
p_mul_div = do
  lft <- p_expression_primitive
  rgt <- many . try $ spaces *> p_mul_div_rest
  return $
    case length rgt of
      0 -> lft
      _ -> ExList (lft, rgt)

p_add_sub_rest :: Parser (String, Expression)
p_add_sub_rest = do
  op <- string "+" <|> string "-"
  rgt <- try $ spaces *> p_mul_div
  return (op, rgt)

p_add_sub :: Parser Expression
p_add_sub = do
  lft <- p_mul_div
  rgt <- many . try $ spaces *> p_add_sub_rest
  return $
    case length rgt of
      0 -> lft
      _ -> ExList (lft, rgt)
