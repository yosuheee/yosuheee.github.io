module Expression where

import Data.Function ((&))
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
  ExInteger Integer |
  ExDouble Double |
  ExIdentity String |
  ExList (Expression, [(String, Expression)]) |
  ExTernary (Expression, Expression, Expression) |
  ExThrow Expression
  deriving (Show)

p_expression :: Parser Expression
p_expression = p_priority_16

p_primitive :: Parser Expression
p_primitive = do
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

p_priority_15 =
  foldl (&) p_primitive $
    map p_binops [
      ["*", "/", "%"],
      ["+", "-"],
      ["<<", ">>"],
      ["<=>"],
      ["<=", ">=", "<", ">"],
      ["==", "!="],
      ["&"], ["^"], ["|"], ["&&"], ["||"]]

p_priority_16 =
  p_ternary <|>
  p_throw <|>
  p_binops ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="] p_priority_15

p_binops :: [String] -> Parser Expression -> Parser Expression
p_binops ops p_high_priority = try $ do
  lft <- p_high_priority
  rgt <- many . try $ spaces *> rest
  return $
    case length rgt of
      0 -> lft
      _ -> ExList (lft, rgt)
  where
    (x : xs) = map (try . string) ops
    rest = do
      o <- foldl (<|>) x xs
      r <- try $ spaces *> p_high_priority
      return (o, r)

p_ternary :: Parser Expression
p_ternary = try $ do
  fst <- p_priority_15
  spaces >> char '?' >> spaces
  snd <- p_priority_15
  spaces >> char ':' >> spaces
  trd <- p_priority_15
  return $ ExTernary (fst, snd, trd)

p_throw :: Parser Expression
p_throw = try $ do
  string "throw"
  skipMany1 space
  fst <- p_priority_15
  return $ ExThrow fst
