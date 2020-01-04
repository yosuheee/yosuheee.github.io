module Primitive.Double where

import Text.Parsec
import Text.Parsec.String

data DblSuffix = DSNone | DSf | DSF | DSl | DSL deriving Show

data DblLiteral = DblLiteral DblSuffix String String deriving Show

p_double :: Parser Double
p_double = do
  str <- p_dbl_literal
  spaces
  let DblLiteral _ fst snd = str
      a = read fst :: Double
      x = read snd :: Double
  return $ a * (10 ** x)

p_dbl_literal :: Parser DblLiteral
p_dbl_literal =
  p_all_double <|> p_front_double <|> p_simple_double

p_double_suffix :: Parser DblSuffix
p_double_suffix = try $ do
  let
    make :: (DblSuffix, String) -> Parser DblSuffix
    make (typ, str) = try $ do
      string str
      return typ
  foldr (<|>) (return DSNone) $ 
    map make [ (DSf, "f"), (DSF, "F"), (DSl, "l"), (DSL, "L") ]

p_simple_double :: Parser DblLiteral
p_simple_double = try $ do
  fst <- many1 digit
  snd <- p_exponent
  suf <- p_double_suffix
  return $ DblLiteral suf fst snd

p_front_double :: Parser DblLiteral
p_front_double = try $ do
  fst <- many1 digit
  char '.'
  snd <- option "0" p_exponent
  suf <- p_double_suffix
  return $ DblLiteral suf fst snd

p_all_double :: Parser DblLiteral
p_all_double = try $ do
  fst <- option "0" $ many1 digit
  char '.'
  snd <- many1 digit
  thd <- option "0" p_exponent
  suf <- p_double_suffix
  return $ DblLiteral suf (fst ++ "." ++ snd) thd

p_exponent :: Parser String
p_exponent = try $ do
  oneOf "eE"
  sign <- oneOf "+-" <|> return '+'
  rest <- many1 digit
  return $ (if sign == '+' then "" else "-") ++ rest
