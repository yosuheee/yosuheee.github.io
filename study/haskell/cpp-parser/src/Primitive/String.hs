module Primitive.String where

import Text.Parsec
import Text.Parsec.String

import Primitive.Char (p_escaped_char)

data StrPrefix =
  SPu8 | SPL | SPu | SPU | 
  SPR | SPu8R | SPLR | SPuR | SPUR | 
  StNone deriving Show

data StrSuffix =
  SSs | SSNone deriving Show

data StrLiteral = StrLiteral StrPrefix StrSuffix String deriving Show

p_string :: Parser String
p_string = do
  str <- p_str_literal
  spaces
  let StrLiteral _ _ s = str
  return s

p_str_literal :: Parser StrLiteral
p_str_literal = try $ do
  pref <- p_str_prefix
  char '"'
  str <- p_escaped_string
  char '"'
  suff <- p_str_suffix
  return $ StrLiteral pref suff str

p_str_prefix :: Parser StrPrefix
p_str_prefix = do
  let
    target = [
      (SPu8R, ["u8R"]), (SPLR, ["LR"]), (SPuR, ["uR"]), (SPUR, ["UR"]),
      (SPu8, ["u8"]), (SPL, ["L"]), (SPu, ["u"]), (SPU, ["U"]), (SPR, ["R"]) ]
    make :: (StrPrefix, [String]) -> Parser StrPrefix
    make (typ, lst) = try $ do
      let (x : xs) = map (try . string) lst
      foldl (<|>) x xs
      return typ
  foldr (<|>) (return StNone) $ 
    map make target

p_str_suffix :: Parser StrSuffix
p_str_suffix = (char 's' *> return SSs) <|> (return SSNone)

p_escaped_string :: Parser String
p_escaped_string = many (try (p_escaped_char '"') <|> noneOf "\"")
