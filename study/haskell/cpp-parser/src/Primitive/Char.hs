module Primitive.Char where

import Text.Parsec
import Text.Parsec.String

data ChrPrefix =
  CSu8 | CSu | CSU | CSL | CSNone deriving Show

data ChrLiteral =
  ChrLiteral ChrPrefix Char deriving Show

p_char :: Parser Char
p_char = do
  chr <- p_chr_literal
  spaces
  let ChrLiteral _ c = chr
  return c

p_chr_literal :: Parser ChrLiteral
p_chr_literal = try $ do
  pref <- p_chr_prefix
  char '\''
  chr <- p_escaped_char '\''
  char '\''
  return $ ChrLiteral pref chr

p_chr_prefix :: Parser ChrPrefix
p_chr_prefix = do
  let
    make :: (ChrPrefix, String) -> Parser ChrPrefix
    make (typ, str) = try $ do
      try . string $ str
      return typ
  foldr (<|>) (return CSNone) $ 
    map make [ (CSu8, "u8"), (CSL, "L"), (CSu, "u"), (CSU, "U") ]

p_escaped_char :: Char -> Parser Char
p_escaped_char c = 
  escaped_char <|> noneOf [c]
  where
    escaped_char = char '\\' >> (
      (char '\\' >> return '\\') <|> 
      (char '\'' >> return '\'') <|> 
      (char '\"' >> return '\"') <|>
      (char 'a'  >> return '\a') <|>
      (char 'b'  >> return '\b') <|> 
      (char 'f'  >> return '\f') <|> 
      (char 'n'  >> return '\n') <|> 
      (char 'r'  >> return '\r') <|> 
      (char 't'  >> return '\t') <|> 
      (char '0'  >> return '\0'))
