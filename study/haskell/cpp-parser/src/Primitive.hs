module Primitive where

import Text.Parsec
import Text.Parsec.String

data NumSuffix = SuNone | SuU | SuL | SuLU | SuLL | SuLLU deriving Show

data Number =
  NuBin String NumSuffix |
  NuOct String NumSuffix |
  NuDec String NumSuffix |
  NuHex String NumSuffix
  deriving Show

p_num_suffix :: Parser NumSuffix
p_num_suffix = do
  let
    target = [
      (SuLLU, [ "llu", "ull", "llU", "ulL", "lLu", "uLl", "lLU", "uLL",
                "Llu", "Ull", "LlU", "UlL", "LLu", "ULl", "LLU", "ULL" ]),
      (SuLU, [ "lu", "ul", "lU", "uL", "Lu", "Ul", "LU", "UL" ]),
      (SuLL, [ "ll", "lL", "Ll", "LL" ]),
      (SuL, [ "l", "L" ]),
      (SuU, [ "u", "U" ]) ]
    make :: (NumSuffix, [String]) -> Parser NumSuffix
    make (typ, lst) = try $ do
      let (x : xs) = map (try . string) lst
      foldl (<|>) x xs
      return typ
  foldr (<|>) (return SuNone) $ 
    map make target

p_num :: Parser Number
p_num = p_bin <|> p_hex <|> p_oct <|> p_dec

p_hex :: Parser Number
p_hex = try $ do
  string "0x"
  head <- oneOf $ "123456789" ++ abc
  rest <- many $ digit <|> oneOf abc
  suff <- p_num_suffix
  return $ NuHex ([head] ++ rest) suff
  where
    abc = "abcdefABCDEF"

p_bin :: Parser Number
p_bin = try $ do
  string "0b"
  head <- char '1'
  rest <- many $ oneOf "01"
  suff <- p_num_suffix
  return $ NuBin ([head] ++ rest) suff

p_oct :: Parser Number
p_oct = try $ do
  string "0"
  head <- oneOf "1234567"
  rest <- many $ oneOf "01234567"
  suff <- p_num_suffix
  return $ NuOct ([head] ++ rest) suff

p_dec :: Parser Number
p_dec = try $ do
  head <- oneOf "123456789"
  rest <- many digit
  suff <- p_num_suffix
  return $ NuDec ([head] ++ rest) suff

data StrPrefix =
  Stu8 | StL | Stu | StU | 
  StR | Stu8R | StLR | StuR | StUR | 
  StNone deriving Show

data StrSuffix =
  Sts | StNon deriving Show

data StrLiteral = StrLiteral StrPrefix StrSuffix String deriving Show

p_str_prefix :: Parser StrPrefix
p_str_prefix = do
  let
    target = [
      (Stu8R, ["u8R"]), (StLR, ["LR"]), (StuR, ["uR"]), (StUR, ["UR"]),
      (Stu8, ["u8"]), (StL, ["L"]), (Stu, ["u"]), (StU, ["U"]), (StR, ["R"]) ]
    make :: (StrPrefix, [String]) -> Parser StrPrefix
    make (typ, lst) = try $ do
      let (x : xs) = map (try . string) lst
      foldl (<|>) x xs
      return typ
  foldr (<|>) (return StNone) $ 
    map make target

p_string_literal :: Parser StrLiteral
p_string_literal = try $ do
  pref <- p_str_prefix
  char '"'
  str <- p_escaped_string
  char '"'
  suff <- (char 's' *> return Sts) <|> (return StNon)
  return $ StrLiteral pref suff str

p_escaped_string :: Parser String
p_escaped_string = do
  many (escaped_char <|> noneOf "\"")
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
      (char '0'  >> return '\0') <|>
      return '\\')

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
