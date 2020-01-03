module Primitive where

import Text.Parsec
import Text.Parsec.String

data DblSuffix = DSNone | DSf | DSF | DSl | DSL deriving Show

data DblLiteral = DblLiteral DblSuffix String String deriving Show

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
  snd <- option "+1" p_exponent
  suf <- p_double_suffix
  return $ DblLiteral suf fst snd

p_all_double :: Parser DblLiteral
p_all_double = try $ do
  fst <- option "0" $ many1 digit
  char '.'
  snd <- many1 digit
  thd <- option "+1" p_exponent
  suf <- p_double_suffix
  return $ DblLiteral suf (fst ++ "." ++ snd) thd

p_exponent :: Parser String
p_exponent = try $ do
  oneOf "eE"
  sign <- oneOf "+-" <|> return '+'
  rest <- many1 digit
  return $ [sign] ++ rest

data IntSuffix = ISNone | ISU | ISL | ISLU | ISLL | ISLLU deriving Show

data IntLiteral =
  IBin IntSuffix String |
  IOct IntSuffix String |
  IDec IntSuffix String |
  IHex IntSuffix String
  deriving Show

p_int_literal :: Parser IntLiteral
p_int_literal = p_bin <|> p_hex <|> p_oct <|> p_dec

p_int_suffix :: Parser IntSuffix
p_int_suffix = do
  let
    target = [
      (ISLLU, [ "llu", "ull", "llU", "ulL", "lLu", "uLl", "lLU", "uLL",
                "Llu", "Ull", "LlU", "UlL", "LLu", "ULl", "LLU", "ULL" ]),
      (ISLU, [ "lu", "ul", "lU", "uL", "Lu", "Ul", "LU", "UL" ]),
      (ISLL, [ "ll", "lL", "Ll", "LL" ]),
      (ISL, [ "l", "L" ]),
      (ISU, [ "u", "U" ]) ]
    make :: (IntSuffix, [String]) -> Parser IntSuffix
    make (typ, lst) = try $ do
      let (x : xs) = map (try . string) lst
      foldl (<|>) x xs
      return typ
  foldr (<|>) (return ISNone) $ 
    map make target

p_int_make :: (IntSuffix -> String -> IntLiteral) -> String -> Parser Char -> Parser IntLiteral
p_int_make typ pre chr = try $ do
  string pre
  rest <- many1 chr
  suff <- p_int_suffix
  return $ typ suff rest

p_bin :: Parser IntLiteral
p_bin = p_int_make IBin "0b" (oneOf "01")

p_hex :: Parser IntLiteral
p_hex = p_int_make IHex "0x" hexDigit

p_oct :: Parser IntLiteral
p_oct = p_int_make IOct "0" octDigit

p_dec :: Parser IntLiteral
p_dec = p_int_make IDec "" digit

data ChrPrefix =
  CSu8 | CSu | CSU | CSL | CSNone deriving Show

data ChrLiteral =
  ChrLiteral ChrPrefix Char deriving Show

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

data StrPrefix =
  SPu8 | SPL | SPu | SPU | 
  SPR | SPu8R | SPLR | SPuR | SPUR | 
  StNone deriving Show

data StrSuffix =
  SSs | SSNone deriving Show

data StrLiteral = StrLiteral StrPrefix StrSuffix String deriving Show

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
