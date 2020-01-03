module Primitive where

import Text.Parsec
import Text.Parsec.String

data DblSuffix = DbNone | Dbf | DbF | Dbl | DbL deriving Show

data DblLiteral = DblLiteral DblSuffix String String deriving Show

p_double :: Parser DblLiteral
p_double =
  p_all_double <|> p_front_double <|> p_simple_double

p_double_suffix :: Parser DblSuffix
p_double_suffix = try $ do
  let
    make :: (DblSuffix, String) -> Parser DblSuffix
    make (typ, str) = try $ do
      string str
      return typ
  foldr (<|>) (return DbNone) $ 
    map make [ (Dbf, "f"), (DbF, "F"), (Dbl, "l"), (DbL, "L") ]

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

p_num_make :: (String -> NumSuffix -> Number) -> String -> Parser Char -> Parser Number
p_num_make typ pre chr = try $ do
  string pre
  rest <- many1 chr
  suff <- p_num_suffix
  return $ typ rest suff

p_bin :: Parser Number
p_bin = p_num_make NuBin "0b" (oneOf "01")

p_hex :: Parser Number
p_hex = p_num_make NuHex "0x" hexDigit

p_oct :: Parser Number
p_oct = p_num_make NuOct "0" octDigit

p_dec :: Parser Number
p_dec = p_num_make NuDec "" digit

data ChrPrefix =
  Chu8 | Chu | ChU | ChL | ChNone deriving Show

data ChrLiteral =
  ChrLiteral ChrPrefix Char deriving Show

p_chr_prefix :: Parser ChrPrefix
p_chr_prefix = do
  let
    make :: (ChrPrefix, String) -> Parser ChrPrefix
    make (typ, str) = try $ do
      try . string $ str
      return typ
  foldr (<|>) (return ChNone) $ 
    map make [ (Chu8, "u8"), (ChL, "L"), (Chu, "u"), (ChU, "U") ]

p_chr_literal :: Parser ChrLiteral
p_chr_literal = try $ do
  pref <- p_chr_prefix
  char '\''
  chr <- p_escaped_char '\''
  char '\''
  return $ ChrLiteral pref chr

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
  str <- many (try (p_escaped_char '"') <|> noneOf "\"")
  return str

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
