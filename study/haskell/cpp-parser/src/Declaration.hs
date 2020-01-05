module Declaration where

import Text.Parsec
import Text.Parsec.String

import Primitive.Identity
import Statement
import Util

p_primitive_type :: Parser String
p_primitive_type = try $ do
  name <- choice $ map make [
    "char", "char8_t", "char16_t", "char32_t", "wchar_t",
    "signed char", "unsigned char",
    "signed short int", "signed short",
    "signed int", "signed long int", "signed long",
    "unsigned short int", "unsigned short",
    "unsigned int", "unsigned long int", "unsigned long",
    "signed", "unsigned",
    "long double", "float", "double",
    "short", "int", "long long", "long", "bool", "void" ]
  spaces
  return name
  where
    make :: String -> Parser String
    make s = try $ string s <* notFollowedBy (alphaNum <|> char '_')

p_primitive_identity :: Parser String
p_primitive_identity = p_identity

p_primitive_type_name :: Parser (String, String)
p_primitive_type_name = try $ do
  typ <- p_primitive_type
  name <- p_primitive_identity
  return (typ, name)

p_primitive_function :: Parser ((String, String), [(String, String)])
p_primitive_function = try $ do
  head <- p_primitive_type_name
  char '('
  spaces
  rest <- option [] $
    sepBy p_primitive_type_name (char ',' >> spaces)
  char ')'
  spaces
  return (head, rest)

p_function_definition :: Parser ((String, String), [(String, String)], Statement)
p_function_definition = try $ do
  func <- p_primitive_function
  stmt <- p_statement_compound
  return (fst func, snd func, stmt)

p_primitive_filename :: Parser String
p_primitive_filename = try $ do
  name <- many1 $ noneOf ">"
  return name

p_primitive_include :: Parser String
p_primitive_include = try $ do
  char '#'
  __
  string "include"
  __
  char '<'
  __
  name <- p_primitive_filename
  __
  char '>'
  __
  endOfLine
  return name
