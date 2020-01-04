module Statement where

import Text.Parsec
import Text.Parsec.String

import Primitive
import Expression

data Statement =
  StCompound [Statement] |
  StExpression Expression |
  StDeclarator Type [Variable]
  deriving Show

p_statement :: Parser Statement
p_statement =
  p_statement_compound <|>
  p_statement_expression <|>
  p_statement_declarator

p_statement_expression :: Parser Statement
p_statement_expression = try $ do
  exp <- p_expression
  spaces
  char ';'
  spaces
  return . StExpression $ exp

p_statement_compound :: Parser Statement
p_statement_compound = try $ do
  char '{'
  body <- many $ spaces *> p_statement
  spaces
  char '}'
  spaces
  return . StCompound $ body

p_statement_declarator :: Parser Statement
p_statement_declarator = try $ do
  typ <- p_simple_type
  many1 space
  decls <- p_declarators
  spaces
  char ';'
  spaces
  return $ StDeclarator typ decls

data Variable = 
  SetVar String Expression |
  UnsetVar String
  deriving Show

p_declarators :: Parser [Variable]
p_declarators = try $ do
  let decl = p_set_declarator <|> p_unset_declarator
  decls <- sepBy decl (char ',' >> spaces)
  return decls

p_set_declarator :: Parser Variable
p_set_declarator = try $ do
  name <- p_identity
  spaces
  char '='
  spaces
  expr <- p_expression
  return $ SetVar name expr

p_unset_declarator :: Parser Variable
p_unset_declarator = try $ do
  name <- p_identity
  return $ UnsetVar name

data Type = Type String deriving Show

p_simple_type :: Parser Type
p_simple_type = try $ do
  typ <- choice $ map string [
    "signed char", "unsigned char", "char",
    "char8_t", "char16_t", "char32_t", "wchar_t",
    "bool",
    "int", "short", "long",
    "signed short int", "unsigned short int",
    "signed long int",  "unsigned long int",
    "signed int",       "unsigned int",
    "signed short",     "unsigned short",
    "signed long",      "unsigned long",
    "signed",           "unsigned",
    "long double", "float", "double",
    "void" ]
  return . Type $ typ
