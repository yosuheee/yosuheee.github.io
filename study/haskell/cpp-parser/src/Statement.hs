module Statement where

import Text.Parsec
import Text.Parsec.String

import Expression

data Statement =
  StCompound [Statement] |
  StExpression Expression
  deriving Show

p_statement :: Parser Statement
p_statement =
  p_statement_compound <|>
  p_statement_expression

p_statement_expression :: Parser Statement
p_statement_expression = try $ do
  exp <- p_expression
  spaces
  char ';'
  return . StExpression $ exp

p_statement_compound :: Parser Statement
p_statement_compound = try $ do
  char '{'
  spaces
  body <- many p_statement
  spaces
  char '}'
  return . StCompound $ body
