{-# LANGUAGE DeriveGeneric #-}

module Statement where

import Text.Parsec
import Text.Parsec.String

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Primitive
import Primitive.Identity (p_identity_char_and_digit)
import Expression
import Util

data Type = Type String deriving (Show, Generic)

data Variable = 
  SetVar String Expression |
  UnsetVar String
  deriving (Show, Generic)

data InitStmt = InitStmt Type [Variable] deriving (Show, Generic)

data LabelPrefix =
  SLCase Expression |
  SLDefault |
  SLIdentity String deriving (Show, Generic)

data Statement =
  StBreak |
  StContinue |
  StReturn Expression |
  StGoto String |
  StLabel LabelPrefix Statement |
  StCompound [Statement] |
  StExpression Expression |
  StDeclarator Type [Variable] |
  StIf Expression Statement |
  StIfElse Expression Statement Statement |
  StSwitch Expression Statement |
  StWhile Expression Statement |
  StDoWhile Statement Expression |
  StFor (Maybe InitStmt) (Maybe Expression) (Maybe Expression) Statement
  deriving (Show, Generic)

instance FromJSON Type
instance FromJSON Variable
instance FromJSON InitStmt
instance FromJSON LabelPrefix
instance FromJSON Statement
instance ToJSON Type
instance ToJSON Variable
instance ToJSON InitStmt
instance ToJSON LabelPrefix
instance ToJSON Statement

type PS = Parser Statement

p_statement :: PS
p_statement =
  p_statement_break <|>
  p_statement_continue <|>
  p_statement_return <|>
  p_statement_goto <|>
  p_statement_compound <|>
  p_statement_expression <|>
  p_statement_declarator <|>
  p_statement_label <|>
  p_statement_if_else <|>
  p_statement_if <|>
  p_statement_switch <|>
  p_statement_while <|>
  p_statement_do_while <|>
  p_statement_for <?>
  "statement"

p_statement_label :: PS
p_statement_label =
  p_statement_label_case <|>
  p_statement_label_default <|>
  p_statement_label_identity

p_statement_label_case :: PS
p_statement_label_case = try $ do
  string "case "
  ___
  expr <- p_expression
  ___
  char ':'
  ___
  stmt <- p_statement
  ___
  return $ StLabel (SLCase expr) stmt

p_statement_label_default :: PS
p_statement_label_default = try $ do
  string "default"
  ___
  char ':'
  ___
  stmt <- p_statement
  ___
  return $ StLabel SLDefault stmt

p_statement_label_identity :: PS
p_statement_label_identity = try $ do
  id <- p_identity
  ___
  char ':'
  ___
  stmt <- p_statement
  ___
  return $ StLabel (SLIdentity id) stmt

p_statement_break :: PS
p_statement_break = try $ do
  string "break"
  ___
  char ';'
  ___
  return StBreak

p_statement_continue :: PS
p_statement_continue = try $ do
  string "continue"
  ___
  char ';'
  ___
  return StContinue

p_statement_return :: PS
p_statement_return = try $ do
  string "return "
  ___
  expr <- p_expression
  ___
  char ';'
  ___
  return . StReturn $ expr

p_statement_goto :: PS
p_statement_goto = try $ do
  string "goto "
  ___
  id <- p_identity
  ___
  char ';'
  ___
  return $ StGoto id 

p_statement_expression :: PS
p_statement_expression = try $ do
  exp <- p_expression
  ___
  char ';'
  ___
  return . StExpression $ exp

p_statement_compound :: PS
p_statement_compound = try $ do
  char '{'
  ___
  body <- many $ p_statement <* ___
  char '}'
  ___
  return . StCompound $ body

p_statement_declarator :: PS
p_statement_declarator = try $ do
  typ <- p_simple_type
  many1 space
  decls <- p_declarators
  ___
  char ';'
  ___
  return $ StDeclarator typ decls

p_declarators :: Parser [Variable]
p_declarators = try $ do
  let decl = p_set_declarator <|> p_unset_declarator
  decls <- sepBy decl (char ',' >> ___)
  return decls

p_set_declarator :: Parser Variable
p_set_declarator = try $ do
  name <- p_identity
  ___
  char '='
  ___
  expr <- p_expression
  ___
  return $ SetVar name expr

p_unset_declarator :: Parser Variable
p_unset_declarator = try $ do
  name <- p_identity
  ___
  return $ UnsetVar name

p_simple_type :: Parser Type
p_simple_type = try $ do
  typ <- choice $ map string [
    "signed char", "unsigned char", "char",
    "char8_t", "char16_t", "char32_t", "wchar_t",
    "bool",
    "signed short int", "unsigned short int",
    "signed long int",  "unsigned long int",
    "signed int",       "unsigned int",
    "signed short",     "unsigned short",
    "signed long",      "unsigned long",
    "signed",           "unsigned",
    "long double", "float", "double",
    "int", "short", "long",
    "void" ]
  return . Type $ typ

p_statement_if :: PS
p_statement_if = try $ do
  string "if"
  ___
  char '('
  ___
  cond <- p_expression
  ___
  char ')'
  ___
  true_stat <- p_statement
  ___
  return $ StIf cond true_stat

p_statement_if_else :: PS
p_statement_if_else = try $ do
  string "if"
  ___
  char '('
  ___
  cond <- p_expression
  ___
  char ')'
  ___
  true_stat <- p_statement
  ___
  string "else"
  ___
  false_stat <- p_statement
  ___
  return $ StIfElse cond true_stat false_stat

p_statement_switch :: PS
p_statement_switch = try $ do
  string "switch"
  ___
  char '('
  ___
  cond <- p_expression
  ___
  char ')'
  ___
  stmt <- p_statement
  ___
  return $ StSwitch cond stmt

p_statement_while :: PS
p_statement_while = try $ do
  string "while"
  ___
  char '('
  ___
  cond <- p_expression
  ___
  char ')'
  ___
  stmt <- p_statement
  ___
  return $ StWhile cond stmt

p_statement_do_while :: PS
p_statement_do_while = try $ do
  string "do"
  notFollowedBy p_identity_char_and_digit
  ___
  stmt <- p_statement
  ___
  string "while"
  ___
  char '('
  ___
  cond <- p_expression
  ___
  char ')'
  ___
  char ';'
  ___
  return $ StDoWhile stmt cond

p_statement_for :: PS
p_statement_for = try $ do
  string "for"
  ___
  char '('
  ___
  fst <-
    optionMaybe $ do
      typ <- p_simple_type
      many1 space
      decls <- p_declarators
      ___
      return $ InitStmt typ decls
  ___
  char ';'
  ___
  snd <- optionMaybe p_expression
  ___
  char ';'
  ___
  thd <- optionMaybe p_expression
  ___
  char ')'
  ___
  stmt <- p_statement
  ___
  return $ StFor fst snd thd stmt
