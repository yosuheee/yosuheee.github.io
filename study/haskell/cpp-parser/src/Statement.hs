module Statement where

import Text.Parsec
import Text.Parsec.String

import Primitive
import Primitive.Identity (p_identity_char_and_digit)
import Expression

data LabelPrefix =
  SLCase Expression |
  SLDefault |
  SLIdentity String deriving Show

data InitStmt = InitStmt Type [Variable] deriving Show

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
  deriving Show

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
  p_statement_for

p_statement_label :: PS
p_statement_label =
  p_statement_label_case <|>
  p_statement_label_default <|>
  p_statement_label_identity

p_statement_label_case :: PS
p_statement_label_case = try $ do
  string "case"
  skipMany1 space
  expr <- p_expression
  spaces
  char ':'
  spaces
  stmt <- p_statement
  spaces
  return $ StLabel (SLCase expr) stmt

p_statement_label_default :: PS
p_statement_label_default = try $ do
  string "default"
  spaces
  char ':'
  spaces
  stmt <- p_statement
  spaces
  return $ StLabel SLDefault stmt

p_statement_label_identity :: PS
p_statement_label_identity = try $ do
  id <- p_identity
  spaces
  char ':'
  spaces
  stmt <- p_statement
  spaces
  return $ StLabel (SLIdentity id) stmt

p_statement_break :: PS
p_statement_break = try $ do
  string "break"
  spaces
  char ';'
  spaces
  return StBreak

p_statement_continue :: PS
p_statement_continue = try $ do
  string "continue"
  spaces
  char ';'
  spaces
  return StContinue

p_statement_return :: PS
p_statement_return = try $ do
  string "return"
  many1 space
  expr <- p_expression
  spaces
  char ';'
  spaces
  return . StReturn $ expr

p_statement_goto :: PS
p_statement_goto = try $ do
  string "goto"
  many1 space
  id <- p_identity
  spaces
  char ';'
  spaces
  return $ StGoto id 

p_statement_expression :: PS
p_statement_expression = try $ do
  exp <- p_expression
  spaces
  char ';'
  spaces
  return . StExpression $ exp

p_statement_compound :: PS
p_statement_compound = try $ do
  char '{'
  body <- many $ spaces *> p_statement
  spaces
  char '}'
  spaces
  return . StCompound $ body

p_statement_declarator :: PS
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
  spaces
  return $ SetVar name expr

p_unset_declarator :: Parser Variable
p_unset_declarator = try $ do
  name <- p_identity
  spaces
  return $ UnsetVar name

data Type = Type String deriving Show

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
  spaces
  char '('
  spaces
  cond <- p_expression
  spaces
  char ')'
  spaces
  true_stat <- p_statement
  spaces
  return $ StIf cond true_stat

p_statement_if_else :: PS
p_statement_if_else = try $ do
  string "if"
  spaces
  char '('
  spaces
  cond <- p_expression
  spaces
  char ')'
  spaces
  true_stat <- p_statement
  spaces
  string "else"
  spaces
  false_stat <- p_statement
  spaces
  return $ StIfElse cond true_stat false_stat

p_statement_switch :: PS
p_statement_switch = try $ do
  string "switch"
  spaces
  char '('
  spaces
  cond <- p_expression
  spaces
  char ')'
  spaces
  stmt <- p_statement
  spaces
  return $ StSwitch cond stmt

p_statement_while :: PS
p_statement_while = try $ do
  string "while"
  spaces
  char '('
  spaces
  cond <- p_expression
  spaces
  char ')'
  spaces
  stmt <- p_statement
  spaces
  return $ StWhile cond stmt

p_statement_do_while :: PS
p_statement_do_while = try $ do
  string "do"
  notFollowedBy p_identity_char_and_digit
  spaces
  stmt <- p_statement
  string "while"
  spaces
  char '('
  spaces
  cond <- p_expression
  spaces
  char ')'
  spaces
  char ';'
  spaces
  return $ StDoWhile stmt cond

p_statement_for :: PS
p_statement_for = try $ do
  string "for"
  spaces
  char '('
  spaces
  fst <-
    option Nothing $ do
      typ <- p_simple_type
      many1 space
      decls <- p_declarators
      spaces
      return . Just $ InitStmt typ decls
  spaces
  char ';'
  spaces
  snd <- option Nothing (Just <$> p_expression)
  spaces
  char ';'
  spaces
  thd <- option Nothing (Just <$> p_expression)
  spaces
  char ')'
  spaces
  stmt <- p_statement
  spaces
  return $ StFor fst snd thd stmt
