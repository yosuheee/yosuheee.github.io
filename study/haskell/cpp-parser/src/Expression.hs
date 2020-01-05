{-# LANGUAGE DeriveGeneric #-}

module Expression where

import Text.Parsec
import Text.Parsec.String

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Primitive

data Infix = InfixL | InfixR deriving (Show)

data Expression =
  ExBoolean Bool |
  ExChar Char |
  ExDouble Double |
  ExInteger Integer |
  ExString String |
  ExIdentity String |
  ExSuffix String Expression |
  ExPrefix String Expression |
  ExBinary String Expression Expression |
  ExTernary Expression Expression Expression |
  ExFunction Expression [Expression]
  deriving (Show, Generic)

instance FromJSON Expression
instance ToJSON Expression

type PE = Parser Expression

p_expression :: PE
p_expression = p_priority_16 <?> "expression"

p_primitive :: PE
p_primitive =
  p_priority_0 <|>
  (ExBoolean <$> p_boolean) <|>
  (ExDouble <$> p_double) <|>
  (ExInteger <$> p_integer) <|>
  (ExString <$> p_string) <|>
  (ExChar <$> p_char) <|>
  (ExIdentity <$> p_identity)

p_priority_0 :: PE
p_priority_0 = try $ do
  char '('
  spaces
  expr <- p_expression
  spaces
  char ')'
  spaces
  return expr

p_priority_2 :: PE
p_priority_2 = try $ do
  expr <- p_primitive
  spaces
  rest <- many $ parsers <* spaces
  let result = foldl merge expr rest
  spaces
  return result
  where
    parsers =
      p_p2_dot <|>
      p_p2_address <|>
      p_p2_function <|>
      p_p2_array <|>
      p_p2_increment <|>
      p_p2_decrement
    merge a c =
      case c of
        E2Dot str -> (ExBinary "." a $ ExIdentity str)
        E2Address str -> (ExBinary "->" a $ ExIdentity str)
        E2Function args -> (ExFunction a args)
        E2Array expr -> (ExBinary "[]" a expr)
        E2Increment -> (ExSuffix "++" a)
        E2Decrement -> (ExSuffix "--" a)

data E2Data =
  E2Dot String |
  E2Address String |
  E2Function [Expression] |
  E2Array Expression |
  E2Increment |
  E2Decrement
  deriving Show

p_p2_increment :: Parser E2Data
p_p2_increment = try $ do
  string "++"
  spaces
  return E2Increment

p_p2_decrement :: Parser E2Data
p_p2_decrement = try $ do
  string "--"
  spaces
  return E2Decrement

p_p2_array :: Parser E2Data
p_p2_array = try $ do
  char '['
  spaces
  expr <- p_expression
  char ']'
  spaces
  return $ E2Array expr

p_p2_dot :: Parser E2Data
p_p2_dot = try $ do
  char '.'
  spaces
  id <- p_identity
  spaces
  return $ E2Dot id

p_p2_address :: Parser E2Data
p_p2_address = try $ do
  string "->"
  spaces
  id <- p_identity
  spaces
  return $ E2Address id

p_p2_function :: Parser E2Data
p_p2_function = try $ do
  char '('
  spaces
  args <- sepBy p_expression (char ',' >> spaces)
  char ')'
  spaces
  return $ E2Function args

p_priority_3 :: PE
p_priority_3 = try $ do
  prefs <- many p_prefs
  value <- p_priority_2
  return $ foldr ExPrefix value prefs
  where
    p_prefs = choice
      [ try (string "++")
      , try (string "--")
      , string "+", string "-", string "!"
      , string "~", string "*", string "&" ]

p_priority_4 :: PE
p_priority_4 = try $ do
  expr <- p_priority_3
  spaces
  rest <- many $ parsers <* spaces
  let result = foldl merge expr rest
  spaces
  return result
  where
    parsers =
      p_p4_member <|>
      p_p4_pointer
    merge a c =
      case c of
        E4Member str -> (ExBinary ".*" a $ ExIdentity str)
        E4Pointer str -> (ExBinary "->*" a $ ExIdentity str)

data E4Data =
  E4Member String |
  E4Pointer String
  deriving Show

p_p4_member :: Parser E4Data
p_p4_member = try $ do
  string ".*"
  spaces
  id <- p_identity
  spaces
  return $ E4Member id

p_p4_pointer :: Parser E4Data
p_p4_pointer = try $ do
  string "->*"
  spaces
  id <- p_identity
  spaces
  return $ E4Pointer id

p_priority_5_15 :: PE
p_priority_5_15 = p_priority_12_15

p_priority_5_10 :: PE
p_priority_5_10 =
  foldl (flip p_binops) p_priority_4 $
    map (\(f, s) -> (f, map string s)) [
      (InfixL, ["*", "/", "%"]),
      (InfixL, ["+", "-"]),
      (InfixL, ["<<", ">>"]),
      (InfixL, ["<=>"]),
      (InfixL, ["<", ">", "<=", ">="]),
      (InfixL, ["==", "!="])]

p_priority_11 :: PE
p_priority_11 =
  p_binops (InfixL, [string "&" <* (notFollowedBy $ char '&')]) p_priority_5_10

p_priority_12_15 :: PE
p_priority_12_15 =
  foldl (flip p_binops) p_priority_11 $
    map (\(f, s) -> (f, map string s)) [
      (InfixL, ["^"]),
      (InfixL, ["|"]),
      (InfixL, ["&&"]),
      (InfixL, ["||"])]

p_priority_16 :: PE
p_priority_16 =
  p_ternary <|>
  p_throw <|>
  flip p_binops p_priority_5_15 (
    (InfixR, map string
      ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]))

p_binops :: (Infix, [Parser String]) -> PE -> PE
p_binops (ifx, ops) p_high_priority = try $ do
  lft <- p_high_priority
  rgt <- many . try $ spaces *> rest
  return $
    case ifx of
      InfixL -> foldl (\a c -> ExBinary (fst c) a (snd c)) lft rgt
      InfixR ->
        case length rgt of
          0 -> lft
          _ ->
            ExBinary (fst g) lft (snd g)
              where
                (x : xs) = reverse rgt
                f a c = ((fst c), (ExBinary (fst a) (snd c) (snd a)))
                g = foldl f x xs
  where
    (x : xs) = ops
    rest = do
      o <- foldl (\a c -> try c <|> a) x xs
      r <- try $ spaces *> p_high_priority
      return (o, r)

p_ternary :: PE
p_ternary = try $ do
  fst <- p_priority_5_15
  spaces
  char '?'
  spaces
  snd <- p_priority_5_15
  spaces
  char ':'
  spaces
  trd <- p_priority_5_15
  return $ ExTernary fst snd trd

p_throw :: PE
p_throw = try $ do
  string "throw"
  skipMany1 space
  val <- p_priority_5_15
  return $ ExPrefix "throw" val
