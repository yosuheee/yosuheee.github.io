module Expression where

import Text.Parsec
import Text.Parsec.String

import Primitive

data Infix = InfixL | InfixR deriving (Show)

data Expression =
  ExInt Integer |
  ExDouble Double |
  ExIdentity String |
  ExPrefix String Expression |
  ExSuffix String Expression |
  ExBinary String Expression Expression |
  ExTernary Expression Expression Expression
  deriving (Show)

type PE = Parser Expression

p_expression :: PE
p_expression = p_priority_16

p_expression_primitive :: PE
p_expression_primitive = do
  try p_expression_double <|> try p_expression_identity <|> try p_expression_integer

p_expression_integer :: PE
p_expression_integer = do
  num <- p_number
  return $ ExInt num

p_expression_identity :: PE
p_expression_identity = do
  ident <- p_identity
  return $ ExIdentity ident

p_expression_double :: PE
p_expression_double = do
  fst <- many1 digit
  char '.'
  snd <- many1 digit
  return . ExDouble . read $ fst ++ "." ++ snd

p_priority_2 :: PE
p_priority_2 = try $ do
  value <- p_expression_primitive
  prefs <- many p_prefs
  return $ foldl (\a c -> ExPrefix c a) value prefs
  where
    p_prefs = choice
      [ try (string "++")
      , try (string "--") ]

p_priority_3 :: PE
p_priority_3 = try $ do
  suffs <- many p_suffs
  value <- p_priority_2
  return $ foldr (\a c -> ExSuffix a c) value suffs
  where
    p_suffs = choice
      [ try (string "++")
      , try (string "--")
      , string "+", string "-", string "!"
      , string "~", string "*", string "&" ]

p_priority_5_15 = p_priority_12_15

p_priority_5_10 =
  foldl (\a c -> p_binops c a) p_priority_3 $
    map (\(f, s) -> (f, map string s)) [
      (InfixL, ["*", "/", "%"]),
      (InfixL, ["+", "-"]),
      (InfixL, ["<<", ">>"]),
      (InfixL, ["<=>"]),
      (InfixL, ["<", ">", "<=", ">="]),
      (InfixL, ["==", "!="])]

p_priority_11 =
  p_binops (InfixL, [string "&" <* (notFollowedBy $ char '&')]) p_priority_5_10

p_priority_12_15 =
  foldl (\a c -> p_binops c a) p_priority_11 $
    map (\(f, s) -> (f, map string s)) [
      (InfixL, ["^"]),
      (InfixL, ["|"]),
      (InfixL, ["&&"]),
      (InfixL, ["||"])]

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
  spaces >> char '?' >> spaces
  snd <- p_priority_5_15
  spaces >> char ':' >> spaces
  trd <- p_priority_5_15
  return $ ExTernary fst snd trd

p_throw :: PE
p_throw = try $ do
  string "throw"
  skipMany1 space
  val <- p_priority_5_15
  return $ ExSuffix "throw" val
