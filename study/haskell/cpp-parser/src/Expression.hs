module Expression where

import Text.Parsec
import Text.Parsec.String

import Primitive

data Infix = InfixL | InfixR deriving (Show)

data Expression =
  ExInt Integer |
  ExDouble Double |
  ExIdentity String |
  ExBinary String Expression Expression |
  ExTernary Expression Expression Expression |
  ExSuffix String Expression |
  ExPrefix String Expression
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

p_suffix :: String -> Parser a -> PE -> PE
p_suffix name p_suf p_val = try $ do
  p_suf
  val <- p_val
  return $ ExSuffix name val

p_prefix :: String -> PE -> Parser a -> PE
p_prefix name p_val p_pre = try $ do
  val <- p_val
  p_pre
  return $ ExPrefix name val

p_priority_2 :: PE
p_priority_2 = 
  p_prefix_increment <|>
  p_prefix_decrement <|>
  p_expression_primitive

p_prefix_increment :: PE
p_prefix_increment = p_prefix "++" (ExIdentity <$> p_identity) (spaces >> string "++")

p_prefix_decrement :: PE
p_prefix_decrement = p_prefix "--" (ExIdentity <$> p_identity) (spaces >> string "--")

p_priority_3 :: PE
p_priority_3 = try $ do
  prefs <- many p_priority_3_pref
  value <- p_priority_2
  return $ foldr (\a c -> ExSuffix a c) value prefs

p_priority_3_pref :: Parser String
p_priority_3_pref = choice 
  [ try (string "++")
  , try (string "--")
  , string "+"
  , string "-"
  , string "!"
  , string "~"
  , string "*"
  , string "&" ]

p_priority_5_15 = p_priority_12_15

p_priority_5_10 =
  foldl (\a c -> p_binops c a) p_priority_3 [
    (InfixL, ["*", "/", "%"]),
    (InfixL, ["+", "-"]),
    (InfixL, ["<<", ">>"]),
    (InfixL, ["<=>"]),
    (InfixL, ["<=", ">=", "<", ">"]),
    (InfixL, ["==", "!="])]

p_priority_11 = try $ do
  lft <- p_priority_5_10
  rgt <- many . try $ spaces *> rest
  return $
    foldl (\a c -> ExBinary (fst c) a (snd c)) lft rgt
  where
    rest = do
      o <- string "&"
      notFollowedBy $ char '&'
      r <- try $ spaces *> p_priority_5_10
      return (o, r)

p_priority_12_15 =
  foldl (\a c -> p_binops c a) p_priority_11 [
    (InfixL, ["^"]),
    (InfixL, ["|"]),
    (InfixL, ["&&"]),
    (InfixL, ["||"])]

p_priority_16 = 
  p_ternary <|>
  p_throw <|>
  p_binops (InfixR, ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]) p_priority_5_15

p_binops :: (Infix, [String]) -> PE -> PE
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
    (x : xs) = map (try . string) ops
    rest = do
      o <- foldl (<|>) x xs
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
p_throw = p_suffix "throw" (string "throw" >> skipMany1 space) p_priority_5_15
