module Expression where

import Text.Parsec
import Text.Parsec.String

import Primitive

data Infix = InfixL | InfixR deriving (Show)

data Expression =
  ExDbl DblLiteral |
  ExInt IntLiteral |
  ExStr StrLiteral |
  ExChr ChrLiteral |
  ExIdentity String |
  ExPrefix String Expression |
  ExSuffix String Expression |
  ExBinary String Expression Expression |
  ExTernary Expression Expression Expression
  deriving (Show)

type PE = Parser Expression

p_expression :: PE
p_expression = p_priority_16

p_primitive :: PE
p_primitive =
  (ExDbl <$> p_dbl) <|>
  (ExInt <$> p_int) <|>
  (ExStr <$> p_str) <|>
  (ExChr <$> p_chr) <|>
  (ExIdentity <$> p_identity)

p_priority_2 :: PE
p_priority_2 = try $ do
  value <- p_primitive
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
