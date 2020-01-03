module Expression where

import Text.Parsec
import Text.Parsec.String

import Primitive

data Expression =
  ExInt Integer |
  ExDouble Double |
  ExIdentity String |
  ExTernary Expression Expression Expression |
  ExThrow Expression |
  ExBinary String Expression Expression
  deriving (Show)

p_expression :: Parser Expression
p_expression = p_priority_16

p_expression_primitive :: Parser Expression
p_expression_primitive = do
  try p_expression_double <|> try p_expression_identity <|> try p_expression_integer

p_expression_integer :: Parser Expression
p_expression_integer = do
  num <- p_number
  return $ ExInt num

p_expression_identity :: Parser Expression
p_expression_identity = do
  ident <- p_identity
  return $ ExIdentity ident

p_expression_double :: Parser Expression
p_expression_double = do
  fst <- many1 digit
  char '.'
  snd <- many1 digit
  return . ExDouble . read $ fst ++ "." ++ snd

data Infix = InfixL | InfixR

p_priority_15 =
  foldl (\a c -> p_binops c a) p_expression_primitive [
    (InfixL, ["*", "/", "%"]),
    (InfixL, ["+", "-"]),
    (InfixL, ["<<", ">>"]),
    (InfixL, ["<=>"]),
    (InfixL, ["<=", ">=", "<", ">"]),
    (InfixL, ["==", "!="]),
    (InfixL, ["&"]),
    (InfixL, ["^"]),
    (InfixL, ["|"]),
    (InfixL, ["&&"]),
    (InfixL, ["||"])]

p_priority_16 = 
  p_ternary <|>
  p_throw <|>
  p_binops (InfixR, ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]) p_priority_15

p_binops :: (Infix, [String]) -> Parser Expression -> Parser Expression
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

p_ternary :: Parser Expression
p_ternary = try $ do
  fst <- p_priority_15
  spaces >> char '?' >> spaces
  snd <- p_priority_15
  spaces >> char ':' >> spaces
  trd <- p_priority_15
  return $ ExTernary fst snd trd

p_throw :: Parser Expression
p_throw = try $ do
  string "throw"
  skipMany1 space
  fst <- p_priority_15
  return $ ExThrow fst
