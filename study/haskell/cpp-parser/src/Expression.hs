module Expression where

import Text.Parsec
import Text.Parsec.String

import Primitive

data Expression =
  ExInt Integer |
  ExDouble Double |
  ExIdentity String |
  ExUnary String Expression |
  ExBinary String Expression Expression |
  ExTernary Expression Expression Expression |
  ExThrow Expression
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

p_priority_2 :: Parser Expression
p_priority_2 = 
  p_prefix_increment <|>
  p_prefix_decrement <|>
  p_expression_primitive

p_prefix_increment :: Parser Expression
p_prefix_increment = try $ do
  val <- p_identity
  spaces
  string "++"
  return $ ExUnary " ++" (ExIdentity val)

p_prefix_decrement :: Parser Expression
p_prefix_decrement = try $ do
  val <- p_identity
  spaces
  string "--"
  return $ ExUnary " --" (ExIdentity val)

p_priority_3 :: Parser Expression
p_priority_3 = 
  p_logical_negative <|>
  p_bit_negative <|>
  p_suffix_increment <|>
  p_suffix_decrement <|>
  p_indirect_reference <|>
  p_get_address <|>
  p_sizeof <|>
  p_priority_2

p_logical_negative :: Parser Expression
p_logical_negative = try $ do
  char '!'
  spaces
  val <- p_expression
  return $ ExUnary "!" val

p_bit_negative :: Parser Expression
p_bit_negative = try $ do
  char '~'
  spaces
  val <- p_expression
  return $ ExUnary "~" val

p_suffix_increment :: Parser Expression
p_suffix_increment = try $ do
  string "++"
  spaces
  val <- p_identity
  return $ ExUnary "++ " (ExIdentity val)

p_suffix_decrement :: Parser Expression
p_suffix_decrement = try $ do
  string "--"
  spaces
  val <- p_identity
  return $ ExUnary "-- " (ExIdentity val)

p_indirect_reference :: Parser Expression
p_indirect_reference = try $ do
  string "*"
  spaces
  val <- p_identity
  return $ ExUnary "*" (ExIdentity val)

p_get_address :: Parser Expression
p_get_address = try $ do
  string "&"
  spaces
  val <- p_identity
  return $ ExUnary "&" (ExIdentity val)

p_sizeof :: Parser Expression
p_sizeof = try $ do
  string "sizeof"
  skipMany1 space
  val <- p_expression
  return $ ExUnary "sizeof" val

p_priority_5_15 =
  foldl (\a c -> p_binops c a) p_priority_3 [
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
  p_binops (InfixR, ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]) p_priority_5_15

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
  fst <- p_priority_5_15
  spaces >> char '?' >> spaces
  snd <- p_priority_5_15
  spaces >> char ':' >> spaces
  trd <- p_priority_5_15
  return $ ExTernary fst snd trd

p_throw :: Parser Expression
p_throw = try $ do
  string "throw"
  skipMany1 space
  fst <- p_priority_5_15
  return $ ExUnary "throw" fst
