module Util where

import Text.Parsec
import Text.Parsec.String

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val

message :: Integer -> Integer -> String -> String
message line num token =
  "(line " ++ (show line) ++ ", column " ++ (show num) ++ "):\n" ++
  "unexpected end of input\n" ++ 
  "expecting " ++ token

p_spaces_not_crlf :: Parser String
p_spaces_not_crlf = try $ do
  many $ notFollowedBy endOfLine >> space

__ :: Parser String
__ = many $ notFollowedBy endOfLine *> space

___ :: Parser String
___ = many space

sepByComma :: Parser a -> Parser [a]
sepByComma p = sepBy p (char ',' >> ___)
