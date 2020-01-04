module Util where

import Text.Parsec
import Text.Parsec.String

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
