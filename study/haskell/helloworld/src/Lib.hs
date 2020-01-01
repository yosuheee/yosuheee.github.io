module Lib
    ( parseInteger
    ) where

import Text.Parsec
import Text.Parsec.String

parseInteger :: Parser Integer
parseInteger = do
    str <- many1 digit
    return $ read str
