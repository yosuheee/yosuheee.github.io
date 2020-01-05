module Lib where

import Text.Parsec
import Text.Parsec.String

import Declaration
import Statement

p_main :: Parser ([String], ((String, String), [(String, String)], Statement))
p_main = do
  files <- many p_primitive_include
  spaces
  def <- p_function_definition
  return $ (files, def)
