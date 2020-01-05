module Lib where

import Text.Parsec
import Text.Parsec.String

import Declaration
import Statement
import Util

p_main :: Parser ([String], ((String, String), [(String, String)], Statement))
p_main = do
  files <- many p_primitive_include
  ___
  def <- p_function_definition
  return $ (files, def)
