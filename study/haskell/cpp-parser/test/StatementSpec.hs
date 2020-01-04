module StatementSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Util

import Expression
import Statement

spec :: Spec
spec = do
  it "expression;" $ do
    1 `shouldBe` 1
    exec p_statement_expression "1;" `shouldBe` (show $ StExpression (ExInteger 1))

  it "{statement}" $ do
    exec p_statement_compound "{1;}" `shouldBe`
      (show $ StCompound [StExpression (ExInteger 1)])
    exec p_statement_compound "{{1;}2;{}}" `shouldBe`
      (show $ StCompound [StCompound [StExpression (ExInteger 1)], StExpression (ExInteger 2), StCompound []])
    exec p_statement_compound "{{1;} 2;{}}" `shouldBe`
      (show $ StCompound [StCompound [StExpression (ExInteger 1)], StExpression (ExInteger 2), StCompound []])