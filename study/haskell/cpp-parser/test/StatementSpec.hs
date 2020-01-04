module StatementSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Util

import Expression
import Statement

spec :: Spec
spec = do
  it "statements" $ do
    exec p_statement "{ int a = 1; a + 1; }" `shouldBe`
      (show $ StCompound [
        StDeclarator (Type "int") [SetVar "a" (ExInteger 1)],
        StExpression (ExBinary "+" (ExIdentity "a") (ExInteger 1)) ])

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

  it "simple type" $ do
    exec p_simple_type "char" `shouldBe` (show $ Type "char")
    exec p_simple_type "long double" `shouldBe` (show $ Type "long double")

  it "simple declarator" $ do
    exec p_set_declarator "a = 1" `shouldBe` (show $ SetVar "a" (ExInteger 1))
    exec p_unset_declarator "a" `shouldBe` (show $ UnsetVar "a")

  it "simple declarators" $ do
    exec p_declarators "a = 1, b, c = 'C'" `shouldBe`
      (show $ [SetVar "a" (ExInteger 1), UnsetVar "b", SetVar "c" (ExChar 'C')])

  it "simple declarator statement" $ do
    exec p_statement_declarator "int a = 1, b;" `shouldBe`
      (show $ StDeclarator (Type "int") [SetVar "a" (ExInteger 1), UnsetVar "b"])
