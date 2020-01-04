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

  it "break;" $ do
    exec p_statement_break "break;" `shouldBe` (show $ StBreak)
    exec p_statement "break;" `shouldBe` (show $ StBreak)

  it "continue;" $ do
    exec p_statement_continue "continue;" `shouldBe` (show $ StContinue)
    exec p_statement "continue;" `shouldBe` (show $ StContinue)

  it "return 1;" $ do
    exec p_statement_return "return 1;" `shouldBe` (show $ StReturn (ExInteger 1))
    exec p_statement "return 1;" `shouldBe` (show $ StReturn (ExInteger 1))

  it "goto label;" $ do
    exec p_statement_goto "goto label;" `shouldBe` (show $ StGoto "label")
    exec p_statement "goto label;" `shouldBe` (show $ StGoto "label")

  it "expression;" $ do
    exec p_statement_expression "1;" `shouldBe` (show $ StExpression (ExInteger 1))
    exec p_statement "1;" `shouldBe` (show $ StExpression (ExInteger 1))

  it "{statement}" $ do
    exec p_statement_compound "{1;}" `shouldBe`
      (show $ StCompound [StExpression (ExInteger 1)])
    exec p_statement_compound "{{1;}2;{}}" `shouldBe`
      (show $ StCompound [StCompound [StExpression (ExInteger 1)], StExpression (ExInteger 2), StCompound []])
    exec p_statement_compound "{{1;} 2;{}}" `shouldBe`
      (show $ StCompound [StCompound [StExpression (ExInteger 1)], StExpression (ExInteger 2), StCompound []])
    exec p_statement "{1;}" `shouldBe`
      (show $ StCompound [StExpression (ExInteger 1)])

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
    exec p_statement "int a = 1, b;" `shouldBe`
      (show $ StDeclarator (Type "int") [SetVar "a" (ExInteger 1), UnsetVar "b"])
