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

  it "label case" $ do
    exec p_statement_label_case "case 1: return 0;" `shouldBe`
      (show $ StLabel (SLCase (ExInteger 1)) (StReturn (ExInteger 0)))
    exec p_statement "case 1: return 0;" `shouldBe`
      (show $ StLabel (SLCase (ExInteger 1)) (StReturn (ExInteger 0)))

  it "label default" $ do
    exec p_statement_label_default "default: return 0;" `shouldBe`
      (show $ StLabel SLDefault (StReturn (ExInteger 0)))
    exec p_statement "default: return 0;" `shouldBe`
      (show $ StLabel SLDefault (StReturn (ExInteger 0)))

  it "label identity" $ do
    exec p_statement_label_identity "label : return 0;" `shouldBe`
      (show $ StLabel (SLIdentity "label") (StReturn (ExInteger 0)))
    exec p_statement "label : return 0;" `shouldBe`
      (show $ StLabel (SLIdentity "label") (StReturn (ExInteger 0)))

  describe "if" $ do
    it "standard" $ do
      exec p_statement_if_else "if (true) { 1; } else { 2; }" `shouldBe`
        (show $ StIfElse (ExBoolean True)
          (StCompound [StExpression (ExInteger 1)])
          (StCompound [StExpression (ExInteger 2)]))
    it "else if" $ do
      exec p_statement_if_else "if (true) { 1; } else if (false) { 2; }" `shouldBe`
        (show $ StIfElse (ExBoolean True)
          (StCompound [StExpression (ExInteger 1)])
          (StIf (ExBoolean False)
            (StCompound [StExpression (ExInteger 2)])))
      exec p_statement_if_else "if (1) { 1; } else if (2) { 2; } else if (3) { 3; }" `shouldBe`
        (show $ StIfElse (ExInteger 1)
          (StCompound [StExpression (ExInteger 1)])
          (StIfElse (ExInteger 2)
            (StCompound [StExpression (ExInteger 2)])
            (StIf (ExInteger 3)
              (StCompound [StExpression (ExInteger 3)]))))

  describe "switch" $ do
    it "standard" $ do
      exec p_statement_switch "switch (a) { case 1: return 0; }" `shouldBe`
        (show $ StSwitch (ExIdentity "a")
          (StCompound [StLabel (SLCase (ExInteger 1)) (StReturn (ExInteger 0))]))
      exec p_statement "switch (a) { case 1: return 0; }" `shouldBe`
        (show $ StSwitch (ExIdentity "a")
          (StCompound [StLabel (SLCase (ExInteger 1)) (StReturn (ExInteger 0))]))

  describe "while" $ do
    it "while () ..." $ do
      exec p_statement_while "while (1) 2;" `shouldBe`
        (show $ StWhile (ExInteger 1) (StExpression (ExInteger 2)))
      exec p_statement "while (1) 2;" `shouldBe`
        (show $ StWhile (ExInteger 1) (StExpression (ExInteger 2)))
    it "do ... while ();" $ do
      exec p_statement_do_while "do 1; while (2);" `shouldBe`
        (show $ StDoWhile (StExpression (ExInteger 1)) (ExInteger 2))
      exec p_statement "do 1; while (2);" `shouldBe`
        (show $ StDoWhile (StExpression (ExInteger 1)) (ExInteger 2))

  describe "for" $ do
    it "standard" $ do
      exec p_statement_for "for (int i = 0; i < 5; i++) sum += i;" `shouldBe`
        (show $ StFor
          (Just $ InitStmt (Type "int") [SetVar "i" (ExInteger 0)])
          (Just $ ExBinary "<" (ExIdentity "i") (ExInteger 5))
          (Just $ ExSuffix "++" (ExIdentity "i"))
          (StExpression
            (ExBinary "+=" (ExIdentity "sum") (ExIdentity "i"))))
      exec p_statement "for (int i = 0; i < 5; i++) sum += i;" `shouldBe`
        (show $ StFor
          (Just $ InitStmt (Type "int") [SetVar "i" (ExInteger 0)])
          (Just $ ExBinary "<" (ExIdentity "i") (ExInteger 5))
          (Just $ ExSuffix "++" (ExIdentity "i"))
          (StExpression
            (ExBinary "+=" (ExIdentity "sum") (ExIdentity "i"))))
    it "nothing" $ do
      exec p_statement_for "for (;;) true;" `shouldBe`
        (show $ StFor Nothing Nothing Nothing (StExpression (ExBoolean True))) 
