module ExpressionSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Util

import Expression

spec :: Spec
spec = do
  describe "p_expression" $ do
    it "accept '123'" $
      exec p_expression "123" `shouldBe` (show $ ExInteger 123)
    it "accept 'ana'" $
      exec p_expression "ana" `shouldBe` (show $ ExIdentity "ana")
    it "accept '1.5'" $
      exec p_expression "1.5" `shouldBe` (show $ ExDouble 1.5)
    it "accept '1 ? 2 : 3'" $ do
      exec p_expression "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1) (ExInteger 2) (ExInteger 3))
    it "accept 'throw 123'" $ do
      exec p_expression "throw 123" `shouldBe`
        (show $ ExSuffix "throw" (ExInteger 123))
    it "accept '1 + ++a + a++'" $ do
      exec p_expression "1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExInteger 1) (ExSuffix "++" (ExIdentity "a"))) (ExPrefix "++" (ExIdentity "a")))
    it "accept '+1 + ++a + a++'" $ do
      exec p_expression "+1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExSuffix "+" (ExInteger 1)) (ExSuffix "++" (ExIdentity "a"))) (ExPrefix "++" (ExIdentity "a")))
    it "accept '1 && 2'" $ do
      exec p_expression "1 && 2" `shouldBe`
        (show $ ExBinary "&&" (ExInteger 1) (ExInteger 2))
    it "accept '!~1'" $ do
      exec p_expression "!~1" `shouldBe`
        (show $ ExSuffix "!" (ExSuffix "~" (ExInteger 1)))
    it "accept '++a++'" $ do
      exec p_expression "++a--" `shouldBe`
        (show $ ExSuffix "++" (ExPrefix "--" (ExIdentity "a")))
    it "accept 'a++--'" $ do
      exec p_expression "a++--" `shouldBe`
        (show $ ExPrefix "--" (ExPrefix "++" (ExIdentity "a")))
    it "accept 'true' or 'false" $ do
      exec p_expression "true" `shouldBe` (show $ ExBoolean True)
      exec p_expression "false" `shouldBe` (show $ ExBoolean False)
    it "p_boolean denied 'trues', and p_identity accept it" $
      exec (p_expression <* eof) "trues" `shouldBe` (show $ ExIdentity "trues")

  describe "p_ternary" $ do
    it "accept '1 ? 2 : 3'" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1) (ExInteger 2) (ExInteger 3))

  describe "p_throw" $ do
    it "accept 'throw 1'" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExSuffix "throw" (ExInteger 1))

  it "p_priority_5_15" $ do
    exec p_priority_5_15 "1 + 2" `shouldBe` (show $
      ExBinary "+" (ExInteger 1) (ExInteger 2))
    exec p_priority_5_15 "1 + 2 - 3" `shouldBe` (show $
      ExBinary "-" (ExBinary "+" (ExInteger 1) (ExInteger 2)) (ExInteger 3))
    exec p_priority_5_15 "1 + 2 * 3" `shouldBe` (show $
      ExBinary "+" (ExInteger 1) (ExBinary "*" (ExInteger 2) (ExInteger 3)))
    exec p_priority_5_15 "1 * 2 << 3 % 4" `shouldBe` (show $
      ExBinary "<<" (ExBinary "*" (ExInteger 1) (ExInteger 2)) (ExBinary "%" (ExInteger 3) (ExInteger 4)))
    exec p_priority_5_15 "1 - 2 / 3 + 4 == 5" `shouldBe` (show $
      ExBinary "==" (ExBinary "+" (ExBinary "-" (ExInteger 1) (ExBinary "/" (ExInteger 2) (ExInteger 3))) (ExInteger 4)) (ExInteger 5))

  it "p_priority_2" $ do
    exec p_priority_2 "a++" `shouldBe` (
      show $ ExPrefix "++" (ExIdentity "a"))
    exec p_priority_2 "a--" `shouldBe` (
      show $ ExPrefix "--" (ExIdentity "a"))

  it "p_priority_3" $ do
    exec p_priority_3 "++a" `shouldBe` (show $ ExSuffix "++" (ExIdentity "a"))
    exec p_priority_3 "--a" `shouldBe` (show $ ExSuffix "--" (ExIdentity "a"))
    exec p_priority_3 "+a"  `shouldBe` (show $ ExSuffix "+" (ExIdentity "a"))
    exec p_priority_3 "-a"  `shouldBe` (show $ ExSuffix "-" (ExIdentity "a"))
    exec p_priority_3 "!1"  `shouldBe` (show $ ExSuffix "!" (ExInteger 1))
    exec p_priority_3 "~1"  `shouldBe` (show $ ExSuffix "~" (ExInteger 1))
    exec p_priority_3 "*a"  `shouldBe` (show $ ExSuffix "*" (ExIdentity "a"))
    exec p_priority_3 "&a"  `shouldBe` (show $ ExSuffix "&" (ExIdentity "a"))

  describe "binary operator" $ do
    flip forM_ it_spec_binop $ concat [
      ["*", "/", "%"],
      ["+", "-"],
      ["<<", ">>"],
      ["<=>"],
      ["<", ">", "<=", ">="],
      ["==", "!="],
      ["&"], ["^"], ["|"], ["&&"], ["||"],
      ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]]

it_spec_binop :: String -> SpecWith (Arg Expectation)
it_spec_binop op =
  it ("accept '1 " ++ op ++ " 2'") $
    exec p_expression ("1" ++ op ++ "2") `shouldBe` expected
  where
    expected = show $ ExBinary op (ExInteger 1) (ExInteger 2)
