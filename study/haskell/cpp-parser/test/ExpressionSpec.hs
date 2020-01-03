module ExpressionSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Primitive.Char
import Primitive.Double
import Primitive.Integer
import Primitive.String
import Expression

ex_int num = ExInt (IDec ISNone (show num))
ex_identity str = ExIdentity str
ex_double num = ExDbl (DblLiteral DSNone (show num) "+1") 

spec :: Spec
spec = do
  describe "p_expression" $ do
    it "accept '123'" $
      exec p_expression "123" `shouldBe` (show $ ex_int 123)
    it "accept 'ana'" $
      exec p_expression "ana" `shouldBe` (show $ ex_identity "ana")
    it "accept '1.5'" $
      exec p_expression "1.5" `shouldBe` (show $ ex_double 1.5)
    it "accept '1 ? 2 : 3'" $ do
      exec p_expression "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ex_int 1) (ex_int 2) (ex_int 3))
    it "accept 'throw 123'" $ do
      exec p_expression "throw 123" `shouldBe`
        (show $ ExSuffix "throw" (ex_int 123))
    it "accept '1 + ++a + a++'" $ do
      exec p_expression "1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ex_int 1) (ExSuffix "++" (ex_identity "a"))) (ExPrefix "++" (ex_identity "a")))
    it "accept '+1 + ++a + a++'" $ do
      exec p_expression "+1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExSuffix "+" (ex_int 1)) (ExSuffix "++" (ex_identity "a"))) (ExPrefix "++" (ex_identity "a")))
    it "accept '1 && 2'" $ do
      exec p_expression "1 && 2" `shouldBe`
        (show $ ExBinary "&&" (ex_int 1) (ex_int 2))
    it "accept '!~1'" $ do
      exec p_expression "!~1" `shouldBe`
        (show $ ExSuffix "!" (ExSuffix "~" (ex_int 1)))
    it "accept '++a++'" $ do
      exec p_expression "++a--" `shouldBe`
        (show $ ExSuffix "++" (ExPrefix "--" (ex_identity "a")))
    it "accept 'a++--'" $ do
      exec p_expression "a++--" `shouldBe`
        (show $ ExPrefix "--" (ExPrefix "++" (ex_identity "a")))

  describe "p_ternary" $ do
    it "accept '1 ? 2 : 3'" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ex_int 1) (ex_int 2) (ex_int 3))

  describe "p_throw" $ do
    it "accept 'throw 1'" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExSuffix "throw" (ex_int 1))

  it "p_priority_5_15" $ do
    exec p_priority_5_15 "1 + 2" `shouldBe` (show $
      ExBinary "+" (ex_int 1) (ex_int 2))
    exec p_priority_5_15 "1 + 2 - 3" `shouldBe` (show $
      ExBinary "-" (ExBinary "+" (ex_int 1) (ex_int 2)) (ex_int 3))
    exec p_priority_5_15 "1 + 2 * 3" `shouldBe` (show $
      ExBinary "+" (ex_int 1) (ExBinary "*" (ex_int 2) (ex_int 3)))
    exec p_priority_5_15 "1 * 2 << 3 % 4" `shouldBe` (show $
      ExBinary "<<" (ExBinary "*" (ex_int 1) (ex_int 2)) (ExBinary "%" (ex_int 3) (ex_int 4)))
    exec p_priority_5_15 "1 - 2 / 3 + 4 == 5" `shouldBe` (show $
      ExBinary "==" (ExBinary "+" (ExBinary "-" (ex_int 1) (ExBinary "/" (ex_int 2) (ex_int 3))) (ex_int 4)) (ex_int 5))

  it "p_priority_2" $ do
    exec p_priority_2 "a++" `shouldBe` (
      show $ ExPrefix "++" (ex_identity "a"))
    exec p_priority_2 "a--" `shouldBe` (
      show $ ExPrefix "--" (ex_identity "a"))

  it "p_priority_3" $ do
    exec p_priority_3 "++a" `shouldBe` (
      show $ ExSuffix "++" (ex_identity "a"))
    exec p_priority_3 "--a" `shouldBe` (
      show $ ExSuffix "--" (ex_identity "a"))
    exec p_priority_3 "+a" `shouldBe` (
      show $ ExSuffix "+" (ex_identity "a"))
    exec p_priority_3 "-a" `shouldBe` (
      show $ ExSuffix "-" (ex_identity "a"))
    exec p_priority_3 "!1" `shouldBe` (
      show $ ExSuffix "!" (ex_int 1))
    exec p_priority_3 "~1" `shouldBe` (
      show $ ExSuffix "~" (ex_int 1))
    exec p_priority_3 "*a" `shouldBe` (
      show $ ExSuffix "*" (ex_identity "a"))
    exec p_priority_3 "&a" `shouldBe` (
      show $ ExSuffix "&" (ex_identity "a"))

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

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val

it_spec_binop :: String -> SpecWith (Arg Expectation)
it_spec_binop op =
  it ("accept '1 " ++ op ++ " 2'") $
    exec p_expression ("1" ++ op ++ "2") `shouldBe` expected
  where
    expected = show $ ExBinary op (ex_int 1) (ex_int 2)
