module ExpressionSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Expression

spec :: Spec
spec = do
  describe "p_expression" $ do
    it "accept '123'" $
      exec p_expression "123" `shouldBe` (show $ ExInt 123)
    it "accept 'ana'" $
      exec p_expression "ana" `shouldBe` (show $ ExIdentity "ana")
    it "accept '1.5'" $
      exec p_expression "1.5" `shouldBe` (show $ ExDouble 1.5)
    it "accept '1 ? 2 : 3'" $ do
      exec p_expression "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInt 1) (ExInt 2) (ExInt 3))
    it "accept 'throw 123'" $ do
      exec p_expression "throw 123" `shouldBe`
        (show $ ExSuffix "throw" (ExInt 123))
    it "accept '1 + ++a + a++'" $ do
      exec p_expression "1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExInt 1) (ExSuffix "++" (ExIdentity "a"))) (ExPrefix "++" (ExIdentity "a")))
    it "accept '+1 + ++a + a++'" $ do
      exec p_expression "+1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExSuffix "+" (ExInt 1)) (ExSuffix "++" (ExIdentity "a"))) (ExPrefix "++" (ExIdentity "a")))
    it "accept '1 && 2'" $ do
      exec p_expression "1 && 2" `shouldBe`
        (show $ ExBinary "&&" (ExInt 1) (ExInt 2))
    it "accept '!~1'" $ do
      exec p_expression "!~1" `shouldBe`
        (show $ ExSuffix "!" (ExSuffix "~" (ExInt 1)))

  describe "p_ternary" $ do
    it "accept '1 ? 2 : 3'" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInt 1) (ExInt 2) (ExInt 3))

  describe "p_throw" $ do
    it "accept 'throw 1'" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExSuffix "throw" (ExInt 1))

  it "p_priority_5_15" $ do
    exec p_priority_5_15 "1 + 2" `shouldBe` (show $
      ExBinary "+" (ExInt 1) (ExInt 2))
    exec p_priority_5_15 "1 + 2 - 3" `shouldBe` (show $
      ExBinary "-" (ExBinary "+" (ExInt 1) (ExInt 2)) (ExInt 3))
    exec p_priority_5_15 "1 + 2 * 3" `shouldBe` (show $
      ExBinary "+" (ExInt 1) (ExBinary "*" (ExInt 2) (ExInt 3)))
    exec p_priority_5_15 "1 * 2 << 3 % 4" `shouldBe` (show $
      ExBinary "<<" (ExBinary "*" (ExInt 1) (ExInt 2)) (ExBinary "%" (ExInt 3) (ExInt 4)))
    exec p_priority_5_15 "1 - 2 / 3 + 4 == 5" `shouldBe` (show $
      ExBinary "==" (ExBinary "+" (ExBinary "-" (ExInt 1) (ExBinary "/" (ExInt 2) (ExInt 3))) (ExInt 4)) (ExInt 5))

  describe "binary operator" $ do
    flip forM_ it_spec_binop $ concat [
      ["*", "/", "%"],
      ["+", "-"],
      ["<<", ">>"],
      ["<=>"],
      ["<=", ">=", "<", ">"],
      ["==", "!="],
      ["&"], ["^"], ["|"], ["&&"], ["||"],
      ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]]

  it "priority 2 unary" $ do
    exec p_prefix_increment "a++" `shouldBe` (
      show $ ExPrefix "++" (ExIdentity "a"))
    exec p_prefix_decrement "a--" `shouldBe` (
      show $ ExPrefix "--" (ExIdentity "a"))

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
    expected = show $ ExBinary op (ExInt 1) (ExInt 2)
