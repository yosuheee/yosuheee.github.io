module ExpressionSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Expression

spec :: Spec
spec = do
  describe "p_number" $ do
    it "accept '12345'" $
      exec p_number "12345" `shouldBe` (show 12345)

  describe "p_identity" $ do
    it "accept start underscore" $ do
      let input = "_anaconda eggplant"
      exec p_identity input `shouldBe` (show "_anaconda")
    it "accept only _" $ do
      let input = "_ eggplant"
      exec p_identity input `shouldBe` (show "_")
    it "reject to start digit" $ do
      let input = "1 a"
      exec p_identity input `shouldNotBe` (show "1")

  describe "p_identity_char" $ do
    it "accept '_' and 'a' and 'A', reject '1'" $ do
      exec p_identity_char "_" `shouldBe` (show '_')
      exec p_identity_char "a" `shouldBe` (show 'a')
      exec p_identity_char "A" `shouldBe` (show 'A')
      exec p_identity_char "1" `shouldNotBe` (show '1')

  describe "p_identity_and_digit_char" $ do
    it "accept '_' and 'a' and 'A' and '1'" $ do
      exec p_identity_and_digit_char "_" `shouldBe` (show '_')
      exec p_identity_and_digit_char "a" `shouldBe` (show 'a')
      exec p_identity_and_digit_char "A" `shouldBe` (show 'A')
      exec p_identity_and_digit_char "1" `shouldBe` (show '1')

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

  describe "p_expression" $ do
    it "accept '123'" $
      exec p_expression "123" `shouldBe` (show $ ExInt 123)
    it "accept 'ana'" $
      exec p_expression "ana" `shouldBe` (show $ ExIdentity "ana")
    it "accept '1.5'" $
      exec p_expression "1.5" `shouldBe` (show $ ExDouble 1.5)
    it "accept '1 ? 2 : 3'" $ do
      exec p_expression "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInt 1, ExInt 2, ExInt 3))
    it "accept 'throw 123'" $ do
      exec p_expression "throw 123" `shouldBe`
        (show $ ExThrow (ExInt 123))

  describe "p_ternary" $ do
    it "accept '1 ? 2 : 3'" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInt 1, ExInt 2, ExInt 3))

  describe "p_throw" $ do
    it "accept 'throw 1'" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExThrow (ExInt 1))

  describe "some operator" $ do
    it "accept some operator" $ do
      let input = "1 * 2 + 3 << 4 <=> 5 <= 6 == 7 & 8 ^ 9 | 10 && 11 || 12 = 13"
      exec p_priority_16 input `shouldBe` (show $
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
        ExList (
          ExInt 1
          , [("*"  , ExInt  2)])
          , [("+"  , ExInt  3)])
          , [("<<" , ExInt  4)])
          , [("<=>", ExInt  5)])
          , [("<=" , ExInt  6)])
          , [("==" , ExInt  7)])
          , [("&"  , ExInt  8)])
          , [("^"  , ExInt  9)])
          , [("|"  , ExInt 10)])
          , [("&&" , ExInt 11)])
          , [("||" , ExInt 12)])
          , [("="  , ExInt 13)]))

    it "accept some operator" $ do
      let input = "1 = 2 || 3 && 4 | 5 ^ 6 & 7 == 8 <= 9 <=> 10 << 11 + 12 * 13"
      exec p_priority_16 input `shouldBe` (show $
        ExList (ExInt  1, [("="  , 
        ExList (ExInt  2, [("||" , 
        ExList (ExInt  3, [("&&" , 
        ExList (ExInt  4, [("|"  , 
        ExList (ExInt  5, [("^"  , 
        ExList (ExInt  6, [("&"  , 
        ExList (ExInt  7, [("==" , 
        ExList (ExInt  8, [("<=" , 
        ExList (ExInt  9, [("<=>", 
        ExList (ExInt 10, [("<<" , 
        ExList (ExInt 11, [("+"  , 
        ExList (ExInt 12, [("*"  , ExInt 13)])
        )])
        )])
        )])
        )])
        )])
        )])
        )])
        )])
        )])
        )])
        )])
        )

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
    expected = show $ ExList (ExInt 1, [(op, ExInt 2)])
