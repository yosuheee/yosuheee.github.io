module ExpressionSpec (spec) where

import Data.List (foldl')
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
    it "accept all binary operators" $ do
      spec_binop "*"
      spec_binop "/"
      spec_binop "%"
      spec_binop "+"
      spec_binop "-"
      spec_binop "<<"
      spec_binop ">>"
      spec_binop "<=>"
      spec_binop "<="
      spec_binop ">="
      spec_binop "<"
      spec_binop ">"
      spec_binop "=="
      spec_binop "!="
      spec_binop "&"
      spec_binop "^"
      spec_binop "|"
      spec_binop "&&"
      spec_binop "||"
      spec_binop "="
      spec_binop "+="
      spec_binop "-="
      spec_binop "*="
      spec_binop "/="
      spec_binop "%="
      spec_binop "<<="
      spec_binop ">>="
      spec_binop "&="
      spec_binop "^="
      spec_binop "|="

  describe "p_expression" $ do
    it "accept '123'" $
      exec p_expression "123" `shouldBe` (show $ ExInteger 123)
    it "accept 'ana'" $
      exec p_expression "ana" `shouldBe` (show $ ExIdentity "ana")
    it "accept '1.5'" $
      exec p_expression "1.5" `shouldBe` (show $ ExDouble 1.5)
    it "accept '1 ? 2 : 3'" $ do
      exec p_expression "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1, ExInteger 2, ExInteger 3))
    it "accept 'throw 123'" $ do
      exec p_expression "throw 123" `shouldBe`
        (show $ ExThrow (ExInteger 123))

  describe "p_ternary" $ do
    it "accept '1 ? 2 : 3'" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1, ExInteger 2, ExInteger 3))

  describe "p_throw" $ do
    it "accept 'throw 1'" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExThrow (ExInteger 1))

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
          ExInteger 1
          , [("*"  , ExInteger  2)])
          , [("+"  , ExInteger  3)])
          , [("<<" , ExInteger  4)])
          , [("<=>", ExInteger  5)])
          , [("<=" , ExInteger  6)])
          , [("==" , ExInteger  7)])
          , [("&"  , ExInteger  8)])
          , [("^"  , ExInteger  9)])
          , [("|"  , ExInteger 10)])
          , [("&&" , ExInteger 11)])
          , [("||" , ExInteger 12)])
          , [("="  , ExInteger 13)]))

    it "accept some operator" $ do
      let input = "1 = 2 || 3 && 4 | 5 ^ 6 & 7 == 8 <= 9 <=> 10 << 11 + 12 * 13"
      exec p_priority_16 input `shouldBe` (show $
        ExList (ExInteger  1, [("="  , 
        ExList (ExInteger  2, [("||" , 
        ExList (ExInteger  3, [("&&" , 
        ExList (ExInteger  4, [("|"  , 
        ExList (ExInteger  5, [("^"  , 
        ExList (ExInteger  6, [("&"  , 
        ExList (ExInteger  7, [("==" , 
        ExList (ExInteger  8, [("<=" , 
        ExList (ExInteger  9, [("<=>", 
        ExList (ExInteger 10, [("<<" , 
        ExList (ExInteger 11, [("+"  , 
        ExList (ExInteger 12, [("*"  , ExInteger 13)])
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

spec_binop :: String -> Expectation
spec_binop op =
  exec p_expression ("1" ++ op ++ "2") `shouldBe`
    (show $ ExList (ExInteger 1, [(op, ExInteger 2)]))
