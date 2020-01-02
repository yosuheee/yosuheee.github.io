module ExpressionSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Expression

spec :: Spec
spec = do
  describe "p_number" $ do
    it "accept \"12345\"" $
      exec p_number "12345" `shouldBe` "12345"

  describe "p_identity" $ do
    it "accept start underscore" $ do
      let input = "_anaconda eggplant"
      exec p_identity input `shouldBe` "\"_anaconda\""
    it "accept only _" $ do
      let input = "_ eggplant"
      exec p_identity input `shouldBe` "\"_\""
    it "reject to start digit" $ do
      let input = "1 a"
      exec p_identity input `shouldNotBe` "\"1\""

  describe "p_identity_char" $ do
    it "accept '_' and 'a' and 'A', reject '1'" $ do
      exec p_identity_char "_" `shouldBe` "'_'"
      exec p_identity_char "a" `shouldBe` "'a'"
      exec p_identity_char "A" `shouldBe` "'A'"
      exec p_identity_char "1" `shouldNotBe` "'1'"

  describe "p_identity_and_digit_char" $ do
    it "accept '_' and 'a' and 'A' and '1'" $ do
      exec p_identity_and_digit_char "_" `shouldBe` "'_'"
      exec p_identity_and_digit_char "a" `shouldBe` "'a'"
      exec p_identity_and_digit_char "A" `shouldBe` "'A'"
      exec p_identity_and_digit_char "1" `shouldBe` "'1'"

  describe "p_expression" $ do
    it "accept \"123\"" $
      exec p_expression "123" `shouldBe` "ExInteger 123"
    it "accept \"1 + 2\"" $
      exec p_expression "1 + 2" `shouldBe` "ExTuple (ExInteger 1,'+',ExInteger 2)"
    it "accept \"anaconda\"" $
      exec p_expression "anaconda" `shouldBe` "ExIdentity \"anaconda\""
    it "accept \"1 + ana\"" $
      exec p_expression "1 + ana" `shouldBe` "ExTuple (ExInteger 1,'+',ExIdentity \"ana\")"
    it "accept \"egg + 5\"" $
      exec p_expression "egg + 5" `shouldBe` "ExTuple (ExIdentity \"egg\",'+',ExInteger 5)"
    it "accept \"1 - 2\"" $
      exec p_expression "1 - 2" `shouldBe` "ExTuple (ExInteger 1,'-',ExInteger 2)"
    it "accept \"1 * 2\"" $
      exec p_expression "1 * 2" `shouldBe` "ExTuple (ExInteger 1,'*',ExInteger 2)"
    it "accept \"1 / 2\"" $
      exec p_expression "1 / 2" `shouldBe` "ExTuple (ExInteger 1,'/',ExInteger 2)"
    it "accept \"1.5\"" $
      exec p_expression "1.5" `shouldBe` "ExDouble 1.5"

  describe "p_mul_div" $ do
    it "accept \"1*2\"" $ do
      exec p_mul_div "1*2" `shouldBe`
        (show $ ExList (ExInteger 1, [("*", ExInteger 2)]))
    it "accept \"1 * 2\"" $ do
      exec p_mul_div "1 * 2" `shouldBe`
        (show $ ExList (ExInteger 1, [("*", ExInteger 2)]))
    it "accept \"1 * 2 * 3\"" $ do
      exec p_mul_div "1 * 2 * 3" `shouldBe`
        (show $ ExList (ExInteger 1, [("*", ExInteger 2), ("*", ExInteger 3)]))

  describe "p_add_sub" $ do
    it "accept \"1 + 2\"" $ do
      exec p_add_sub "1 + 2" `shouldBe`
        (show $ ExList (ExInteger 1, [("+", ExInteger 2)]))
    it "accept \"1 + 2 + 3\"" $ do
      exec p_add_sub "1 + 2 + 3" `shouldBe`
        (show $ ExList (ExInteger 1, [("+", ExInteger 2), ("+", ExInteger 3)]))
    it "accept \"1 + 2 * 3\"" $ do
      exec p_add_sub "1 + 2 * 3" `shouldBe`
        (show $ ExList (ExInteger 1, [("+", ExList (ExInteger 2, [("*", ExInteger 3)]))]))
    it "accept \"1 * 2 + 3\"" $ do
      exec p_add_sub "1 * 2 + 3" `shouldBe`
        (show $ ExList (ExList (ExInteger 1, [("*", ExInteger 2)]), [("+", ExInteger 3)]))

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
