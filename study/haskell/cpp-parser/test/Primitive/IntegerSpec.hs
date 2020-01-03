module Primitive.IntegerSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Primitive.Integer

spec :: Spec
spec = do
  it "p_integer" $ do
    exec p_integer "0" `shouldBe` (show $ 0)
    exec p_integer "1234" `shouldBe` (show $ 1234)
    exec p_integer "03312" `shouldBe` (show $ 1738)
    exec p_integer "0x6CA" `shouldBe` (show $ 1738)
    exec p_integer "0x6ca" `shouldBe` (show $ 1738)
    exec p_integer "0b11011001010" `shouldBe` (show $ 1738)
    exec p_integer "0b011011001010" `shouldBe` (show $ 1738)
    exec p_integer "10llu" `shouldBe` (show $ 10)
    exec p_integer "010llu" `shouldBe` (show $ 8)
    exec p_integer "10uLl" `shouldBe` (show $ 10)

  describe "number" $ do
    it "p_hex" $ do
      exec p_hex "0xaf" `shouldBe` (show $ IHex ISNone "af")
      exec p_hex "0x1A" `shouldBe` (show $ IHex ISNone "1A")
      exec p_hex "0x1g" `shouldNotBe` (show $ IHex ISNone "1g")
      exec p_hex "0x" `shouldNotBe` (show $ IHex ISNone "")
    it "p_oct" $ do
      exec p_oct "010" `shouldBe` (show $ IOct ISNone "10")
      exec p_oct "023" `shouldBe` (show $ IOct ISNone "23")
    it "p_bin" $ do
      exec p_bin "0b10" `shouldBe` (show $ IBin ISNone "10")
      exec p_bin "0b1100011010" `shouldBe` (show $ IBin ISNone "1100011010")
      exec p_bin "0b" `shouldNotBe` (show $ IBin ISNone "")
    it "p_dec" $ do
      exec p_dec "1234" `shouldBe` (show $ IDec ISNone "1234")
      exec p_dec "1000" `shouldBe` (show $ IDec ISNone "1000")
    it "p_int_literal" $ do
      exec p_int_literal "0xaf" `shouldBe` (show $ IHex ISNone "af")
      exec p_int_literal "0100" `shouldBe` (show $ IOct ISNone "100")
      exec p_int_literal "0b10" `shouldBe` (show $ IBin ISNone "10")
      exec p_int_literal "1234" `shouldBe` (show $ IDec ISNone "1234")
      exec p_int_literal "0" `shouldBe` (show $ IDec ISNone "0")

  describe "p_num_suffix" $ do
    it "llu" $ do
      exec p_int_literal "10llu" `shouldBe` (show $ IDec ISLLU "10")
      exec p_int_literal "01llu" `shouldBe` (show $ IOct ISLLU "1")
      exec p_int_literal "10uLl" `shouldBe` (show $ IDec ISLLU "10")
    it "ll" $ do
      exec p_int_literal "10ll" `shouldBe` (show $ IDec ISLL "10")
    it "lu" $ do
      exec p_int_literal "10lu" `shouldBe` (show $ IDec ISLU "10")
    it "l" $ do
      exec p_int_literal "10l" `shouldBe` (show $ IDec ISL "10")
    it "u" $ do
      exec p_int_literal "10u" `shouldBe` (show $ IDec ISU "10")

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
