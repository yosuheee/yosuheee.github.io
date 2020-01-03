module PrimitiveSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Primitive

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

  describe "number" $ do
    it "p_hex" $ do
      exec p_hex "0xaf" `shouldBe` (show $ NuHex "af" SuNone)
      exec p_hex "0x1A" `shouldBe` (show $ NuHex "1A" SuNone)
      exec p_hex "0x1g" `shouldNotBe` (show $ NuHex "1g" SuNone)
      exec p_hex "0x" `shouldNotBe` (show $ NuHex "" SuNone)
    it "p_oct" $ do
      exec p_oct "010" `shouldBe` (show $ NuOct "10" SuNone)
      exec p_oct "023" `shouldBe` (show $ NuOct "23" SuNone)
      exec p_oct "005" `shouldNotBe` (show $ NuOct "05" SuNone)
    it "p_bin" $ do
      exec p_bin "0b10" `shouldBe` (show $ NuBin "10" SuNone)
      exec p_bin "0b1100011010" `shouldBe` (show $ NuBin "1100011010" SuNone)
      exec p_bin "0b01" `shouldNotBe` (show $ NuBin "01" SuNone)
      exec p_bin "0b" `shouldNotBe` (show $ NuBin "" SuNone)
    it "p_dec" $ do
      exec p_dec "1234" `shouldBe` (show $ NuDec "1234" SuNone)
      exec p_dec "1000" `shouldBe` (show $ NuDec "1000" SuNone)
      exec p_dec "01" `shouldNotBe` (show $ NuDec "01" SuNone)
    it "p_num" $ do
      exec p_num "0xaf" `shouldBe` (show $ NuHex "af" SuNone)
      exec p_num "0100" `shouldBe` (show $ NuOct "100" SuNone)
      exec p_num "0b10" `shouldBe` (show $ NuBin "10" SuNone)
      exec p_num "1234" `shouldBe` (show $ NuDec "1234" SuNone)

  describe "p_num_suffix" $ do
    it "llu" $ do
      exec p_num "10llu" `shouldBe` (show $ NuDec "10" SuLLU)
      exec p_num "01llu" `shouldBe` (show $ NuOct "1" SuLLU)
      exec p_num "10uLl" `shouldBe` (show $ NuDec "10" SuLLU)
    it "ll" $ do
      exec p_num "10ll" `shouldBe` (show $ NuDec "10" SuLL)
    it "lu" $ do
      exec p_num "10lu" `shouldBe` (show $ NuDec "10" SuLU)
    it "l" $ do
      exec p_num "10l" `shouldBe` (show $ NuDec "10" SuL)
    it "u" $ do
      exec p_num "10u" `shouldBe` (show $ NuDec "10" SuU)

  describe "p_string_literal" $ do
    it "\"test\"" $ do
      exec p_string_literal "\"test\"" `shouldBe` (show $ StrLiteral StNone StNon "test")

  describe "p_string prefix" $ do
    it "u8\"test\"" $ do
      exec p_string_literal "u8\"test\"" `shouldBe` (show $ StrLiteral Stu8 StNon "test")
    it "L\"test\"" $ do
      exec p_string_literal "L\"test\"" `shouldBe` (show $ StrLiteral StL StNon "test")
    it "u\"test\"" $ do
      exec p_string_literal "u\"test\"" `shouldBe` (show $ StrLiteral Stu StNon "test")
    it "U\"test\"" $ do
      exec p_string_literal "U\"test\"" `shouldBe` (show $ StrLiteral StU StNon "test")
    it "R\"test\"" $ do
      exec p_string_literal "R\"test\"" `shouldBe` (show $ StrLiteral StR StNon "test")

    it "u8R\"test\"" $ do
      exec p_string_literal "u8R\"test\"" `shouldBe` (show $ StrLiteral Stu8R StNon "test")
    it "LR\"test\"" $ do
      exec p_string_literal "LR\"test\"" `shouldBe` (show $ StrLiteral StLR StNon "test")
    it "uR\"test\"" $ do
      exec p_string_literal "uR\"test\"" `shouldBe` (show $ StrLiteral StuR StNon "test")
    it "UR\"test\"" $ do
      exec p_string_literal "UR\"test\"" `shouldBe` (show $ StrLiteral StUR StNon "test")

  describe "p_string suffix" $ do
    it "\"test\"s" $ do
      exec p_string_literal "\"test\"s" `shouldBe` (show $ StrLiteral StNone Sts "test")

  describe "escape sequence" $ do
    let expected str = show $ StrLiteral StNone StNon str
    it "\"test\\\\\"" $ do
      exec p_string_literal "\"test\\\\\"" `shouldBe` expected "test\\"
    it "\"test\\'\"" $ do
      exec p_string_literal "\"test\\'\"" `shouldBe` expected "test'"
    it "\"test\\\"\"" $ do
      exec p_string_literal "\"test\\\"\"" `shouldBe` expected "test\""
    it "\"test\\a\"" $ do
      exec p_string_literal "\"test\\a\"" `shouldBe` expected "test\a"
    it "\"test\\b\"" $ do
      exec p_string_literal "\"test\\b\"" `shouldBe` expected "test\b"
    it "\"test\\f\"" $ do
      exec p_string_literal "\"test\\f\"" `shouldBe` expected "test\f"
    it "\"test\\n\"" $ do
      exec p_string_literal "\"test\\n\"" `shouldBe` expected "test\n"
    it "\"test\\r\"" $ do
      exec p_string_literal "\"test\\r\"" `shouldBe` expected "test\r"
    it "\"test\\t\"" $ do
      exec p_string_literal "\"test\\t\"" `shouldBe` expected "test\t"
    it "\"test\\0\"" $ do
      exec p_string_literal "\"test\\0\"" `shouldBe` expected "test\0"
    it "\"test\\xff\"" $ do
      exec p_string_literal "\"test\\xff\"" `shouldBe` expected "test\\xff"
    it "\"test\\o77\"" $ do
      exec p_string_literal "\"test\\o77\"" `shouldBe` expected "test\\o77"

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
