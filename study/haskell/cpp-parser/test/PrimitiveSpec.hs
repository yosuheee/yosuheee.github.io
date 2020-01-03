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

  describe "p_str_literal" $ do
    it "\"test\"" $ do
      exec p_str_literal "\"test\"" `shouldBe` (show $ StrLiteral StNone SSNone "test")

  describe "p_string prefix" $ do
    it "u8\"test\"" $ do
      exec p_str_literal "u8\"test\"" `shouldBe` (show $ StrLiteral SPu8 SSNone "test")
    it "L\"test\"" $ do
      exec p_str_literal "L\"test\"" `shouldBe` (show $ StrLiteral SPL SSNone "test")
    it "u\"test\"" $ do
      exec p_str_literal "u\"test\"" `shouldBe` (show $ StrLiteral SPu SSNone "test")
    it "U\"test\"" $ do
      exec p_str_literal "U\"test\"" `shouldBe` (show $ StrLiteral SPU SSNone "test")
    it "R\"test\"" $ do
      exec p_str_literal "R\"test\"" `shouldBe` (show $ StrLiteral SPR SSNone "test")

    it "u8R\"test\"" $ do
      exec p_str_literal "u8R\"test\"" `shouldBe` (show $ StrLiteral SPu8R SSNone "test")
    it "LR\"test\"" $ do
      exec p_str_literal "LR\"test\"" `shouldBe` (show $ StrLiteral SPLR SSNone "test")
    it "uR\"test\"" $ do
      exec p_str_literal "uR\"test\"" `shouldBe` (show $ StrLiteral SPuR SSNone "test")
    it "UR\"test\"" $ do
      exec p_str_literal "UR\"test\"" `shouldBe` (show $ StrLiteral SPUR SSNone "test")

  describe "p_string suffix" $ do
    it "\"test\"s" $ do
      exec p_str_literal "\"test\"s" `shouldBe` (show $ StrLiteral StNone SSs "test")

  it "escaped str" $ do
    let expected str = show $ StrLiteral StNone SSNone str
    exec p_str_literal "\"test\\\\\"" `shouldBe` expected "test\\"
    exec p_str_literal "\"test\\'\"" `shouldBe` expected "test'"
    exec p_str_literal "\"test\\\"\"" `shouldBe` expected "test\""
    exec p_str_literal "\"test\\a\"" `shouldBe` expected "test\a"
    exec p_str_literal "\"test\\b\"" `shouldBe` expected "test\b"
    exec p_str_literal "\"test\\f\"" `shouldBe` expected "test\f"
    exec p_str_literal "\"test\\n\"" `shouldBe` expected "test\n"
    exec p_str_literal "\"test\\r\"" `shouldBe` expected "test\r"
    exec p_str_literal "\"test\\t\"" `shouldBe` expected "test\t"
    exec p_str_literal "\"test\\0\"" `shouldBe` expected "test\0"
    exec p_str_literal "\"test\\xff\"" `shouldBe` expected "test\\xff"
    exec p_str_literal "\"test\\o77\"" `shouldBe` expected "test\\o77"

  describe "p_chr_literal prefix" $ do
    it "u8't'" $ do
      exec p_chr_literal "u8't'" `shouldBe` (show $ ChrLiteral CSu8 't')
    it "u't'" $ do
      exec p_chr_literal "u't'" `shouldBe` (show $ ChrLiteral CSu 't')
    it "U't'" $ do
      exec p_chr_literal "U't'" `shouldBe` (show $ ChrLiteral CSU 't')
    it "L't'" $ do
      exec p_chr_literal "L't'" `shouldBe` (show $ ChrLiteral CSL 't')

  it "escaped chr" $ do
    exec p_chr_literal "'\\\\'" `shouldBe` (show $ ChrLiteral CSNone '\\')
    exec p_chr_literal "'\\\''" `shouldBe` (show $ ChrLiteral CSNone '\'')
    exec p_chr_literal "'\\\"'" `shouldBe` (show $ ChrLiteral CSNone '"')
    exec p_chr_literal "'\\a'" `shouldBe` (show $ ChrLiteral CSNone '\a')
    exec p_chr_literal "'\\b'" `shouldBe` (show $ ChrLiteral CSNone '\b')
    exec p_chr_literal "'\\f'" `shouldBe` (show $ ChrLiteral CSNone '\f')
    exec p_chr_literal "'\\n'" `shouldBe` (show $ ChrLiteral CSNone '\n')
    exec p_chr_literal "'\\r'" `shouldBe` (show $ ChrLiteral CSNone '\r')
    exec p_chr_literal "'\\t'" `shouldBe` (show $ ChrLiteral CSNone '\t')
    exec p_chr_literal "'\\0'" `shouldBe` (show $ ChrLiteral CSNone '\0')

  it "simple" $ do
    exec p_simple_double "5e10" `shouldBe` (show $ DblLiteral DSNone "5" "+10")
    exec p_simple_double "5e5f" `shouldBe` (show $ DblLiteral DSf "5" "+5")
    exec p_simple_double "5e5F" `shouldBe` (show $ DblLiteral DSF "5" "+5")
    exec p_simple_double "5e5l" `shouldBe` (show $ DblLiteral DSl "5" "+5")
    exec p_simple_double "5e5L" `shouldBe` (show $ DblLiteral DSL "5" "+5")
  
  it "front" $ do
    exec p_front_double "5.e-10" `shouldBe` (show $ DblLiteral DSNone "5" "-10")
    exec p_front_double "5." `shouldBe` (show $ DblLiteral DSNone "5" "+1")
    exec p_front_double "5.l" `shouldBe` (show $ DblLiteral DSl "5" "+1")
    exec p_front_double "2.e+3F" `shouldBe` (show $ DblLiteral DSF "2" "+3")

  it "all double" $ do
    exec p_all_double "5.3e-13" `shouldBe` (show $ DblLiteral DSNone "5.3" "-13")
    exec p_all_double ".3e+2L" `shouldBe` (show $ DblLiteral DSL "0.3" "+2")
    exec p_all_double "0.158l" `shouldBe` (show $ DblLiteral DSl "0.158" "+1")
    exec p_all_double "1.23E-13f" `shouldBe` (show $ DblLiteral DSf "1.23" "-13")

  it "double" $ do
    exec p_dbl_literal "5e10" `shouldBe` (show $ DblLiteral DSNone "5" "+10")
    exec p_dbl_literal "5e5f" `shouldBe` (show $ DblLiteral DSf "5" "+5")
    exec p_dbl_literal "5e5F" `shouldBe` (show $ DblLiteral DSF "5" "+5")
    exec p_dbl_literal "5e5l" `shouldBe` (show $ DblLiteral DSl "5" "+5")
    exec p_dbl_literal "5e5L" `shouldBe` (show $ DblLiteral DSL "5" "+5")
    exec p_dbl_literal "5.e-10" `shouldBe` (show $ DblLiteral DSNone "5" "-10")
    exec p_dbl_literal "5." `shouldBe` (show $ DblLiteral DSNone "5" "+1")
    exec p_dbl_literal "5.l" `shouldBe` (show $ DblLiteral DSl "5" "+1")
    exec p_dbl_literal "2.e+3F" `shouldBe` (show $ DblLiteral DSF "2" "+3")
    exec p_dbl_literal "5.3e-13" `shouldBe` (show $ DblLiteral DSNone "5.3" "-13")
    exec p_dbl_literal ".3e+2L" `shouldBe` (show $ DblLiteral DSL "0.3" "+2")
    exec p_dbl_literal "0.158l" `shouldBe` (show $ DblLiteral DSl "0.158" "+1")
    exec p_dbl_literal "1.23E-13f" `shouldBe` (show $ DblLiteral DSf "1.23" "-13")

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
