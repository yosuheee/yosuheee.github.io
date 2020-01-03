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
      exec p_hex "0xaf" `shouldBe` (show $ NuHex SuNone "af")
      exec p_hex "0x1A" `shouldBe` (show $ NuHex SuNone "1A")
      exec p_hex "0x1g" `shouldNotBe` (show $ NuHex SuNone "1g")
      exec p_hex "0x" `shouldNotBe` (show $ NuHex SuNone "")
    it "p_oct" $ do
      exec p_oct "010" `shouldBe` (show $ NuOct SuNone "10")
      exec p_oct "023" `shouldBe` (show $ NuOct SuNone "23")
    it "p_bin" $ do
      exec p_bin "0b10" `shouldBe` (show $ NuBin SuNone "10")
      exec p_bin "0b1100011010" `shouldBe` (show $ NuBin SuNone "1100011010")
      exec p_bin "0b" `shouldNotBe` (show $ NuBin SuNone "")
    it "p_dec" $ do
      exec p_dec "1234" `shouldBe` (show $ NuDec SuNone "1234")
      exec p_dec "1000" `shouldBe` (show $ NuDec SuNone "1000")
    it "p_int" $ do
      exec p_int "0xaf" `shouldBe` (show $ NuHex SuNone "af")
      exec p_int "0100" `shouldBe` (show $ NuOct SuNone "100")
      exec p_int "0b10" `shouldBe` (show $ NuBin SuNone "10")
      exec p_int "1234" `shouldBe` (show $ NuDec SuNone "1234")
      exec p_int "0" `shouldBe` (show $ NuDec SuNone "0")

  describe "p_num_suffix" $ do
    it "llu" $ do
      exec p_int "10llu" `shouldBe` (show $ NuDec SuLLU "10")
      exec p_int "01llu" `shouldBe` (show $ NuOct SuLLU "1")
      exec p_int "10uLl" `shouldBe` (show $ NuDec SuLLU "10")
    it "ll" $ do
      exec p_int "10ll" `shouldBe` (show $ NuDec SuLL "10")
    it "lu" $ do
      exec p_int "10lu" `shouldBe` (show $ NuDec SuLU "10")
    it "l" $ do
      exec p_int "10l" `shouldBe` (show $ NuDec SuL "10")
    it "u" $ do
      exec p_int "10u" `shouldBe` (show $ NuDec SuU "10")

  describe "p_str" $ do
    it "\"test\"" $ do
      exec p_str "\"test\"" `shouldBe` (show $ StrLiteral StNone StNon "test")

  describe "p_string prefix" $ do
    it "u8\"test\"" $ do
      exec p_str "u8\"test\"" `shouldBe` (show $ StrLiteral Stu8 StNon "test")
    it "L\"test\"" $ do
      exec p_str "L\"test\"" `shouldBe` (show $ StrLiteral StL StNon "test")
    it "u\"test\"" $ do
      exec p_str "u\"test\"" `shouldBe` (show $ StrLiteral Stu StNon "test")
    it "U\"test\"" $ do
      exec p_str "U\"test\"" `shouldBe` (show $ StrLiteral StU StNon "test")
    it "R\"test\"" $ do
      exec p_str "R\"test\"" `shouldBe` (show $ StrLiteral StR StNon "test")

    it "u8R\"test\"" $ do
      exec p_str "u8R\"test\"" `shouldBe` (show $ StrLiteral Stu8R StNon "test")
    it "LR\"test\"" $ do
      exec p_str "LR\"test\"" `shouldBe` (show $ StrLiteral StLR StNon "test")
    it "uR\"test\"" $ do
      exec p_str "uR\"test\"" `shouldBe` (show $ StrLiteral StuR StNon "test")
    it "UR\"test\"" $ do
      exec p_str "UR\"test\"" `shouldBe` (show $ StrLiteral StUR StNon "test")

  describe "p_string suffix" $ do
    it "\"test\"s" $ do
      exec p_str "\"test\"s" `shouldBe` (show $ StrLiteral StNone Sts "test")

  it "escaped str" $ do
    let expected str = show $ StrLiteral StNone StNon str
    exec p_str "\"test\\\\\"" `shouldBe` expected "test\\"
    exec p_str "\"test\\'\"" `shouldBe` expected "test'"
    exec p_str "\"test\\\"\"" `shouldBe` expected "test\""
    exec p_str "\"test\\a\"" `shouldBe` expected "test\a"
    exec p_str "\"test\\b\"" `shouldBe` expected "test\b"
    exec p_str "\"test\\f\"" `shouldBe` expected "test\f"
    exec p_str "\"test\\n\"" `shouldBe` expected "test\n"
    exec p_str "\"test\\r\"" `shouldBe` expected "test\r"
    exec p_str "\"test\\t\"" `shouldBe` expected "test\t"
    exec p_str "\"test\\0\"" `shouldBe` expected "test\0"
    exec p_str "\"test\\xff\"" `shouldBe` expected "test\\xff"
    exec p_str "\"test\\o77\"" `shouldBe` expected "test\\o77"

  describe "p_chr prefix" $ do
    it "u8't'" $ do
      exec p_chr "u8't'" `shouldBe` (show $ ChrLiteral Chu8 't')
    it "u't'" $ do
      exec p_chr "u't'" `shouldBe` (show $ ChrLiteral Chu 't')
    it "U't'" $ do
      exec p_chr "U't'" `shouldBe` (show $ ChrLiteral ChU 't')
    it "L't'" $ do
      exec p_chr "L't'" `shouldBe` (show $ ChrLiteral ChL 't')

  it "escaped chr" $ do
    exec p_chr "'\\\\'" `shouldBe` (show $ ChrLiteral ChNone '\\')
    exec p_chr "'\\\''" `shouldBe` (show $ ChrLiteral ChNone '\'')
    exec p_chr "'\\\"'" `shouldBe` (show $ ChrLiteral ChNone '"')
    exec p_chr "'\\a'" `shouldBe` (show $ ChrLiteral ChNone '\a')
    exec p_chr "'\\b'" `shouldBe` (show $ ChrLiteral ChNone '\b')
    exec p_chr "'\\f'" `shouldBe` (show $ ChrLiteral ChNone '\f')
    exec p_chr "'\\n'" `shouldBe` (show $ ChrLiteral ChNone '\n')
    exec p_chr "'\\r'" `shouldBe` (show $ ChrLiteral ChNone '\r')
    exec p_chr "'\\t'" `shouldBe` (show $ ChrLiteral ChNone '\t')
    exec p_chr "'\\0'" `shouldBe` (show $ ChrLiteral ChNone '\0')

  it "simple" $ do
    exec p_simple_double "5e10" `shouldBe` (show $ DblLiteral DbNone "5" "+10")
    exec p_simple_double "5e5f" `shouldBe` (show $ DblLiteral Dbf "5" "+5")
    exec p_simple_double "5e5F" `shouldBe` (show $ DblLiteral DbF "5" "+5")
    exec p_simple_double "5e5l" `shouldBe` (show $ DblLiteral Dbl "5" "+5")
    exec p_simple_double "5e5L" `shouldBe` (show $ DblLiteral DbL "5" "+5")
  
  it "front" $ do
    exec p_front_double "5.e-10" `shouldBe` (show $ DblLiteral DbNone "5" "-10")
    exec p_front_double "5." `shouldBe` (show $ DblLiteral DbNone "5" "+1")
    exec p_front_double "5.l" `shouldBe` (show $ DblLiteral Dbl "5" "+1")
    exec p_front_double "2.e+3F" `shouldBe` (show $ DblLiteral DbF "2" "+3")

  it "all double" $ do
    exec p_all_double "5.3e-13" `shouldBe` (show $ DblLiteral DbNone "5.3" "-13")
    exec p_all_double ".3e+2L" `shouldBe` (show $ DblLiteral DbL "0.3" "+2")
    exec p_all_double "0.158l" `shouldBe` (show $ DblLiteral Dbl "0.158" "+1")
    exec p_all_double "1.23E-13f" `shouldBe` (show $ DblLiteral Dbf "1.23" "-13")

  it "double" $ do
    exec p_dbl "5e10" `shouldBe` (show $ DblLiteral DbNone "5" "+10")
    exec p_dbl "5e5f" `shouldBe` (show $ DblLiteral Dbf "5" "+5")
    exec p_dbl "5e5F" `shouldBe` (show $ DblLiteral DbF "5" "+5")
    exec p_dbl "5e5l" `shouldBe` (show $ DblLiteral Dbl "5" "+5")
    exec p_dbl "5e5L" `shouldBe` (show $ DblLiteral DbL "5" "+5")
    exec p_dbl "5.e-10" `shouldBe` (show $ DblLiteral DbNone "5" "-10")
    exec p_dbl "5." `shouldBe` (show $ DblLiteral DbNone "5" "+1")
    exec p_dbl "5.l" `shouldBe` (show $ DblLiteral Dbl "5" "+1")
    exec p_dbl "2.e+3F" `shouldBe` (show $ DblLiteral DbF "2" "+3")
    exec p_dbl "5.3e-13" `shouldBe` (show $ DblLiteral DbNone "5.3" "-13")
    exec p_dbl ".3e+2L" `shouldBe` (show $ DblLiteral DbL "0.3" "+2")
    exec p_dbl "0.158l" `shouldBe` (show $ DblLiteral Dbl "0.158" "+1")
    exec p_dbl "1.23E-13f" `shouldBe` (show $ DblLiteral Dbf "1.23" "-13")

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
