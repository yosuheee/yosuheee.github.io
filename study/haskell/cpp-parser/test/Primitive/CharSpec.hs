module Primitive.CharSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Primitive.Char

spec :: Spec
spec = do
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

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
