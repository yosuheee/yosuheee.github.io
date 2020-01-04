module Primitive.DoubleSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Util

import Primitive.Double

spec :: Spec
spec = do
  it "p_double" $ do
    exec p_double "3e3" `shouldBe` (show $ 3e3)
    exec p_double "3e+3" `shouldBe` (show $ 3e3)
    exec p_double "3e-3" `shouldBe` (show $ 3e-3)
    exec p_double "3e3f" `shouldBe` (show $ 3e3)
    exec p_double "3e3F" `shouldBe` (show $ 3e3)
    exec p_double "3e3l" `shouldBe` (show $ 3e3)
    exec p_double "3e3L" `shouldBe` (show $ 3e3)
    exec p_double "5." `shouldBe` (show $ 5.0)
    exec p_double "5.l" `shouldBe` (show $ 5.0)
    exec p_double "2.e+3F" `shouldBe` (show $ 2e3)
    exec p_double "5.3e-13" `shouldBe` (show $ 5.3e-13)
    exec p_double "0.158l" `shouldBe` (show $ 0.158)
    exec p_double "1.23E-13f" `shouldBe` (show $ 1.23e-13)

  it "simple" $ do
    exec p_simple_double "5e10" `shouldBe` (show $ DblLiteral DSNone "5" "10")
    exec p_simple_double "5e5f" `shouldBe` (show $ DblLiteral DSf "5" "5")
    exec p_simple_double "5e5F" `shouldBe` (show $ DblLiteral DSF "5" "5")
    exec p_simple_double "5e5l" `shouldBe` (show $ DblLiteral DSl "5" "5")
    exec p_simple_double "5e5L" `shouldBe` (show $ DblLiteral DSL "5" "5")
  
  it "front" $ do
    exec p_front_double "5.e-10" `shouldBe` (show $ DblLiteral DSNone "5" "-10")
    exec p_front_double "5." `shouldBe` (show $ DblLiteral DSNone "5" "0")
    exec p_front_double "5.l" `shouldBe` (show $ DblLiteral DSl "5" "0")
    exec p_front_double "2.e+3F" `shouldBe` (show $ DblLiteral DSF "2" "3")

  it "all double" $ do
    exec p_all_double "5.3e-13" `shouldBe` (show $ DblLiteral DSNone "5.3" "-13")
    exec p_all_double ".3e+2L" `shouldBe` (show $ DblLiteral DSL "0.3" "2")
    exec p_all_double "0.158l" `shouldBe` (show $ DblLiteral DSl "0.158" "0")
    exec p_all_double "1.23E-13f" `shouldBe` (show $ DblLiteral DSf "1.23" "-13")

  it "double" $ do
    exec p_dbl_literal "5e10" `shouldBe` (show $ DblLiteral DSNone "5" "10")
    exec p_dbl_literal "5e5f" `shouldBe` (show $ DblLiteral DSf "5" "5")
    exec p_dbl_literal "5e5F" `shouldBe` (show $ DblLiteral DSF "5" "5")
    exec p_dbl_literal "5e5l" `shouldBe` (show $ DblLiteral DSl "5" "5")
    exec p_dbl_literal "5e5L" `shouldBe` (show $ DblLiteral DSL "5" "5")
    exec p_dbl_literal "5.e-10" `shouldBe` (show $ DblLiteral DSNone "5" "-10")
    exec p_dbl_literal "5." `shouldBe` (show $ DblLiteral DSNone "5" "0")
    exec p_dbl_literal "5.l" `shouldBe` (show $ DblLiteral DSl "5" "0")
    exec p_dbl_literal "2.e+3F" `shouldBe` (show $ DblLiteral DSF "2" "3")
    exec p_dbl_literal "5.3e-13" `shouldBe` (show $ DblLiteral DSNone "5.3" "-13")
    exec p_dbl_literal ".3e+2L" `shouldBe` (show $ DblLiteral DSL "0.3" "2")
    exec p_dbl_literal "0.158l" `shouldBe` (show $ DblLiteral DSl "0.158" "0")
    exec p_dbl_literal "1.23E-13f" `shouldBe` (show $ DblLiteral DSf "1.23" "-13")
