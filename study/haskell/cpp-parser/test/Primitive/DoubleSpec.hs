module Primitive.DoubleSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Primitive.Double

spec :: Spec
spec = do
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
