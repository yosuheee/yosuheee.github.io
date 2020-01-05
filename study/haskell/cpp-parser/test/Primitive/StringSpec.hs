module Primitive.StringSpec where

import Test.Hspec

import Util

import Primitive.String

spec :: Spec
spec = do
  it "p_string" $ do
    exec p_string "\"test\"" `shouldBe` (show $ "test")
    exec p_string "u8\"test\"" `shouldBe` (show $ "test")
    exec p_string "L\"test\"" `shouldBe` (show $ "test")
    exec p_string "u\"test\"" `shouldBe` (show $ "test")
    exec p_string "U\"test\"" `shouldBe` (show $ "test")
    exec p_string "R\"test\"" `shouldBe` (show $ "test")
    exec p_string "u8R\"test\"" `shouldBe` (show $ "test")
    exec p_string "LR\"test\"" `shouldBe` (show $ "test")
    exec p_string "uR\"test\"" `shouldBe` (show $ "test")
    exec p_string "UR\"test\"" `shouldBe` (show $ "test")
    exec p_string "\"test\\\\\"" `shouldBe` (show $ "test\\")
    exec p_string "\"test\\'\"" `shouldBe` (show $ "test'")
    exec p_string "\"test\\\"\"" `shouldBe` (show $ "test\"")
    exec p_string "\"test\\a\"" `shouldBe` (show $ "test\a")
    exec p_string "\"test\\b\"" `shouldBe` (show $ "test\b")
    exec p_string "\"test\\f\"" `shouldBe` (show $ "test\f")
    exec p_string "\"test\\n\"" `shouldBe` (show $ "test\n")
    exec p_string "\"test\\r\"" `shouldBe` (show $ "test\r")
    exec p_string "\"test\\t\"" `shouldBe` (show $ "test\t")
    exec p_string "\"test\\0\"" `shouldBe` (show $ "test\0")
    exec p_string "\"test\\xff\"" `shouldBe` (show $ "test\\xff")
    exec p_string "\"test\\o77\"" `shouldBe` (show $ "test\\o77")

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
