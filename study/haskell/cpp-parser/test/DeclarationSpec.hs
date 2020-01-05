module DeclarationSpec (spec) where

import Test.Hspec 

import Util
import Declaration
import Expression
import Statement

spec :: Spec
spec = do
  describe "type" $ do
    it "standard" $ do
      exec p_primitive_type "int" `shouldBe` (show $ "int")
      exec p_primitive_type "double" `shouldBe` (show $ "double")
      exec p_primitive_type "long long" `shouldBe` (show $ "long long")
      exec p_primitive_type "long doubles" `shouldBe` (show $ "long")

    it "ambicious" $ do
      exec p_primitive_type "ints" `shouldNotBe` (show $ "int")
      exec p_primitive_type "ints" `shouldNotBe` (show $ "ints")
      exec p_primitive_type "int_" `shouldNotBe` (show $ "int")

  describe "function" $ do
    it "standard" $ do
      exec p_primitive_function "int a (double b, long long c)" `shouldBe`
        (show $ (("int", "a"), [("double", "b"), ("long long", "c")]))

  describe "defined function" $ do
    it "standard" $ do
      exec p_function_definition "int a() { 1; }" `shouldBe`
        (show $ (("int", "a"), [] :: [String], StCompound [StExpression (ExInteger 1)]))
