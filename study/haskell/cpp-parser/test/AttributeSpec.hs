module AttributeSpec (spec) where

import Test.Hspec

import Util
import Attribute

spec :: Spec
spec = do
  describe "one identifier attribute" $ do
    it "standard" $ do
      exec p_attribute_list "[[ noreturn ]]" `shouldBe`
        (show $ AtList [AtIdentity "noreturn"])

  describe "attribute list" $ do
    it "standard" $ do
      exec p_attribute_list "[[ a, b, c ]]" `shouldBe`
        (show $ AtList [AtIdentity "a", AtIdentity "b", AtIdentity "c"])

  describe "attribute namespace" $ do
    it "standard" $ do
      exec p_attribute_namespace "CC::opt" `shouldBe`
        (show $ AtNamespace ["CC"] "opt")
    it "combinate" $ do
      exec p_attributes_block "[[ CC::opt, a ]]" `shouldBe`
        (show $ AtList [AtNamespace ["CC"] "opt", AtIdentity "a"])

  describe "argument list" $ do
    it "standard" $ do
      exec p_arguments "1, 2, 3" `shouldBe`
        (show $ AtArgument [AtInteger 1, AtInteger 2, AtInteger 3])
    it "function" $ do
      exec p_attribute_function "func(1, 2, 3)" `shouldBe`
        (show $ AtFunction "func" $
          AtArgument [AtInteger 1, AtInteger 2, AtInteger 3])
    it "combinate" $ do
      exec p_attributes_block "[[ CC::opt, a, f(1, 2) ]]" `shouldBe`
        (show $ AtList [
          AtNamespace ["CC"] "opt",
          AtIdentity "a",
          AtFunction "f" $ AtArgument [AtInteger 1, AtInteger 2]])

  describe "namespace argument" $ do
    it "standard" $ do
      exec p_attribute_namespace_function "cats::meow(1)" `shouldBe`
        (show $ AtNamespaceFunction ["cats"] "meow" $ AtArgument [AtInteger 1])
    it "combinate" $ do
      exec p_attributes_block "[[ CC :: opt , a , f ( 1, 2 ) , cats :: meow ( 5 ) ]]" `shouldBe`
        (show $ AtList [
          AtNamespace ["CC"] "opt",
          AtIdentity "a",
          AtFunction "f" $ AtArgument [AtInteger 1, AtInteger 2],
          AtNamespaceFunction ["cats"] "meow" $ AtArgument [AtInteger 5] ])

  describe "using" $ do
    it "standard" $ do
      exec p_attribute_using_list "[[ using gnu : const , always ]]" `shouldBe`
        (show $ AtUsingList "gnu" [
          AtIdentity "const",
          AtIdentity "always" ])
    it "combinate" $ do
      exec p_attributes_block "[[ using s : CC :: opt , a , f ( 1, 2 ) , cats :: meow ( 5 ) ]]" `shouldBe`
        (show $ AtUsingList "s" [
          AtNamespace ["CC"] "opt",
          AtIdentity "a",
          AtFunction "f" $ AtArgument [AtInteger 1, AtInteger 2],
          AtNamespaceFunction ["cats"] "meow" $ AtArgument [AtInteger 5] ])
