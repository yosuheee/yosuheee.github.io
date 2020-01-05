module AttributeSpec (spec) where

import Test.Hspec

import Attribute

spec :: Spec
spec = do
  it "a" $ do
    1 `shouldBe` 1
