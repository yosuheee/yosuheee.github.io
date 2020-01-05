module Primitive.BooleanSpec (spec) where

import Test.Hspec

import Util

import Primitive.Boolean

spec :: Spec
spec = do
  it "true and false" $ do
    exec p_boolean "true" `shouldBe` (show $ True)
    exec p_boolean "false" `shouldBe` (show $ False)
