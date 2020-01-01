module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      1 `shouldBe` 1
