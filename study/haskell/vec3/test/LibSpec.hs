module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  let
    v = Vec3 1 2 3
    u = Vec3 4 5 6
  describe "Vec3" $ do
    it "add()" $ do
      (v + u) `shouldBe` (Vec3 5 7 9)
      (v + v) `shouldBe` (Vec3 2 4 6)
    it "sub()" $ do
      (u - v) `shouldBe` (Vec3 3 3 3)
      (v - u) `shouldBe` (Vec3 (-3) (-3) (-3))
