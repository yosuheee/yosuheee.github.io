module UtilSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Util

import Expression

spec :: Spec
spec =
  describe "library spec" $ do
    it "option & optionMaybe" $ do
      let p = optionMaybe (try $ string "anaconda")
      exec p "ab" `shouldBe` "Nothing"

    it "<?>" $ do
      exec p_expression "" `shouldBe` (message 1 1 "expression")
