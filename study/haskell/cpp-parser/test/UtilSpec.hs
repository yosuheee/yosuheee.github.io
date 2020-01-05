module UtilSpec (spec) where

import Test.Hspec
import Text.Parsec

import Util

import Expression

spec :: Spec
spec = do
  describe "library spec" $ do
    it "accept '\\n' and ' '" $ do
      exec space "\n" `shouldBe` "'\\n'"
      exec space " " `shouldBe` "' '"

    it "accept \"\"" $ do
      exec spaces "" `shouldBe` "()"

    it "option & optionMaybe" $ do
      let p = optionMaybe (try $ string "anaconda")
      exec p "ab" `shouldBe` "Nothing"

    it "<?>" $ do
      exec p_expression "" `shouldBe` (message 1 1 "expression")

  describe "p_spaces_not_crlf" $ do
    it "line feed" $ do
      exec p_spaces_not_crlf "   \n   " `shouldBe` (show $ "   ")
    it "line feed" $ do
      exec __ "   \n   " `shouldBe` (show $ "   ")
    it "all blank" $ do
      exec ___ "   \n   " `shouldBe` (show $ "   \n   ")
