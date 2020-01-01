module LibSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Lib

spec :: Spec
spec = do
  describe "p_sharp" $ do
    it "accept '#'" $
      exec p_sharp "#" `shouldBe` "'#'"

  describe "p_include_string" $ do
    it "accept \"include\"" $
      exec p_include_string "include" `shouldBe` "\"include\""

  describe "p_lt and p_gt" $ do
    it "accept '<'" $
      exec p_lt "<" `shouldBe` "'<'"
    it "accept '>'" $
      exec p_gt ">" `shouldBe` "'>'"

  describe "p_not_gt_or_blank" $ do
    it "reject '>'" $
      exec p_not_gt_or_blank ">" `shouldNotBe` "'>'"
    it "reject ' '" $
      exec p_not_gt_or_blank " " `shouldNotBe` "' '"
    it "accept 'a'" $
      exec p_not_gt_or_blank "a" `shouldBe` "'a'"

  describe "p_include" $ do
    it "accept \"# include < iostream >\"" $
      exec p_include "# include < iostream >" `shouldBe` "\"iostream\""

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
