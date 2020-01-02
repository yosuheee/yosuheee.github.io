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
    it "accept \"# include < iostream > \\n\"" $
      exec p_include "# include < iostream > \n" `shouldBe` "\"iostream\""

  describe "space" $ do
    it "accept '\\n' and ' '" $ do
      exec space "\n" `shouldBe` "'\\n'"
      exec space " " `shouldBe` "' '"

  describe "spaces" $ do
    it "accept \"\"" $ do
      exec spaces "" `shouldBe` "()"

  describe "p_return_string" $ do
    it "accept \"return\"" $
      exec p_return_string "return" `shouldBe` "\"return\""

  describe "p_number" $ do
    it "accept \"12345\"" $
      exec p_number "12345" `shouldBe` "12345"

  describe "p_return" $ do
    it "accept \"return 12345;\"" $
      exec p_return "return 12345;" `shouldBe` "12345"

  describe "p_function" $ do
    let source = "int main() { return 123; }"
    it "accept source" $
      exec p_function source `shouldBe` "123"

  describe "p_namespace" $ do
    it "accept \"using namespace std;\"" $
      exec p_namespace "using namespace std;" `shouldBe` "\"std\""

  describe "p_main" $ do
    it "accept main.cpp file" $ do
      input <- readFile "./test/main.cpp"
      exec p_main input `shouldBe` "0"

  describe "p_identity" $ do
    it "accept start underscore" $ do
      let input = "_anaconda eggplant"
      exec p_identity input `shouldBe` "\"_anaconda\""
    it "accept only _" $ do
      let input = "_ eggplant"
      exec p_identity input `shouldBe` "\"_\""
    it "reject to start digit" $ do
      let input = "1 a"
      exec p_identity input `shouldNotBe` "\"1\""

  describe "p_identity_char" $ do
    it "accept '_' and 'a' and 'A', reject '1'" $ do
      exec p_identity_char "_" `shouldBe` "'_'"
      exec p_identity_char "a" `shouldBe` "'a'"
      exec p_identity_char "A" `shouldBe` "'A'"
      exec p_identity_char "1" `shouldNotBe` "'1'"

  describe "p_identity_and_digit_char" $ do
    it "accept '_' and 'a' and 'A' and '1'" $ do
      exec p_identity_and_digit_char "_" `shouldBe` "'_'"
      exec p_identity_and_digit_char "a" `shouldBe` "'a'"
      exec p_identity_and_digit_char "A" `shouldBe` "'A'"
      exec p_identity_and_digit_char "1" `shouldBe` "'1'"

  describe "p_var_defined" $ do
    it "accept \"int abc = 123;\"" $ do
      exec p_var_defined "int abc = 123;" `shouldBe` "(\"int\",\"abc\",ExInteger 123)"

  describe "p_expression" $ do
    it "accept \"123\"" $
      exec p_expression "123" `shouldBe` "ExInteger 123"
    it "accept \"1 + 2\"" $
      exec p_expression "1 + 2" `shouldBe` "ExTuple (ExInteger 1,'+',ExInteger 2)"
    it "accept \"anaconda\"" $
      exec p_expression "anaconda" `shouldBe` "ExIdentity \"anaconda\""
    it "accept \"1 + ana\"" $
      exec p_expression "1 + ana" `shouldBe` "ExTuple (ExInteger 1,'+',ExIdentity \"ana\")"
    it "accept \"egg + 5\"" $
      exec p_expression "egg + 5" `shouldBe` "ExTuple (ExIdentity \"egg\",'+',ExInteger 5)"
    it "accept \"1 - 2\"" $
      exec p_expression "1 - 2" `shouldBe` "ExTuple (ExInteger 1,'-',ExInteger 2)"
    it "accept \"1 * 2\"" $
      exec p_expression "1 * 2" `shouldBe` "ExTuple (ExInteger 1,'*',ExInteger 2)"
    it "accept \"1 / 2\"" $
      exec p_expression "1 / 2" `shouldBe` "ExTuple (ExInteger 1,'/',ExInteger 2)"
    it "accept \"1.5\"" $
      exec p_expression "1.5" `shouldBe` "ExDouble 1.5"

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val
