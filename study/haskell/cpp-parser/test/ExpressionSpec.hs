module ExpressionSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Expression

spec :: Spec
spec = do
  describe "p_number" $ do
    it "accept \"12345\"" $
      exec p_number "12345" `shouldBe` "12345"

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

  describe "p_priority_5" $ do
    it "accept \"1*2\"" $ do
      exec p_priority_5 "1*2" `shouldBe`
        (show $ ExList (ExInteger 1, [("*", ExInteger 2)]))
    it "accept \"1 * 2\"" $ do
      exec p_priority_5 "1 * 2" `shouldBe`
        (show $ ExList (ExInteger 1, [("*", ExInteger 2)]))
    it "accept \"1 * 2 * 3\"" $ do
      exec p_priority_5 "1 * 2 * 3" `shouldBe`
        (show $ ExList (ExInteger 1, [("*", ExInteger 2), ("*", ExInteger 3)]))
    it "accept \"1 / 2\"" $ do
      exec p_priority_5 "1 / 2" `shouldBe`
        (show $ ExList (ExInteger 1, [("/", ExInteger 2)]))
    it "accept \"1 % 2\"" $ do
      exec p_priority_5 "1 % 2" `shouldBe`
        (show $ ExList (ExInteger 1, [("%", ExInteger 2)]))

  describe "p_priority_6" $ do
    it "accept \"1 + 2\"" $ do
      exec p_priority_6 "1 + 2" `shouldBe`
        (show $ ExList (ExInteger 1, [("+", ExInteger 2)]))
    it "accept \"1 + 2 + 3\"" $ do
      exec p_priority_6 "1 + 2 + 3" `shouldBe`
        (show $ ExList (ExInteger 1, [("+", ExInteger 2), ("+", ExInteger 3)]))
    it "accept \"1 + 2 * 3\"" $ do
      exec p_priority_6 "1 + 2 * 3" `shouldBe`
        (show $ ExList (ExInteger 1, [("+", ExList (ExInteger 2, [("*", ExInteger 3)]))]))
    it "accept \"1 * 2 + 3\"" $ do
      exec p_priority_6 "1 * 2 + 3" `shouldBe`
        (show $ ExList (ExList (ExInteger 1, [("*", ExInteger 2)]), [("+", ExInteger 3)]))

  describe "p_priority_7" $ do
    it "accept \"1 << 2\"" $ spec_binop p_priority_7 "<<"
    it "accept \"1 >> 2\"" $ spec_binop p_priority_7 ">>"

  describe "p_priority_8" $ do
    it "accept \"1 <=> 2\"" $ spec_binop p_priority_8 "<=>"

  describe "p_priority_9" $ do
    it "accept \"1 <= 2\"" $ spec_binop p_priority_9 "<="
    it "accept \"1 >= 2\"" $ spec_binop p_priority_9 ">="
    it "accept \"1 < 2\""  $ spec_binop p_priority_9 "<"
    it "accept \"1 > 2\""  $ spec_binop p_priority_9 ">"

  describe "p_priority_10" $ do
    it "accept \"1 == 2\"" $ spec_binop p_priority_10 "=="
    it "accept \"1 != 2\"" $ spec_binop p_priority_10 "!="

  describe "p_priority_11" $ do
    it "accept \"1 & 2\""  $ spec_binop p_priority_11 "&"

  describe "p_priority_12" $ do
    it "accept \"1 ^ 2\""  $ spec_binop p_priority_12 "^"

  describe "p_priority_13" $ do
    it "accept \"1 | 2\""  $ spec_binop p_priority_13 "|"

  describe "p_priority_14" $ do
    it "accept \"1 && 2\"" $ spec_binop p_priority_14 "&&"

  describe "p_priority_15" $ do
    it "accept \"1 || 2\"" $ spec_binop p_priority_15 "||"

  describe "p_priority_16" $ do
    it "accept \"1 = 2\""  $ spec_binop p_priority_16 "="
    it "accept \"1 += 2\"" $ spec_binop p_priority_16 "+="
    it "accept \"1 -= 2\"" $ spec_binop p_priority_16 "-="
    it "accept \"1 *= 2\"" $ spec_binop p_priority_16 "*="
    it "accept \"1 /= 2\"" $ spec_binop p_priority_16 "/="
    it "accept \"1 %= 2\"" $ spec_binop p_priority_16 "%="
    it "accept \"1 <<= 2\"" $ spec_binop p_priority_16 "<<="
    it "accept \"1 >>= 2\"" $ spec_binop p_priority_16 ">>="
    it "accept \"1 &= 2\"" $ spec_binop p_priority_16 "&="
    it "accept \"1 ^= 2\"" $ spec_binop p_priority_16 "^="
    it "accept \"1 |= 2\"" $ spec_binop p_priority_16 "|="
    it "accept \"1 ? 2 : 3\"" $ do
      exec p_priority_16 "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1, ExInteger 2, ExInteger 3))
    it "accept \"throw 1\"" $ do
      exec p_priority_16 "throw 1" `shouldBe`
        (show $ ExThrow (ExInteger 1))

  describe "p_ternary" $ do
    it "accept \"1 ? 2 : 3\"" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1, ExInteger 2, ExInteger 3))

  describe "p_throw" $ do
    it "accept \"throw 1\"" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExThrow (ExInteger 1))

exec :: Show a => Parser a -> String -> String
exec p input =
  case parse p "" input of
    Left  err -> show err
    Right val -> show val

spec_binop :: Parser Expression -> String -> Expectation
spec_binop pa op = 
  exec pa ("1" ++ op ++ "2") `shouldBe`
    (show $ ExList (ExInteger 1, [(op, ExInteger 2)]))
