module ExpressionSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
import Text.Parsec (eof)

import Util

import Expression

spec :: Spec
spec = do
  describe "p_expression" $ do
    it "accept '1 * (2 + 3)'" $
      exec p_expression "1 * (2 + 3)" `shouldBe` (show $
        (ExBinary "*" (ExInteger 1) (ExBinary "+" (ExInteger 2) (ExInteger 3))))
    it "accept '123'" $
      exec p_expression "123" `shouldBe` (show $ ExInteger 123)
    it "accept 'ana'" $
      exec p_expression "ana" `shouldBe` (show $ ExIdentity "ana")
    it "accept '1.5'" $
      exec p_expression "1.5" `shouldBe` (show $ ExDouble 1.5)
    it "accept '1 ? 2 : 3'" $ do
      exec p_expression "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1) (ExInteger 2) (ExInteger 3))
    it "accept 'throw 123'" $ do
      exec p_expression "throw 123" `shouldBe`
        (show $ ExPrefix "throw" (ExInteger 123))
    it "accept '1 + ++a + a++'" $ do
      exec p_expression "1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExInteger 1) (ExPrefix "++" (ExIdentity "a"))) (ExSuffix "++" (ExIdentity "a")))
    it "accept '+1 + ++a + a++'" $ do
      exec p_expression "+1 + ++a + a++" `shouldBe`
        (show $ ExBinary "+" (ExBinary "+" (ExPrefix "+" (ExInteger 1)) (ExPrefix "++" (ExIdentity "a"))) (ExSuffix "++" (ExIdentity "a")))
    it "accept '1 && 2'" $ do
      exec p_expression "1 && 2" `shouldBe`
        (show $ ExBinary "&&" (ExInteger 1) (ExInteger 2))
    it "accept '!~1'" $ do
      exec p_expression "!~1" `shouldBe`
        (show $ ExPrefix "!" (ExPrefix "~" (ExInteger 1)))
    it "accept '++a++'" $ do
      exec p_expression "++a--" `shouldBe`
        (show $ ExPrefix "++" (ExSuffix "--" (ExIdentity "a")))
    it "accept 'a++--'" $ do
      exec p_expression "a++--" `shouldBe`
        (show $ ExSuffix "--" (ExSuffix "++" (ExIdentity "a")))
    it "accept 'true' or 'false" $ do
      exec p_expression "true" `shouldBe` (show $ ExBoolean True)
      exec p_expression "false" `shouldBe` (show $ ExBoolean False)
    it "p_boolean denied 'trues', and p_identity accept it" $
      exec (p_expression <* eof) "trues" `shouldBe` (show $ ExIdentity "trues")

  describe "p_ternary" $ do
    it "accept '1 ? 2 : 3'" $ do
      exec p_ternary "1 ? 2 : 3" `shouldBe`
        (show $ ExTernary (ExInteger 1) (ExInteger 2) (ExInteger 3))

  describe "p_throw" $ do
    it "accept 'throw 1'" $ do
      exec p_throw "throw 1" `shouldBe`
        (show $ ExPrefix "throw" (ExInteger 1))

  it "p_priority_5_15" $ do
    exec p_priority_5_15 "1 + 2" `shouldBe` (show $
      ExBinary "+" (ExInteger 1) (ExInteger 2))
    exec p_priority_5_15 "1 + 2 - 3" `shouldBe` (show $
      ExBinary "-" (ExBinary "+" (ExInteger 1) (ExInteger 2)) (ExInteger 3))
    exec p_priority_5_15 "1 + 2 * 3" `shouldBe` (show $
      ExBinary "+" (ExInteger 1) (ExBinary "*" (ExInteger 2) (ExInteger 3)))
    exec p_priority_5_15 "1 * 2 << 3 % 4" `shouldBe` (show $
      ExBinary "<<" (ExBinary "*" (ExInteger 1) (ExInteger 2)) (ExBinary "%" (ExInteger 3) (ExInteger 4)))
    exec p_priority_5_15 "1 - 2 / 3 + 4 == 5" `shouldBe` (show $
      ExBinary "==" (ExBinary "+" (ExBinary "-" (ExInteger 1) (ExBinary "/" (ExInteger 2) (ExInteger 3))) (ExInteger 4)) (ExInteger 5))

  it "p_priority_2" $ do
    exec p_priority_2 "a++" `shouldBe` (
      show $ ExSuffix "++" (ExIdentity "a"))
    exec p_priority_2 "a--" `shouldBe` (
      show $ ExSuffix "--" (ExIdentity "a"))

  it "p_priority_3" $ do
    exec p_priority_3 "++a" `shouldBe` (show $ ExPrefix "++" (ExIdentity "a"))
    exec p_priority_3 "--a" `shouldBe` (show $ ExPrefix "--" (ExIdentity "a"))
    exec p_priority_3 "+a"  `shouldBe` (show $ ExPrefix "+"  (ExIdentity "a"))
    exec p_priority_3 "-a"  `shouldBe` (show $ ExPrefix "-"  (ExIdentity "a"))
    exec p_priority_3 "!1"  `shouldBe` (show $ ExPrefix "!"  (ExInteger 1))
    exec p_priority_3 "~1"  `shouldBe` (show $ ExPrefix "~"  (ExInteger 1))
    exec p_priority_3 "*a"  `shouldBe` (show $ ExPrefix "*"  (ExIdentity "a"))
    exec p_priority_3 "&a"  `shouldBe` (show $ ExPrefix "&"  (ExIdentity "a"))

  describe "binary operator" $ do
    flip forM_ it_spec_binop $ concat [
      ["*", "/", "%"],
      ["+", "-"],
      ["<<", ">>"],
      ["<=>"],
      ["<", ">", "<=", ">="],
      ["==", "!="],
      ["&"], ["^"], ["|"], ["&&"], ["||"],
      ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]]

  describe "function" $ do
    it "standard" $ do
      exec p_priority_2 "func(1, a)" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExInteger 1, ExIdentity "a"])
  
  describe "priority 1" $ do
    it "::" $ do
      exec p_priority_1 "a::b::c" `shouldBe`
        (show $ ExBinary "::" (ExBinary "::" (ExIdentity "a") (ExIdentity "b")) (ExIdentity "c"))
      exec p_expression "a::b::c" `shouldBe`
        (show $ ExBinary "::" (ExBinary "::" (ExIdentity "a") (ExIdentity "b")) (ExIdentity "c"))

  describe "chain" $ do
    it "dot" $ do
      exec p_priority_2 "a.b.c" `shouldBe`
        (show $ ExBinary "." (ExBinary "." (ExIdentity "a") (ExIdentity "b")) (ExIdentity "c"))
    it "address" $ do
      exec p_priority_2 "a->b->c" `shouldBe`
        (show $ ExBinary "->" (ExBinary "->" (ExIdentity "a") (ExIdentity "b")) (ExIdentity "c"))
    it "function" $ do
      exec p_priority_2 "a(1)(2)" `shouldBe`
        (show $ ExFunction (ExFunction (ExIdentity "a") [(ExInteger 1)]) [(ExInteger 2)])
    it "array" $ do
      exec p_priority_2 "a[1][2]" `shouldBe`
        (show $ ExBinary "[]" (ExBinary "[]" (ExIdentity "a") (ExInteger 1)) (ExInteger 2))
    it "combinate" $ do
      exec p_priority_2 "a[1].b->c++--" `shouldBe`
        (show $
          ExSuffix "--"
            (ExSuffix "++"
              (ExBinary "->"
                (ExBinary "."
                  (ExBinary "[]" (ExIdentity "a") (ExInteger 1))
                  (ExIdentity "b"))
                (ExIdentity "c"))))
    it "top level parser" $ do
      exec p_expression "a.b.c" `shouldBe`
        (show $ ExBinary "." (ExBinary "." (ExIdentity "a") (ExIdentity "b")) (ExIdentity "c"))
      exec p_expression "a->b->c" `shouldBe`
        (show $ ExBinary "->" (ExBinary "->" (ExIdentity "a") (ExIdentity "b")) (ExIdentity "c"))
      exec p_expression "a(1)(2)" `shouldBe`
        (show $ ExFunction (ExFunction (ExIdentity "a") [(ExInteger 1)]) [(ExInteger 2)])
      exec p_expression "a[1][2]" `shouldBe`
        (show $ ExBinary "[]" (ExBinary "[]" (ExIdentity "a") (ExInteger 1)) (ExInteger 2))
      exec p_expression "a[1].b->c++--" `shouldBe`
        (show $
          ExSuffix "--"
            (ExSuffix "++"
              (ExBinary "->"
                (ExBinary "."
                  (ExBinary "[]" (ExIdentity "a") (ExInteger 1))
                  (ExIdentity "b"))
                (ExIdentity "c"))))

  describe "priority 4" $ do
    it ".*" $ do
      exec p_priority_4 "a.*b" `shouldBe`
        (show $ ExBinary ".*" (ExIdentity "a") (ExIdentity "b"))
      exec p_expression "a.*b" `shouldBe`
        (show $ ExBinary ".*" (ExIdentity "a") (ExIdentity "b"))
    it "->*" $ do
      exec p_priority_4 "a->*b" `shouldBe`
        (show $ ExBinary "->*" (ExIdentity "a") (ExIdentity "b"))
      exec p_expression "a->*b" `shouldBe`
        (show $ ExBinary "->*" (ExIdentity "a") (ExIdentity "b"))

  describe "bug blank" $ do
    it "function" $ do
      exec p_priority_2 "func(1 )" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExInteger 1])
      exec p_priority_2 "func(1 ,2)" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExInteger 1, ExInteger 2])
      exec p_priority_2 "func(a ,b)" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExIdentity "a", ExIdentity "b"])
      exec p_priority_2 "func(\"a\" ,\"b\")" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExString "a", ExString "b"])
      exec p_priority_2 "func('a' ,'b')" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExChar 'a', ExChar 'b'])
      exec p_priority_2 "func(1.2 ,1.3)" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExDouble 1.2, ExDouble 1.3])
      exec p_priority_2 "func(true ,false)" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExBoolean True, ExBoolean False])
      exec p_priority_2 "func(f() ,g())" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExFunction (ExIdentity "f") [], ExFunction (ExIdentity "g") []])
      exec p_priority_2 "func((1) ,(2))" `shouldBe`
        (show $ ExFunction (ExIdentity "func") [ExInteger 1, ExInteger 2])

  describe "error" $
    it "unexpected" $ do
      exec p_expression "" `shouldBe` (message 1 1 "expression")

it_spec_binop :: String -> SpecWith (Arg Expectation)
it_spec_binop op =
  it ("accept '1 " ++ op ++ " 2'") $
    exec p_expression ("1" ++ op ++ "2") `shouldBe` expected
  where
    expected = show $ ExBinary op (ExInteger 1) (ExInteger 2)
