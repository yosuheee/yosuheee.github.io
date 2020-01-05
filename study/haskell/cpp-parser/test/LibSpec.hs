module LibSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String

import Util

import Primitive
import Primitive.Integer
import Expression
import Statement
import Lib

spec :: Spec
spec = do
  it "" $ 1 `shouldBe` 1
