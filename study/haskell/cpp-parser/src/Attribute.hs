{-# LANGUAGE DeriveGeneric #-}

module Attribute where

import Text.Parsec
import Text.Parsec.String

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Primitive.Identity
import Primitive.Integer

data AtList =
  AtUsingList String [Attribute] |
  AtList [Attribute]
  deriving (Show, Generic)

data Attribute =
  AtIdentity String |
  AtNamespace [String] String |
  AtFunction String AtArgument |
  AtNamespaceFunction [String] String AtArgument
  deriving (Show, Generic)

data AtArgument =
  AtArgument [AtPrimitive]
  deriving (Show, Generic)

data AtPrimitive =
  AtInteger Integer
  deriving (Show, Generic)

instance FromJSON AtList
instance FromJSON Attribute
instance FromJSON AtArgument
instance FromJSON AtPrimitive
instance ToJSON AtList
instance ToJSON Attribute
instance ToJSON AtArgument
instance ToJSON AtPrimitive

p_attributes_blocks :: Parser [AtList]
p_attributes_blocks = try $ do
  blocks <- many1 $ p_attributes_block <* spaces
  return blocks

p_attributes_block :: Parser AtList
p_attributes_block = 
  p_attribute_using_list <|>
  p_attribute_list

p_attribute_using_list :: Parser AtList
p_attribute_using_list = try $ do
  string "[["
  spaces
  string "using"
  skipMany1 space
  name <- p_identity
  spaces
  char ':'
  spaces
  attrs <- sepBy (try $ p_attribute <* spaces) (try $ char ',' >> spaces)
  string "]]"
  return $ AtUsingList name attrs

p_attribute_list :: Parser AtList
p_attribute_list = try $ do
  string "[["
  spaces
  attrs <- sepBy (try $ p_attribute <* spaces) (try $ char ',' >> spaces)
  string "]]"
  return $ AtList attrs

p_attribute :: Parser Attribute
p_attribute =
  p_attribute_namespace_function <|>
  p_attribute_namespace <|>
  p_attribute_function <|>
  p_attribute_identity

p_attribute_identity :: Parser Attribute
p_attribute_identity = try $ do
  name <- p_identity
  spaces
  return $ AtIdentity name

p_attribute_function :: Parser Attribute
p_attribute_function = try $ do
  name <- p_identity
  spaces
  char '('
  spaces
  args <- p_arguments
  char ')'
  spaces
  return $ AtFunction name args

p_attribute_namespace :: Parser Attribute
p_attribute_namespace = try $ do
  prefs <- many1 . try $ p_identity <* (spaces >> string "::" >> spaces)
  tail <- p_identity
  spaces
  return $ AtNamespace prefs tail

p_attribute_namespace_function :: Parser Attribute
p_attribute_namespace_function = try $ do
  prefs <- many1 . try $ p_identity <* (spaces >> string "::" >> spaces)
  tail <- p_identity
  spaces
  char '('
  spaces
  args <- p_arguments
  char ')'
  spaces
  return $ AtNamespaceFunction prefs tail args
  
p_attribute_primitive :: Parser AtPrimitive
p_attribute_primitive =
  (AtInteger <$> p_integer)

p_arguments :: Parser AtArgument
p_arguments = try $ do
  attrs <- sepBy (try $ p_attribute_primitive <* spaces) (try (char ',' >> spaces))
  return $ AtArgument attrs
