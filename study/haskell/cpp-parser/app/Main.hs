{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (responseLBS, Application, requestMethod, pathInfo, getRequestBodyChunk)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup)
import Data.Maybe
import Data.ByteString.Internal as DBI (unpackChars)
import Data.ByteString.Lazy as DBL (fromStrict)
import Data.ByteString.Lazy.Internal as DBLI (unpackChars, packChars)

import Text.Parsec
import Data.Aeson as DA (FromJSON, ToJSON, encode, decode)
import GHC.Generics

import Statement

import Util

data MyData = MyData { kind :: String, source :: String } deriving (Show, Generic)
instance DA.FromJSON MyData
instance DA.ToJSON MyData

main :: IO ()
main = do
  port <- getPort
  putStr "start Server: http://localhost:"
  print port
  run port helloApp

helloApp :: Application
helloApp req respond = do
  body <- getRequestBodyChunk req
  value <-
    if requestMethod req == "POST" && pathInfo req == ["parse"] then do
      let input = DBI.unpackChars body
      let dat = DBL.fromStrict body
          (Just mydat) = DA.decode dat :: Maybe MyData
          input = source mydat

      if input == "" then
        return " "
      else
        return $
          case parse (spaces *> p_statement <* eof) "" input of
            Left err -> show err
            Right val ->
              if kind mydat == "json" then
                DBLI.unpackChars . DA.encode $ val
              else
                show val
    else
      readFile "app/index.html"
  respond $ responseLBS status200 [] $ DBLI.packChars value

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000
