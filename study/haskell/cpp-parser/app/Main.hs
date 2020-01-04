{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (responseLBS, Application, requestMethod, pathInfo, getRequestBodyChunk)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup)
import Data.Maybe
import Data.ByteString.Lazy.Internal (packChars)
import Data.ByteString.Internal (unpackChars)

import Text.Parsec
import Statement

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
      let input = unpackChars body
      if input == "" then return " " else
        return $
          case parse p_statement "" input of
            Left  err -> show err
            Right val -> show val
    else
      readFile "app/index.html"
  respond $ responseLBS status200 [] $ packChars value

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000
