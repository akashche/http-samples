#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.21
  --ghc-options -Wall
  --package aeson
  --package bytestring
  --package http-client
  --package http-client-tls
  --package http-types
  --package text
  --package unordered-containers
  --package vector
  --package wai
  --package warp
-}

{-# LANGUAGE OverloadedStrings #-}

import Prelude (Either(..), Int, IO, Maybe(..), String, ($), (.), (<$>), error, return, show)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Data.Aeson (Value, (.=), encode, eitherDecode, object)
import Data.ByteString.Lazy (fromStrict)
import Data.HashMap.Strict (HashMap, fromList, lookup)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (putStrLn)
import Network.HTTP.Client (newManager, parseRequest, responseBody, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.HTTP.Types (Header, status200, status404, status500)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)

ctJson :: [Header]
ctJson = [("Content-Type", "application/json")]

a404Handler :: Application
a404Handler req respond = do
    respond $ responseLBS status404 ctJson $
        encode $ object
            [ "error" .= ("Not Found" :: Text)
            , "code" .= (404 :: Int)
            , "path" .= (decodeUtf8 . rawPathInfo) req
            ]

a500Handler :: SomeException -> Application
a500Handler exc req respond = do
    respond $ responseLBS status500 ctJson $
        encode $ object
            [ "error" .= show exc
            , "code" .= (500 :: Int)
            , "path" .= (decodeUtf8 . rawPathInfo) req
            ]

rootHandler :: Application
rootHandler _ respond = do
    respond $ responseLBS status200 ctJson $
        encode $ object
            [ "foo" .= (42 :: Int)
            ]

errorHandler :: Application
errorHandler _ _ = error "Fail!"

handlers :: HashMap Text Application
handlers = fromList
    [ ("/", rootHandler)
    , ("/error", errorHandler)
    ]

application :: Application
application req respond = do
    let path = (decodeUtf8 . rawPathInfo) req
    case lookup path handlers of
        Just ha -> do
            outcome <- try $ ha req respond
            case outcome of
                Left exc -> a500Handler exc req respond
                Right rr -> return rr
        Nothing -> a404Handler req respond

main :: IO ()
main = do
    let port = 8080 :: Int
    putStrLn $ "Starting server on port [" <> ((pack . show) port) <> "] ..."
    let settings = setPort port defaultSettings
    _ <- forkIO $ runSettings settings application

    putStrLn $ "Server started, performing requests ..."
    manager <- newManager tlsManagerSettings

    reqRoot <- parseRequest "http://127.0.0.1:8080/"
    withResponse reqRoot manager $ \response -> do
        bs <- fromStrict <$> (responseBody response)
        case eitherDecode bs :: Either String Value of
            Left msg -> error msg
            Right val -> putStrLn $ (pack . show) val

    req404 <- parseRequest "http://127.0.0.1:8080/fail"
    withResponse req404 manager $ \response -> do
        bs <- fromStrict <$> (responseBody response)
        case eitherDecode bs :: Either String Value of
            Left msg -> error msg
            Right val -> putStrLn $ (pack . show) val

    reqError <- parseRequest "http://127.0.0.1:8080/error"
    withResponse reqError manager $ \response -> do
        bs <- fromStrict <$> (responseBody response)
        case eitherDecode bs :: Either String Value of
            Left msg -> error msg
            Right val -> putStrLn $ (pack . show) val

    putStrLn $ "Shutting down ..."
