{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module App.Application where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Req

import qualified App.Client as C

data MyData = MyData
  { biz :: Int
  , baz :: Int
  } deriving (Show, Generic)

instance ToJSON MyData
instance FromJSON MyData

get :: () -> IO (Either String String)
get () = do
  let request = C.Request 
        { C.requestMethod = C.GET
        , C.requestUrl = https "httpbin.org" /: "ip"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing }
  r <- C.run request 
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

getWithParams :: () -> IO (Either String String)
getWithParams () = do
  let params = [("foo", "15"), ("bar", "20")]
  let request = C.Request
        { C.requestMethod = C.GET
        , C.requestUrl = https "httpbin.org" /: "get"
        , C.requestBody = Nothing 
        , C.requestQueryParams = Just params }
  r <- C.run request
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

post :: () -> IO (Either String String)
post () = do
  let myData = MyData { biz = 15, baz = 20 }
  let payload = encode myData
  let request = C.Request
        { C.requestMethod = C.POST
        , C.requestUrl = https "httpbin.org" /: "post"
        , C.requestBody = Just payload 
        , C.requestQueryParams = Nothing }
  r <- C.run request
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

put :: () -> IO (Either String String)
put () = do
  let params = [("foo", "15"), ("bar", "20")]
  let request = C.Request
        { C.requestMethod = C.PUT
        , C.requestUrl = https "httpbin.org" /: "put"
        , C.requestBody = Nothing
        , C.requestQueryParams = Just params }
  r <- C.run request
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

delete :: () -> IO (Either String String)
delete () = do
  let request = C.Request
        { C.requestMethod = C.DELETE
        , C.requestUrl = https "httpbin.org" /: "delete"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing }
  r <- C.run request
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)
