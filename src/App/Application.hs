{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module App.Application where

{-

| This is the main application.

It illustrates common ways to make HTTP requests.
The 'App.Client' library is the real work horse. It does the actual
requesting, and parsing of responses (or errors). The functions  
below illustrate how to use that library.

The functions below show how to make various GET, POST, PUT, 
and DELETE requests. The basic flow is:

1. Construct a request record ('App.Client.Request')

2. Make a request (call 'App.Client.Run').

3. The result is either an error ('App.Client.Error'), or a
   successful response ('App.Client.Response').

You will notice the following

* You build URLs using the 'https' or 'http' and '/:' functions that
  come from the 'Network.HTTP.Req' library.

* If you send a body in your request, you encode it as JSON before
  sending it (using the 'encode' function that comes from 'Data.Aeson').
  The body of the response, by contrast, is already decoded, and so
  is an Aeson 'Value'.

* If you send query parameters, you specify them as a list of key/value
  pairs, where both the key and the value are 'Text'. Make sure to use
  the 'OverloadedStrings' pragma when working with 'Aeson' and 'Req'. 
  
-}

import GHC.Generics (Generic)
import Data.Aeson
import Network.HTTP.Req

import qualified App.Client as C

{- | This is some sample data that can be sent in the body of a request.

It is an instance of the 'Generic', 'ToJSON', and 'FromJSON' type classes,
so that it can be automatically converted into JSON. -} 
data MyData = MyData
  { biz :: Int
  , baz :: Int
  } deriving (Show, Generic)

instance ToJSON MyData
instance FromJSON MyData

{- | An example of making a GET request. -}
get :: () -> IO (Either String String)
get () = do

  -- First, build the request.
  let request = C.Request 
        { C.requestMethod = C.GET -- The method
        , C.requestUrl = https "httpbin.org" /: "ip" -- "Safe" urls.
        , C.requestBody = Nothing -- No body
        , C.requestQueryParams = Nothing } -- No query parameters.

  -- Now run the request.
  r <- C.run request 

  -- The result is either an error, or a response.
  -- In this case, we just stringify both and return them,
  -- but you can inspect them further if you need.
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

{- | An example of making a GET request, with query parameters. -}
getWithParams :: () -> IO (Either String String)
getWithParams () = do

  -- Suppose we want to send the query parameters '?foo=15&bar=20'.
  -- Specify this as a list of key/value pairs, where both
  -- the key and the value are 'Text'.
  let params = [("foo", "15"), ("bar", "20")]

  -- Build the request.
  let request = C.Request
        { C.requestMethod = C.GET
        , C.requestUrl = https "httpbin.org" /: "get"
        , C.requestBody = Nothing 
        , C.requestQueryParams = Just params } -- Add the query parameters.

  -- Now run the request.
  r <- C.run request

  -- Handle it as you need.
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

{- | An example of making a POST request. -}
post :: () -> IO (Either String String)
post () = do

  -- Suppose we want to send '{"biz": 15, "baz": 20}' in the request body.
  -- We first make our Haskell record, then we encode it as JSON (which
  -- is a bytestring), using the 'encode' function from 'Data.Aeson'.
  -- Recall that the 'MyData' type is an instance of 'ToJSON', and so
  -- can by encoded into JSON.
  let myData = MyData { biz = 15, baz = 20 }
  let payload = encode myData

  -- Build the request.
  let request = C.Request
        { C.requestMethod = C.POST
        , C.requestUrl = https "httpbin.org" /: "post"
        , C.requestBody = Just payload -- Add the payload.
        , C.requestQueryParams = Nothing }

  -- Now run the request.
  r <- C.run request

  -- Handle the response as you need.
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

{- | An example of making a PUT' request. -}
put :: () -> IO (Either String String)
put () = do

  -- We could send a body, or query parameters, or both.
  -- Here we'll just use query parameters.
  let params = [("foo", "15"), ("bar", "20")]

  -- Build the request.
  let request = C.Request
        { C.requestMethod = C.PUT
        , C.requestUrl = https "httpbin.org" /: "put"
        , C.requestBody = Nothing
        , C.requestQueryParams = Just params }

  -- Send the request.
  r <- C.run request

  -- Handle the response as you need.
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)

{- | An example of making a 'DELETE' request. -}
delete :: () -> IO (Either String String)
delete () = do

  -- Build the request.
  let request = C.Request
        { C.requestMethod = C.DELETE
        , C.requestUrl = https "httpbin.org" /: "delete"
        , C.requestBody = Nothing
        , C.requestQueryParams = Nothing }

  -- Make the request.
  r <- C.run request

  -- Handle the response as you need.
  case r of
    Left e -> return $ Left (show e)
    Right r -> return $ Right (show r)
