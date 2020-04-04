module App.Client
  ( Code
  , Url
  , Reason
  , Message
  , Status
  , EncodedJSON
  , QueryParameter
  , Error (..)
  , Method (..)
  , Request (..)
  , Response (..)
  , run
  ) where

{-

| This is a wrapper around the Haskell 'Network.HTTP.Req' library.

It provides utilities for making basic HTTP requests. To make a request,
the work flow is this:

1. Build an 'App.Client.Request' record, which contains 
   a request method (GET, POST, etc), a request URL, an optional 
   request body, and an optional set of query parameters.
   This record represents the request you want to make.

2. Call 'App.Client.run request', where 'request' is your request
   record. That function will actually make the request for you, and 
   return the response (or catch and return an error).

3. The response is an 'App.Client.Response' record, which contains the
   fields 'responseCode' (an 'Int') and 'responseBody' (a JSON 'Value').
   An error is one of the 'App.Client.Error' variants.

Note in particular:

* Safe URLs are built using the 'Req' library's 'http', 'https', 
  and '/:' functions.

* The body of requests must be encoded JSON, i.e., a byte string.
  The response is decoded into an 'Aeson' 'Value' for you though.
  So encoded JSON goes in, and decoded JSON comes out.

* Query parameters are specified as a list of key/value pairs,
  where both the key and the value are 'Text' (so convert your
  values into text).

-}

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Aeson as J
import qualified Network.HTTP.Req as R
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as S

{- | For convenience/clarity. -}
type Code = Int
type Url = String
type Reason = String
type Message = LB.ByteString
type Status = B.ByteString
type EncodedJSON = LB.ByteString
type QueryParameter = (T.Text, T.Text)

{- | The different types of errors that can arise during a request. -}
data Error
  = StatusCode Code Status
  | ResponseTimeout
  | ConnectionTimeout
  | ConnectionFailure Exc.SomeException
  | InternalException Exc.SomeException
  | ConnectionClosed
  | InvalidUrl Url Reason
  | JsonDecode Reason
  | Other String
  deriving (Show)

{- | The HTTP methods. -}
data Method
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Show)

{- | This reperesents the request you want to make. -}
data Request a b = Request
  { requestMethod :: Method
  , requestUrl :: R.Url a
  , requestBody :: Maybe EncodedJSON
  , requestQueryParams :: Maybe [QueryParameter]
  } deriving (Show)

{- | This represents a successful response. -}
data Response = Response
  { responseCode :: Int
  , responseBody :: J.Value
  } deriving (Show)

-- Used internally to retrieve the status code/message from
-- a 404/5XX error thrown by 'Req'.
getStatus :: C.Response () -> (Code, Status)
getStatus r =
  let s = C.responseStatus r
      code = S.statusCode s
      msg = S.statusMessage s
  in (code, msg)

-- Used internall to catch various errors that 'Req' might throw,
-- and convert them into simpler 'Error' variants.
handleError :: R.HttpException -> Error
handleError e =
  case e of
    R.VanillaHttpException httpExc -> 
      case httpExc of
        C.HttpExceptionRequest _ exc ->
          case exc of 
            C.StatusCodeException r _ ->
              let (code, msg) = getStatus r
              in StatusCode code msg
            C.ResponseTimeout -> ResponseTimeout
            C.ConnectionTimeout -> ConnectionTimeout
            C.ConnectionFailure e -> ConnectionFailure e
            C.InternalException e -> InternalException e
            C.ConnectionClosed -> ConnectionClosed
            _ -> Other (show exc)
        C.InvalidUrlException url reason -> InvalidUrl url reason
    R.JsonHttpException err -> JsonDecode err

-- This is used internally to actually perform an HTTP request. 
-- The argument is a 'Request' record.
doRequest request = R.runReq R.defaultHttpConfig $ do
  let url = requestUrl request
  let body = requestBody request
  let params = requestQueryParams request
  let query = case params of
        Nothing -> mempty
        Just q -> 
          let query = map (\(k, v) -> (R.=:) k v) q
          in mconcat query
  case requestMethod request of
    GET ->  R.req R.GET url R.NoReqBody R.jsonResponse query
    POST ->
      case requestBody request of
        Nothing -> R.req R.POST url R.NoReqBody R.jsonResponse query
        Just b -> R.req R.POST url (R.ReqBodyLbs b) R.jsonResponse query
    PUT ->
      case requestBody request of
        Nothing -> R.req R.PUT url R.NoReqBody R.jsonResponse query
        Just b -> R.req R.PUT url (R.ReqBodyLbs b) R.jsonResponse query
    DELETE -> R.req R.DELETE url R.NoReqBody R.jsonResponse query

{- | Run a request. -}
run :: Request a b -> IO (Either Error Response)
run request = do
  r <- Exc.try $ doRequest request
       :: IO (Either R.HttpException (R.JsonResponse J.Value))
  case r of
    Left e -> return $ Left (handleError e)
    Right r ->
      let code = R.responseStatusCode r
          body = R.responseBody r :: J.Value
          resp = Response { responseCode = code, responseBody = body }
      in return $ Right resp 
