module App.Client
  ( Code
  , Url
  , Reason
  , Message
  , Status
  , Error (..)
  , Method (..)
  , Request (..)
  , Response (..)
  , run
  ) where

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Aeson as J
import qualified Network.HTTP.Req as R
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as S

type Code = Int
type Url = String
type Reason = String
type Message = LB.ByteString
type Status = B.ByteString

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

data Method
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Show)

data Request a b = Request
  { requestMethod :: Method
  , requestUrl :: R.Url a
  , requestBody :: Maybe Message
  , requestQueryParams :: Maybe [(T.Text, T.Text)]
  } deriving (Show)

data Response = Response
  { responseCode :: Int
  , responseBody :: J.Value
  } deriving (Show)

getStatus :: C.Response () -> (Code, Status)
getStatus r =
  let s = C.responseStatus r
      code = S.statusCode s
      msg = S.statusMessage s
  in (code, msg)

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
