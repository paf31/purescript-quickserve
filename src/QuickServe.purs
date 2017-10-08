module QuickServe
  ( class Servable
  , serveWith
  , class IsResponse
  , encodeResponse
  , responseType
  , class IsRequest
  , decodeRequest
  , requestType
  , JSON(..)
  , Method(..)
  , GET
  , POST
  , PUT
  , RequestBody(..)
  , Capture(..)
  , quickServe
  , class ServableList
  , serveListWith
  ) where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, catchException, message)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Record (get)
import Data.String (split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, ListenOptions, Request, Response, createServer, listen, requestAsStream, requestMethod, requestURL, responseAsStream, setHeader, setStatusCode, setStatusMessage)
import Node.Stream (end, onDataString, onEnd, onError, writeString)
import Node.URL (parse)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

-- | A type class for types of values which define
-- | servers.
-- |
-- | Servers are built from the `Method` data type, which
-- | defines the method, record types which define routes
-- | and function types which make things like the request
-- | body and query parameters available.
class Servable eff server | server -> eff where
  serveWith
    :: server
    -> Request
    -> Response
    -> List String
    -> Maybe (Eff (http :: HTTP | eff) Unit)

-- | Start a web server given some `Servable` type
-- | and an implementation of that type.
-- |
-- | For example:
-- |
-- | ```purescript
-- | opts = { hostname: "localhost"
-- |        , port: 3000
-- |        , backlog: Nothing
-- |        }
-- |
-- | main = quickServe opts hello where
-- |   hello :: GET String
-- |   hello = pure "Hello, World!""
-- | ```
quickServe
  :: forall eff server
   . Servable (console :: CONSOLE | eff) server
  => ListenOptions
  -> server
  -> Eff (http :: HTTP, console :: CONSOLE | eff) Unit
quickServe opts serve = do
  server <- createServer \req res -> do
    let url = parse (requestURL req)
        path = maybe mempty toParts (toMaybe (url.path))
        toParts = dropEmpty <<< fromFoldable <<< split (wrap "/")
        dropEmpty ("" : xs) = dropEmpty xs
        dropEmpty xs = xs
    log (requestMethod req <> " " <> show (url.path))
    case serveWith serve req res path of
      Nothing -> badRoute res
      Just s -> s
  listen server opts (log ("Listening on port " <> show (_.port opts)))

-- | A type class for response data.
class IsResponse response where
  encodeResponse :: response -> String
  responseType :: Proxy response -> String

instance isResponseString :: IsResponse String where
  encodeResponse = id
  responseType _ = "text/plain"

-- | A type class for request data.
class IsRequest request where
  decodeRequest :: String -> Either String request
  requestType :: Proxy request -> String

instance isRequestString :: IsRequest String where
  decodeRequest = Right
  requestType _ = "text/plain"

-- | A request/response type which uses JSON as its
-- | data representation.
newtype JSON a = JSON a

derive instance newtypeJSON :: Newtype (JSON a) _

instance isResponseJSON :: Encode a => IsResponse (JSON a) where
  encodeResponse =
    encodeResponse
    <<< encodeJSON
    <<< unwrap
  responseType _ = "application/json"

instance isRequestJSON :: Decode a => IsRequest (JSON a) where
  decodeRequest =
    bimap (renderForeignError <<< extract) JSON
    <<< runExcept
    <<< decodeJSON
    <=< decodeRequest
  requestType _ = "application/json"

-- | A `Servable` type constructor which indicates the expected
-- | method (GET, POST, PUT, etc.) using a type-level string.
newtype Method (m :: Symbol) eff response = Method (Aff (http :: HTTP | eff) response)

derive instance newtypeMethod :: Newtype (Method m eff response) _

derive newtype instance functorMethod :: Functor (Method m eff)
derive newtype instance applyMethod :: Apply (Method m eff)
derive newtype instance applicativeMethod :: Applicative (Method m eff)
derive newtype instance bindMethod :: Bind (Method m eff)
derive newtype instance monadMethod :: Monad (Method m eff)
derive newtype instance monadEffMethod :: MonadEff (http :: HTTP | eff) (Method m eff)
derive newtype instance monadAffMethod :: MonadAff (http :: HTTP | eff) (Method m eff)

-- | A resource which responds to GET requests.
type GET = Method "GET"

-- | A resource which responds to POST requests.
type POST = Method "POST"

-- | A resource which responds to PUT requests.
type PUT = Method "PUT"

instance servableMethod
    :: (IsSymbol method, IsResponse response)
    => Servable eff (Method method eff response) where
  serveWith respond req res Nil = pure do
    let outputStream = responseAsStream res

        handleError = sendError res 500 "Internal server error" <<< message

        handleResponse r = do
          setHeader res "Content-Type" (responseType (Proxy :: Proxy response))
          _ <- writeString outputStream UTF8 (encodeResponse r) (pure unit)
          end outputStream (pure unit)
    let actual = requestMethod req
        expected = reflectSymbol (SProxy :: SProxy method)
    if actual == expected
      then void $ runAff (either handleError handleResponse) (unwrap respond)
      else sendError res 405 "Method not allowed" ("Expected " <> expected)
  serveWith _ _ _ _ = Nothing

-- | `RequestBody` can be used to read the request body.
-- |
-- | To read the request body, use a function type with a function
-- | argument type which has an `IsRequest` instance:
-- |
-- | ```purescript
-- | main = quickServe opts echo where
-- |   echo :: RequestBody String -> GET String
-- |   echo (RequestBody s) = pure s
-- | ```
newtype RequestBody a = RequestBody a

derive instance newtypeRequestBody :: Newtype (RequestBody a) _

instance servableRequestBody
    :: (IsRequest request, Servable eff service)
    => Servable eff (RequestBody request -> service) where
  serveWith read req res path = Just $ unsafeRunRef $ void do
    buffer <- newRef ""

    let inputStream = requestAsStream req
        outputStream = responseAsStream res

        handleData str = modifyRef buffer (_ <> str)

        handleError :: forall e. Error -> Eff (http :: HTTP | e) Unit
        handleError = sendError res 500 "Internal server error" <<< message

        handleEnd = do
          body <- readRef buffer
          case decodeRequest body of
            Left err ->
              sendError res 400 "Bad Request" err
            Right request ->
              case serveWith (read (RequestBody request)) req res path of
                Nothing -> badRoute res
                Just eff -> unsafeCoerceEff eff
    catchException handleError do
      onError inputStream handleError
      onDataString inputStream UTF8 handleData
      onEnd inputStream handleEnd

-- | `Capture` can be used to capture a part of the route.
-- |
-- | Use a function type with a function
-- | argument of type `Capture`:
-- |
-- | ```purescript
-- | main = quickServe opts echo' where
-- |   echo' :: Capture -> GET String
-- |   echo' (Capture s) = pure s
-- | ```
newtype Capture = Capture String

derive instance newtypeCapture :: Newtype Capture _

instance servableCapture
    :: Servable eff service
    => Servable eff (Capture -> service) where
  serveWith read req res (part : path) =
    serveWith (read (Capture part)) req res path
  serveWith _ _ _ _ = Nothing

sendError
  :: forall eff
   . Response
  -> Int
  -> String
  -> String
  -> Eff (http :: HTTP | eff) Unit
sendError res code msg body = do
  let outputStream = responseAsStream res
  setHeader res "Content-Type" "text/plain"
  setStatusCode res code
  setStatusMessage res msg
  _ <- writeString outputStream UTF8 body (pure unit)
  end outputStream (pure unit)

badRoute :: forall eff. Response -> Eff (http :: HTTP | eff) Unit
badRoute res = sendError res 400 "Bad Request" "No such route"

instance servableRecord :: (RowToList r l, ServableList eff l r) => Servable eff (Record r) where
  serveWith r = serveListWith (RLProxy :: RLProxy l) r

class ServableList eff (l :: RowList) (r :: # Type) | l -> r where
  serveListWith
    :: RLProxy l
    -> Record r
    -> Request
    -> Response
    -> List String
    -> Maybe (Eff (http :: HTTP | eff) Unit)

instance servableListNil :: ServableList eff Nil () where
  serveListWith _ _ _ _ _ = Nothing

instance servableListCons
  :: (IsSymbol route, Servable eff s, ServableList eff l r1, RowCons route s r1 r)
  => ServableList eff (Cons route s l) r where
  serveListWith _ rec req res (actual : xs)
    | actual == reflectSymbol (SProxy :: SProxy route)
    = serveWith (get (SProxy :: SProxy route) rec :: s) req res xs
  serveListWith _ rec req res xs = serveListWith (RLProxy :: RLProxy l) (unsafeCoerce rec) req res xs
