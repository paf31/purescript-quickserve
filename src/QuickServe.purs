module QuickServe where

import Prelude
import Control.Alt ((<|>))
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
import Data.Either (Either(..))
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Generic.Rep (class Generic, Constructor(..), Field(..), Product(..), Rec(..), from)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.String (split)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, ListenOptions, Request, Response, createServer, listen, requestAsStream, requestMethod, requestURL, responseAsStream, setHeader, setStatusCode, setStatusMessage)
import Node.Stream (end, onDataString, onEnd, onError, writeString)
import Node.URL (parse)
import Type.Proxy (Proxy(..))

class IsResponse response where
  encodeResponse :: response -> String
  responseType :: Proxy response -> String

instance isResponseString :: IsResponse String where
  encodeResponse = id
  responseType _ = "text/plain"

class IsRequest request where
  decodeRequest :: String -> Either String request
  requestType :: Proxy request -> String

instance isRequestString :: IsRequest String where
  decodeRequest = Right
  requestType _ = "text/plain"

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

class Servable eff server | server -> eff where
  serveWith
    :: server
    -> Request
    -> Response
    -> List String
    -> Maybe (Eff (http :: HTTP | eff) Unit)

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
  listen server opts (pure unit)

newtype Method (m :: Symbol) eff response = Method (Aff (http :: HTTP | eff) response)

derive instance newtypeMethod :: Newtype (Method m eff response) _

derive newtype instance functorMethod :: Functor (Method m eff)
derive newtype instance applyMethod :: Apply (Method m eff)
derive newtype instance applicativeMethod :: Applicative (Method m eff)
derive newtype instance bindMethod :: Bind (Method m eff)
derive newtype instance monadMethod :: Monad (Method m eff)
derive newtype instance monadEffMethod :: MonadEff (http :: HTTP | eff) (Method m eff)
derive newtype instance monadAffMethod :: MonadAff (http :: HTTP | eff) (Method m eff)

type GET = Method "GET"
type POST = Method "POST"
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
      then void $ runAff handleError handleResponse (unwrap respond)
      else sendError res 405 "Method not allowed" ("Expected " <> expected)
  serveWith _ _ _ _ = Nothing

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

genericServeWith
  :: forall eff routes rep
   . Generic routes rep
  => Servable eff rep
  => routes
  -> Request
  -> Response
  -> List String
  -> Maybe (Eff (http :: HTTP | eff) Unit)
genericServeWith = serveWith <<< from

instance servableConstructor
  :: Servable eff server
  => Servable eff (Constructor name server) where
  serveWith (Constructor s) = serveWith s

instance servableRec
  :: Servable eff server
  => Servable eff (Rec server) where
  serveWith (Rec r) = serveWith r

instance servableProduct
  :: (Servable eff route1, Servable eff route2)
  => Servable eff (Product route1 route2) where
  serveWith (Product r1 r2) req res p =
    serveWith r1 req res p <|> serveWith r2 req res p

instance routableField
  :: (IsSymbol route, Servable eff server)
  => Servable eff (Field route server) where
  serveWith (Field s) req res (actual : xs)
    | actual == reflectSymbol (SProxy :: SProxy route)
    = serveWith s req res xs
  serveWith _ _ _ _ = Nothing
