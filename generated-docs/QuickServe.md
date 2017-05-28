## Module QuickServe

#### `Servable`

``` purescript
class Servable eff server | server -> eff where
  serveWith :: server -> Request -> Response -> List String -> Maybe (Eff (http :: HTTP | eff) Unit)
```

A type class for types of values which define
servers.

Servers are built from the `Method` data type, which
defines the method, record types which define routes
and function types which make things like the request
body and query parameters available.

##### Instances
``` purescript
(IsSymbol method, IsResponse response) => Servable eff (Method method eff response)
(IsRequest request, Servable eff service) => Servable eff (RequestBody request -> service)
(Servable eff service) => Servable eff (Capture -> service)
(Servable eff server) => Servable eff (Constructor name server)
(Servable eff server) => Servable eff (Rec server)
(Servable eff route1, Servable eff route2) => Servable eff (Product route1 route2)
(IsSymbol route, Servable eff server) => Servable eff (Field route server)
```

#### `quickServe`

``` purescript
quickServe :: forall eff server. Servable (console :: CONSOLE | eff) server => ListenOptions -> server -> Eff (http :: HTTP, console :: CONSOLE | eff) Unit
```

Start a web server given some `Servable` type
and an implementation of that type.

For example:

```purescript
opts = { hostname: "localhost"
       , port: 3000
       , backlog: Nothing
       }

main = quickServe opts hello where
  hello :: GET String
  hello = pure "Hello, World!""
```

#### `genericServeWith`

``` purescript
genericServeWith :: forall eff routes rep. Generic routes rep => Servable eff rep => routes -> Request -> Response -> List String -> Maybe (Eff (http :: HTTP | eff) Unit)
```

A default implementation of `serveWith` for record types
with derived `Generic` instances.

For example:

```purescript
newtype Routes eff = Routes
  { foo :: GET eff String
  , bar :: GET eff (JSON Response)
  }

derive instance genericRoutes :: Generic (Routes eff) _

instance servableRoutes :: Servable eff (Routes eff) where
  serveWith = genericServeWith
```

#### `IsResponse`

``` purescript
class IsResponse response  where
  encodeResponse :: response -> String
  responseType :: Proxy response -> String
```

A type class for response data.

##### Instances
``` purescript
IsResponse String
(Encode a) => IsResponse (JSON a)
```

#### `IsRequest`

``` purescript
class IsRequest request  where
  decodeRequest :: String -> Either String request
  requestType :: Proxy request -> String
```

A type class for request data.

##### Instances
``` purescript
IsRequest String
(Decode a) => IsRequest (JSON a)
```

#### `JSON`

``` purescript
newtype JSON a
  = JSON a
```

A request/response type which uses JSON as its
data representation.

##### Instances
``` purescript
Newtype (JSON a) _
(Encode a) => IsResponse (JSON a)
(Decode a) => IsRequest (JSON a)
```

#### `Method`

``` purescript
newtype Method (m :: Symbol) eff response
  = Method (Aff (http :: HTTP | eff) response)
```

A `Servable` type constructor which indicates the expected
method (GET, POST, PUT, etc.) using a type-level string.

##### Instances
``` purescript
Newtype (Method m eff response) _
Functor (Method m eff)
Apply (Method m eff)
Applicative (Method m eff)
Bind (Method m eff)
Monad (Method m eff)
MonadEff (http :: HTTP | eff) (Method m eff)
MonadAff (http :: HTTP | eff) (Method m eff)
(IsSymbol method, IsResponse response) => Servable eff (Method method eff response)
```

#### `GET`

``` purescript
type GET = Method "GET"
```

A resource which responds to GET requests.

#### `POST`

``` purescript
type POST = Method "POST"
```

A resource which responds to POST requests.

#### `PUT`

``` purescript
type PUT = Method "PUT"
```

A resource which responds to PUT requests.

#### `RequestBody`

``` purescript
newtype RequestBody a
  = RequestBody a
```

`RequestBody` can be used to read the request body.

To read the request body, use a function type with a function
argument type which has an `IsRequest` instance:

```purescript
main = quickServe opts echo where
  echo :: RequestBody String -> GET String
  echo (RequestBody s) = pure s
```

##### Instances
``` purescript
Newtype (RequestBody a) _
(IsRequest request, Servable eff service) => Servable eff (RequestBody request -> service)
```

#### `Capture`

``` purescript
newtype Capture
  = Capture String
```

`Capture` can be used to capture a part of the route.

Use a function type with a function
argument of type `Capture`:

```purescript
main = quickServe opts echo' where
  echo' :: Capture -> GET String
  echo' (Capture s) = pure s
```

##### Instances
``` purescript
Newtype Capture _
(Servable eff service) => Servable eff (Capture -> service)
```


