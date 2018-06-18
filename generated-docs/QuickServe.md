## Module QuickServe

#### `Servable`

``` purescript
class Servable server  where
  serveWith :: server -> Request -> Response -> List String -> Maybe (Effect Unit)
```

A type class for types of values which define
servers.

Servers are built from the `Method` data type, which
defines the method, record types which define routes
and function types which make things like the request
body and query parameters available.

##### Instances
``` purescript
(IsSymbol method, IsResponse response) => Servable (Method method response)
(IsRequest request, Servable service) => Servable (RequestBody request -> service)
(Servable service) => Servable (Capture -> service)
(RowToList r l, ServableList l r) => Servable {  | r }
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
newtype Method (m :: Symbol) response
  = Method (Aff response)
```

A `Servable` type constructor which indicates the expected
method (GET, POST, PUT, etc.) using a type-level string.

##### Instances
``` purescript
Newtype (Method m response) _
Functor (Method m)
Apply (Method m)
Applicative (Method m)
Bind (Method m)
Monad (Method m)
MonadEffect (Method m)
MonadAff (Method m)
(IsSymbol method, IsResponse response) => Servable (Method method response)
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
(IsRequest request, Servable service) => Servable (RequestBody request -> service)
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
(Servable service) => Servable (Capture -> service)
```

#### `quickServe`

``` purescript
quickServe :: forall server. Servable server => ListenOptions -> server -> Effect Unit
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

#### `ServableList`

``` purescript
class ServableList (l :: RowList) (r :: # Type) | l -> r where
  serveListWith :: RLProxy l -> {  | r } -> Request -> Response -> List String -> Maybe (Effect Unit)
```

##### Instances
``` purescript
ServableList Nil ()
(IsSymbol route, Servable s, ServableList l r1, Cons route s r1 r) => ServableList (Cons route s l) r
```


