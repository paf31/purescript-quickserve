# purescript-quickserve

Quickly create HTTP servers from functions!

[Module Documentation](generated-docs/QuickServe.md)

## Getting Started

A single endpoint which returns plain text:

```purescript
server :: GET String
server = pure "Hello, World!"

main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server
```

## Parsing Requests

Use a function argument with type `RequestBody a` to read the request body:

```purescript
server :: RequestBody String -> POST String
server (RequestBody s) = pure s
```

## JSON

Instead of `String`s, values which support the `Decode` and `Encode` classes
from `purescript-foreign-generic` can be used as JSON request and response types
respectively. See the [test project](test/Main.purs) for an example.

## Effects

The `GET`/`POST` monad has instances for `MonadEffect` and `MonadAff` to lift
synchronous and asynchronous effects.

## Routing

Routing tables are defined using records of functions, where the record's labels
are used to match routes. See the [test project](test/Main.purs) for an example.

Routing tables can be nested.
