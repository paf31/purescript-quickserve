module Test.Main where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)
import QuickServe (Capture(..), GET, JSON(..), POST, RequestBody(..), quickServe)

newtype Message = Message { message :: String }

derive instance genericMessage :: Generic Message _

jsonOpts :: Options
jsonOpts = defaultOptions { unwrapSingleConstructors = true }

instance decodeMessage :: Decode Message where
  decode = genericDecode jsonOpts

instance encodeMessage :: Encode Message where
  encode = genericEncode jsonOpts

-- | This will serve three endpoints:
-- |
-- | - `/hello`, which returns the plain text string "Hello World!"
-- | - `/echo1`, which receives a JSON message in a POST body
-- | - `/echo2/<arg>`, which receives a plain text message to echo as a path argument
-- |
-- | Each of these can be tested with cURL:
-- |
-- | ```
-- | curl http://localhost:3000/hello
-- | curl http://localhost:3000/echo1 -XPOST -d '{"message": "test"}'
-- | curl http://localhost:3000/echo2/test
-- | ```
main :: Effect Unit
main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts $
    let
      echo1 :: RequestBody (JSON Message)
            -> POST (JSON Message)
      echo1 (RequestBody (JSON (Message { message }))) = do
        liftEffect (log message)
        pure (JSON (Message { message }))

      echo2 :: Capture -> GET String
      echo2 (Capture message) = pure message

      hello :: GET String
      hello = pure "Hello, World!"
    in { echo1, echo2, hello }
