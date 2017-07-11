module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP)
import QuickServe (Capture(..), GET, JSON(..), POST, RequestBody(..), quickServe)

newtype Message = Message { message :: String }

derive instance genericMessage :: Generic Message _

jsonOpts :: Options
jsonOpts = defaultOptions { unwrapSingleConstructors = true }

instance decodeMessage :: Decode Message where
  decode = genericDecode jsonOpts

instance encodeMessage :: Encode Message where
  encode = genericEncode jsonOpts

main :: forall eff. Eff (console :: CONSOLE, http :: HTTP | eff) Unit
main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts $
    let
      echo1 :: RequestBody (JSON Message)
            -> POST (console :: CONSOLE | eff) (JSON Message)
      echo1 (RequestBody (JSON (Message { message }))) = do
        liftEff (log message)
        pure (JSON (Message { message }))

      echo2 :: Capture -> GET (console :: CONSOLE | eff) String
      echo2 (Capture message) = pure message

      hello :: GET (console :: CONSOLE | eff) String
      hello = pure "Hello, World!"
    in { echo1, echo2, hello }
