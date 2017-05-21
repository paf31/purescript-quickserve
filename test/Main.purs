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
import QuickServe (class Servable, GET, JSON(..), POST, RequestBody(..), genericServeWith, quickServe)

newtype Message = Message { message :: String }

derive instance genericMessage :: Generic Message _

jsonOpts :: Options
jsonOpts = defaultOptions { unwrapSingleConstructors = true }

instance decodeMessage :: Decode Message where
  decode = genericDecode jsonOpts

instance encodeMessage :: Encode Message where
  encode = genericEncode jsonOpts

newtype Routes eff = Routes
  { echo :: RequestBody (JSON Message) -> POST (console :: CONSOLE | eff) (JSON Message)
  , hello :: GET (console :: CONSOLE | eff) String
  }

derive instance genericRoutes :: Generic (Routes eff) _

instance servableRoutes :: Servable (console :: CONSOLE | eff) (Routes eff) where
  serveWith = genericServeWith

server :: forall eff. Routes eff
server = Routes
  { echo: \(RequestBody (JSON (Message { message }))) -> do
      liftEff (log message)
      pure (JSON (Message { message }))
  , hello: pure "Hello, World!"
  }

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server
