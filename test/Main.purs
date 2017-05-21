module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Node.HTTP (HTTP)
import QuickServe (class Servable, GET, POST, RequestBody(RequestBody), genericServeWith, quickServe)

newtype Routes eff = Routes
  { echo :: RequestBody String -> POST (console :: CONSOLE | eff) String
  , hello :: GET (console :: CONSOLE | eff) String
  }

derive instance genericRoutes :: Generic (Routes eff) _

instance servableRoutes :: Servable (console :: CONSOLE | eff) (Routes eff) where
  serveWith = genericServeWith

server :: forall eff. Routes eff
server = Routes
  { echo: \(RequestBody s) -> do
      liftEff (log s)
      pure s
  , hello: pure "Hello, World!"
  }

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts server
