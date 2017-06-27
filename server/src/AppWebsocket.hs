module AppWebsocket where

import Data.Aeson
import Network.Wai.Handler.WebSockets
import Protolude
import Handlers

mainWebSocketHandler :: IO ()
mainWebSocketHandler = putStrLn "Handler"
