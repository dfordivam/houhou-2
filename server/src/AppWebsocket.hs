module AppWebsocket where

import Data.Aeson
import Network.Wai.Handler.WebSockets
import Protolude

mainWebSocketHandler :: IO ()
mainWebSocketHandler = putStrLn "Handler"
