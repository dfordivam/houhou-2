import Protolude
import Reflex.Dom
import KanjiBrowser

import Reflex.WebSocket.WithWebSocket.Base
import Reflex.WebSocket.WithWebSocket.Shared

main = mainWidget $ do
  text "Welcome to Houhou"
  let url = "ws://localhost:3000/"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    kanjiBrowseWidget
  return ()
