import Reflex.Dom
import KanjiBrowser

import Reflex.Dom.Contrib.WithWebSocket.Base

main = mainWidget $ do
  text "Welcome to Houhou"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    kanjiBrowseWidget
