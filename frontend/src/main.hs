import Protolude
import Reflex.Dom
import KanjiBrowser

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.SemanticUI

main = mainWidget $ divClass "ui container" $ do
  text "Welcome to Houhou"
  elClass "h4" "ui header" $ do
    text "Dropdowns"
    uiButton (rightFloated . blue . mini . compact . basic <$> def) $ text "Reset"
  let url = "ws://localhost:3000/"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    kanjiBrowseWidget
  return ()
