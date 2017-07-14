import Protolude
import Reflex.Dom
import KanjiBrowser

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.SemanticUI
import Message (AppRequest)
import Control.Monad.Primitive

main = mainWidget $ do
  let url = "ws://localhost:3000/"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    topWidget
  return ()

topWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
topWidget = divClass "ui grid container" $ do
  -- navigation with visibility control
  kanjiBrowseWidget
