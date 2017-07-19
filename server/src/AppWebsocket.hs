{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module AppWebsocket where

import Protolude
import Handlers
import DBInterface (openKanjiDB)

import Control.Monad.RWS
import Data.IORef
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.WebSocket.Server
import qualified Database.SQLite.Simple as DB
import qualified Message
import Network.HTTP.Types.Status

mainWebSocketHandler :: IO ()
mainWebSocketHandler = do
  handlerStateRef <- newIORef $
                     HandlerState [] 20
  dbConn <- openKanjiDB
  runEnv 3000 (app handlerStateRef dbConn)


app :: IORef HandlerState -> DB.Connection -> Application
app handlerStateRef dbConn =
  websocketsOr defaultConnectionOptions wsApp backupApp
  where
    -- wsApp :: ServerApp
    wsApp pending_conn = do
      conn <- acceptRequest pending_conn
      loop conn

    loop conn = do
      d <- receiveData conn
      print d
      let
          rwst = handleRequest handler d

      hState <- readIORef handlerStateRef
      (resp, newState, _) <- runRWST rwst dbConn hState
      writeIORef handlerStateRef newState

      -- print resp
      sendBinaryData conn resp
      loop conn

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

handler :: HandlerWrapper HandlerM Message.AppRequest
handler = HandlerWrapper $
  h getKanjiFilterResult
  :<&> h getLoadMoreKanjiResults
  :<&> h getKanjiDetails
  :<&> h getVocabSearch

  where
  h :: (WebSocketMessage Message.AppRequest a, Monad m)
    => (a -> m (ResponseT Message.AppRequest a))
    -> Handler m Message.AppRequest a
  h = makeHandler
