module Handler.Kanji where

import Import
import Message

import Yesod.WebSockets
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

getKanjiListR = do
  webSockets (webSockHandler)
  return ()

webSockHandler = do

  sourceWS $$ Data.Conduit.List.mapMaybeM
      (handler)
    =$= sinkWSBinary
  where
    handler :: (MonadIO m, MonadLogger m)
      => BSL.ByteString
      -> m (Maybe BSL.ByteString)
    handler req' = do
      case decode req' of
        Just (tag,req) -> do
          resp <- handleRequest req
          return $ Just $ encode ((tag :: Value),resp)
        _ -> Nothing

handleRequest :: (ToJSON a) => ClientReq -> WebSocketsT Handler a
handleRequest (DoKanjiFilter filt) = do
  $logError $ tshow filt
  let kanjiList = [
        (KanjiId 1, KanjiT "a", RankT 1, MeaningT "a-meaning"),
        (KanjiId 2, KanjiT "b", RankT 2, MeaningT "b-meaning")]
      validRadicals = [1,2]
  return $ KanjiFilterResult kanjiList validRadicals

handleRequest (GetKanjiDetails i) = do
  $logError $ tshow i
  let kanjiDetails = undefined
      vocabs = VocabDisplay ("","", False,False) []
  return $ KanjiSelectionDetails kanjiDetails vocabs
