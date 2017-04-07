module KanjiBrowser where

import Reflex.Dom
import Message
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Data.Monoid

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
enc mes = (:[]) <$> BSL.toStrict <$> encode <$> mes

-- Kanji Widget Contents
--   To search kanji
--     User text input
--     Search by meaning, reading, etc
--     Radical Table
--   Display list of kanji based on filters
--   Kanji Page
--     Kanji Details
--     Kanji -> Vocab list
--       Kana, meaning filter

-- Local Data

kanjiBrowseWidget = do
  text "Here is Kanji list"

  return ()

searchBar = do
  text

radicalTable (RadicalMatrix ) = undefined
kanjiListTable (KanjiList lst) = do
  el "table" $ do
    let rowItem (k,r,m) = do
          el "tr" $ do
            el "td" $ text (unKanjiT k)
            el "td" $ text (getRankMessage r)
            el "td" $ text (unMeaningT m)
        getRankMessage (RankT i) =
          case (mod i 10) of
            1 -> T.pack $ show i ++ "st"
            2 -> T.pack $ show i ++ "nd"
            3 -> T.pack $ show i ++ "rd"
            _ -> T.pack $ show i ++ "th"
    mapM rowItem lst

kanjiPage = undefined

kanjiWidgetControlCode = do
  refreshEv <- button "Refresh"

  let url = "localhost:3000"
      eventMessage = enc $ const ClientReq <$> refreshEv
  ws <- webSocket ("ws://" <> url <> "/kanji/list/") $
    def & webSocketConfig_send .~ eventMessage

  let handleEv ev _ =
        case (decodeStrict ev) of
          Just (KanjiBrowserResp kl) -> Just kl
          _ -> Nothing

  klDyn <- foldDynMaybe handleEv (KanjiList []) (_webSocket_recv ws)
