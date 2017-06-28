{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Message
  where

import Common
import Protolude
import Data.Aeson
import Reflex.WebSocket.WithWebSocket.Shared

-- Messages

type AppRequest =
  KanjiFilter
  :<|> LoadMoreKanjiResults
  :<|> GetKanjiDetails
  -- :<|>

data LoadMoreKanjiResults = LoadMoreKanjiResults
  deriving (Generic, Show)

instance WebSocketMessage AppRequest LoadMoreKanjiResults where
  type ResponseT AppRequest LoadMoreKanjiResults = KanjiFilterResult

data KanjiFilter = KanjiFilter
  { textContent :: Text
  , filter :: (Text, FilterOptions)
  , selectedRadicals :: [RadicalId]
  }
  deriving (Generic, Show)

data KanjiFilterResult =
  KanjiFilterResult KanjiList --
                    [RadicalId] -- Valid Radicals
  deriving (Eq, Generic, Show)

instance WebSocketMessage AppRequest KanjiFilter where
  type ResponseT AppRequest KanjiFilter = KanjiFilterResult

data GetKanjiDetails =
  GetKanjiDetails KanjiId
  deriving (Generic, Show)

data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails
                        VocabDisplay
  deriving (Eq, Generic, Show)

instance WebSocketMessage AppRequest GetKanjiDetails where
  type ResponseT AppRequest GetKanjiDetails = KanjiSelectionDetails

instance ToJSON KanjiSelectionDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiSelectionDetails

instance ToJSON KanjiFilterResult where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiFilterResult

instance ToJSON KanjiFilter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiFilter

instance ToJSON FilterOptions where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FilterOptions

instance ToJSON GetKanjiDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GetKanjiDetails

instance ToJSON LoadMoreKanjiResults where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LoadMoreKanjiResults
