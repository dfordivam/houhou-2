{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Message
  where

import Common
import Protolude
import Data.Aeson
import Data.Default

import Reflex.Dom.WebSocket.Message
-- Messages

type AppRequest =
  KanjiFilter
  :<|> LoadMoreKanjiResults
  :<|> GetKanjiDetails
  :<|> VocabSearch

data KanjiFilter = KanjiFilter
  { textContent :: Text
  , filter :: Filter
  , selectedRadicals :: [RadicalId]
  }
  deriving (Generic, Show)

instance Default KanjiFilter where
  def = KanjiFilter "" def []

instance WebSocketMessage AppRequest KanjiFilter where
  type ResponseT AppRequest KanjiFilter = KanjiFilterResult

data KanjiFilterResult =
  KanjiFilterResult KanjiList --
                    [RadicalId] -- Valid Radicals
  deriving (Eq, Generic, Show)

data GetKanjiDetails =
  GetKanjiDetails KanjiId Filter
  deriving (Generic, Show)

instance WebSocketMessage AppRequest GetKanjiDetails where
  type ResponseT AppRequest GetKanjiDetails = Maybe KanjiSelectionDetails

data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails
                        [VocabDispItem]
  deriving (Eq, Generic, Show)

data LoadMoreKanjiResults = LoadMoreKanjiResults
  deriving (Generic, Show)

instance WebSocketMessage AppRequest LoadMoreKanjiResults where
  type ResponseT AppRequest LoadMoreKanjiResults = KanjiFilterResult

data VocabSearch = VocabSearch Filter
  deriving (Generic, Show)

instance WebSocketMessage AppRequest VocabSearch where
  type ResponseT AppRequest VocabSearch = [VocabDispItem]

instance ToJSON KanjiSelectionDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiSelectionDetails

instance ToJSON KanjiFilterResult where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiFilterResult

instance ToJSON KanjiFilter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiFilter

instance ToJSON GetKanjiDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GetKanjiDetails

instance ToJSON VocabSearch where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VocabSearch

instance ToJSON LoadMoreKanjiResults where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LoadMoreKanjiResults
