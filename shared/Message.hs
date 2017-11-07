{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  :<|> GetSrsStats
  :<|> BrowseSrsItems
  :<|> GetNextReviewItem
  :<|> CheckAnswerAudio
  :<|> DoReview
  :<|> GetSrsItem
  :<|> EditSrsItem
  :<|> BulkEditSrsItems

----------------------------------------------------------------
data KanjiFilter = KanjiFilter
  { textContent :: Text
  , kanjiFilter :: Filter
  , selectedRadicals :: [RadicalId]
  }
  deriving (Generic, Show)

instance Default KanjiFilter where
  def = KanjiFilter "" def []

instance WebSocketMessage AppRequest KanjiFilter where
  type ResponseT AppRequest KanjiFilter = KanjiFilterResult

----------------------------------------------------------------
data KanjiFilterResult =
  KanjiFilterResult KanjiList --
                    [RadicalId] -- Valid Radicals
  deriving (Eq, Generic, Show)

data GetKanjiDetails =
  GetKanjiDetails KanjiId Filter
  deriving (Generic, Show)

instance WebSocketMessage AppRequest GetKanjiDetails where
  type ResponseT AppRequest GetKanjiDetails = Maybe KanjiSelectionDetails

----------------------------------------------------------------
data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails
                        [VocabDispItem]
  deriving (Eq, Generic, Show)

data LoadMoreKanjiResults = LoadMoreKanjiResults
  deriving (Generic, Show)

instance WebSocketMessage AppRequest LoadMoreKanjiResults where
  type ResponseT AppRequest LoadMoreKanjiResults = KanjiFilterResult

----------------------------------------------------------------
data VocabSearch = VocabSearch Filter
  deriving (Generic, Show)

instance WebSocketMessage AppRequest VocabSearch where
  type ResponseT AppRequest VocabSearch = [VocabDispItem]

----------------------------------------------------------------
data GetSrsStats = GetSrsStats ()
  deriving (Generic, Show)

data SrsStats = SrsStats
  { pendingReviewCount :: Int
  , reviewsToday :: Int
  , totalItems :: Int
  , totalReviews :: Int
  , averageSuccess :: Int
  , discoveringCount :: (Int, Int)
  , committingCount :: (Int, Int)
  , bolsteringCount :: (Int, Int)
  , assimilatingCount :: (Int, Int)
  , setInStone :: Int
  }
  deriving (Generic, Show)

instance WebSocketMessage AppRequest GetSrsStats where
  type ResponseT AppRequest GetSrsStats = SrsStats

----------------------------------------------------------------
data BrowseSrsItems = BrowseSrsItems [SrsLevel]
  deriving (Generic, Show)

data SrsItems = SrsItems
  deriving (Generic, Show)

instance WebSocketMessage AppRequest BrowseSrsItems where
  type ResponseT AppRequest BrowseSrsItems = [SrsItem]

----------------------------------------------------------------
data GetNextReviewItem = GetNextReviewItem
  deriving (Generic, Show)

instance WebSocketMessage AppRequest GetNextReviewItem where
  type ResponseT AppRequest GetNextReviewItem
    = Maybe ReviewItem

data CheckAnswerAudio =
  CheckAnswerAudio ReadingT [Int]
  deriving (Generic, Show, ToJSON, FromJSON)

data CheckAnswerAudioResult
  = AnswerCorrect
  | AnswerIncorrect Text
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest CheckAnswerAudio where
  type ResponseT AppRequest CheckAnswerAudio = CheckAnswerAudioResult

----------------------------------------------------------------
data DoReview
  = DoReview SrsItemId ReviewType Bool
  | UndoReview
  | AddAnswer SrsItemId ReviewType Text
  deriving (Generic, Show)

instance WebSocketMessage AppRequest DoReview where
  type ResponseT AppRequest DoReview = Maybe ReviewItem

----------------------------------------------------------------
data GetSrsItem = GetSrsItem SrsItemId
  deriving (Generic, Show)

instance WebSocketMessage AppRequest GetSrsItem where
  type ResponseT AppRequest GetSrsItem = Maybe SrsItemFull

----------------------------------------------------------------
data EditSrsItem = EditSrsItem SrsItemFull
  deriving (Generic, Show)

instance WebSocketMessage AppRequest EditSrsItem where
  type ResponseT AppRequest EditSrsItem = ()

----------------------------------------------------------------
data BulkEditSrsItems = BulkEditSrsItems [SrsItemId] BulkEditOperation BrowseSrsItems
  deriving (Generic, Show)

instance WebSocketMessage AppRequest BulkEditSrsItems where
  type ResponseT AppRequest BulkEditSrsItems = [SrsItem]

----------------------------------------------------------------
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

instance ToJSON GetSrsStats where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GetSrsStats
instance ToJSON SrsStats where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SrsStats
instance ToJSON BrowseSrsItems where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BrowseSrsItems
instance ToJSON SrsItems where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SrsItems
instance ToJSON GetNextReviewItem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GetNextReviewItem
instance ToJSON DoReview where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON DoReview
instance ToJSON EditSrsItem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EditSrsItem

instance ToJSON BulkEditSrsItems where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON BulkEditSrsItems

instance ToJSON GetSrsItem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GetSrsItem
