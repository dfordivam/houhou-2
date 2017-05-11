{-# LANGUAGE DeriveGeneric #-}

module Common
  where

import GHC.Generics
import Data.Aeson
import Prelude

import Data.Text

-- New Types
newtype KanjiT = KanjiT { unKanjiT :: Text }
  deriving (Generic, Show)
newtype RankT = RankT { unRankT :: Int }
  deriving (Generic, Show)
newtype MeaningT = MeaningT { unMeaningT :: Text }
  deriving (Generic, Show)
newtype GradeT = GradeT { unGradeT :: Int }
  deriving (Generic, Show)
newtype StrokeCountT = StrokeCountT { unStrokeCountT :: Int }
  deriving (Generic, Show)
newtype MostUsedRankT = MostUsedRankT { unMostUsedRankT :: Int }
  deriving (Generic, Show)
newtype JlptLevelT = JlptLevelT { unJlptLevelT :: Int }
  deriving (Generic, Show)
newtype WkLevelT = WkLevelT { unWkLevelT :: Int }
  deriving (Generic, Show)
newtype OnYomiT = OnYomiT { unOnYomiT :: Text }
  deriving (Generic, Show)
newtype KunYomiT = KunYomiT { unKunYomiT :: Text }
  deriving (Generic, Show)
newtype NanoriT = NanoriT { unNanoriT :: Text }
  deriving (Generic, Show)

-- Kanji Widget related data

data KanjiFilter = KanjiFilter
  { textContent :: Text
  , filter :: (Text, FilterOption)
  , selectedRadicals :: [RadicalId]
  }

data KanjiList =
  KanjiList [(KanjiId, KanjiT, RankT, MeaningT)]
  deriving (Generic, Show)

data KanjiDetails =
  KanjiDetails KanjiT
               RankT
               MeaningT
               GradeT
               MostUsedRankT
               JlptLevelT
               WkLevelT
               OnYomiT
               KunYomiT
               NanoriT


data VocabDisplay = VocabDisplay
  { vocabDispFilter :: (Text, Text, Bool, Bool)
  , vocabList :: [VocabDispItem]
  }
-- Instance declarations
instance ToJSON KanjiT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiT

instance ToJSON RankT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON RankT

instance ToJSON MeaningT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON MeaningT

instance ToJSON KanjiList where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiList

instance ToJSON ClientReq where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ClientReq

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Response
