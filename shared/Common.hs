{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common
  where

import ClassyPrelude
-- import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map

-- import Data.Text

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
newtype RadicalId = RadicalId { unRadicalId :: Int }
  deriving (Generic, Show, Eq, Ord)
newtype KanjiId = KanjiId { unKanjiId :: Int }
  deriving (Generic, Show, Eq, Ord)

-- Kanji Widget related data

data RadicalDetails =
  RadicalDetails String
  deriving (Generic, Show)

type RadicalTable = Map RadicalId RadicalDetails

radicalTable :: RadicalTable
radicalTable = Map.fromList $
  [(RadicalId 1, RadicalDetails "a")
  ,(RadicalId 2, RadicalDetails "b")
  ,(RadicalId 3, RadicalDetails "c")]

data KanjiFilter = KanjiFilter
  { textContent :: Text
  , filter :: (Text, FilterOptions)
  , selectedRadicals :: [RadicalId]
  }
  deriving (Generic, Show)

data FilterOptions = OnYomi | KonYumi | Nanori
  deriving (Generic, Show)

type KanjiList =
   [(KanjiId, KanjiT, RankT, MeaningT)]

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
  deriving (Generic, Show)


data VocabDisplay = VocabDisplay
  { vocabDispFilter :: (Text, Text, Bool, Bool)
  , vocabList :: [VocabDispItem]
  }
  deriving (Generic, Show)

data VocabDispItem = VocabDispItem
  deriving (Generic, Show)

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

instance ToJSON RadicalId where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON RadicalId

instance ToJSON KanjiId where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiId

instance ToJSON KanjiDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiDetails

instance ToJSON GradeT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GradeT

instance ToJSON MostUsedRankT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON MostUsedRankT
instance ToJSON JlptLevelT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON JlptLevelT
instance ToJSON StrokeCountT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StrokeCountT
instance ToJSON WkLevelT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON WkLevelT
instance ToJSON OnYomiT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON OnYomiT
instance ToJSON KunYomiT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KunYomiT
instance ToJSON NanoriT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON NanoriT
instance ToJSON VocabDisplay where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VocabDisplay

instance ToJSON VocabDispItem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VocabDispItem
-- instance ToJSON  where
--   toEncoding = genericToEncoding defaultOptions
-- instance FromJSON
