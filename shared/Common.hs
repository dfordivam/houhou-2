{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
  where

import Protolude
-- import GHC.Generics
import Data.Aeson
import Data.Default

-- import Data.Text

-- New Types
newtype KanjiT = KanjiT { unKanjiT :: Text }
  deriving (Eq, Ord, Generic, Show)
newtype RankT = RankT { unRankT :: Int }
  deriving (Eq, Generic, Show)
newtype MeaningT = MeaningT { unMeaningT :: Text }
  deriving (Eq, Generic, Show)
newtype GradeT = GradeT { unGradeT :: Int }
  deriving (Eq, Generic, Show)
newtype StrokeCountT = StrokeCountT { unStrokeCountT :: Int }
  deriving (Eq, Generic, Show)
newtype JlptLevelT = JlptLevelT { unJlptLevelT :: Int }
  deriving (Eq, Generic, Show)
newtype WikiRank = WikiRank { unWikiRank :: Int }
  deriving (Eq, Generic, Show)
newtype WkLevelT = WkLevelT { unWkLevelT :: Int }
  deriving (Eq, Generic, Show)
newtype OnYomiT = OnYomiT { unOnYomiT :: Text }
  deriving (Eq, Generic, Show)
newtype KunYomiT = KunYomiT { unKunYomiT :: Text }
  deriving (Eq, Generic, Show)
newtype NanoriT = NanoriT { unNanoriT :: Text }
  deriving (Eq, Generic, Show)
newtype RadicalId = RadicalId { unRadicalId :: Int }
  deriving (Generic, Show, Eq, Ord)
newtype KanjiId = KanjiId { unKanjiId :: Int }
  deriving (Generic, Show, Eq, Ord)
newtype VocabT = VocabT { unVocabT :: [KanjiOrKana] }
  deriving (Generic, Show, Eq, Ord)

-- Kanji Widget related data

data RadicalDetails =
  RadicalDetails Text Text
  deriving (Eq, Generic, Show)

data Filter = Filter
  { readingKana :: Text
  , readingType :: ReadingType
  , meaningText :: Text
  }
  deriving (Generic, Show)

instance Default Filter where
  def = Filter "" KunYomi ""

data ReadingType = OnYomi | KunYomi | Nanori
  deriving (Eq, Ord, Generic, Show)

type KanjiList =
   [(KanjiId, KanjiT, Maybe RankT, [MeaningT])]

-- Newspaper rank
data KanjiDetails =
  KanjiDetails KanjiT
               (Maybe RankT)
               ([MeaningT])
               (Maybe GradeT)
               (Maybe JlptLevelT)
               (Maybe WkLevelT)
               (Maybe OnYomiT)
               (Maybe KunYomiT)
               (Maybe NanoriT)
  deriving (Eq, Generic, Show)


data VocabDisplay = VocabDisplay
  { vocabDispFilter :: (Text, Text, Bool, Bool)
  , vocabList :: [VocabDispItem]
  }
  deriving (Eq, Generic, Show)

data VocabDispItem =
  VocabDispItem VocabT
                (Maybe RankT)
                ([MeaningT])
                (Maybe JlptLevelT)
                (Maybe WkLevelT)
                (Maybe WikiRank)
  deriving (Eq, Generic, Show)

data KanjiOrKana
  = Kanji KanjiT Text
  | Kana Text
  deriving (Eq, Ord, Generic, Show)

type VocabCategory = Text

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

instance ToJSON JlptLevelT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON JlptLevelT
instance ToJSON StrokeCountT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StrokeCountT
instance ToJSON WikiRank where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON WikiRank
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
instance ToJSON VocabT where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VocabT
instance ToJSON KanjiOrKana where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiOrKana
instance ToJSON Filter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Filter
instance ToJSON ReadingType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ReadingType
