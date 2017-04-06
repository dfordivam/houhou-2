{-# LANGUAGE DeriveGeneric #-}

module Message
  where

import GHC.Generics
import Data.Aeson

import Data.Text

newtype KanjiT = KanjiT { unKanjiT :: Text }
  deriving (Generic, Show)
newtype RankT = RankT { unRankT :: Int }
  deriving (Generic, Show)
newtype MeaningT = MeaningT { unMeaningT :: Text }
  deriving (Generic, Show)

data KanjiList = KanjiList [(KanjiT, RankT, MeaningT)]
  deriving (Generic, Show)

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
