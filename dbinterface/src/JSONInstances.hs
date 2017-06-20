module JSONInstances where

import Model
import DBInstances

import Protolude
import Database.Beam
import Data.Aeson

instance ToJSON KanjiId where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiId

instance ToJSON (Auto Int) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Auto Int)

instance ToJSON Kanji where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Kanji
