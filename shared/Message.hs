module Message
  where

import Common
import ClassyPrelude
import Data.Aeson

-- Messages


-- data ClientReq
--   = DoKanjiFilter KanjiFilter
--   | GetKanjiDetails KanjiId
--   deriving (Generic, Show)

data KanjiFilterResult =
  KanjiFilterResult KanjiList --
                    [RadicalId] -- Valid Radicals
  deriving (Generic, Show)

data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails
                        VocabDisplay
  deriving (Generic, Show)

instance ToJSON KanjiSelectionDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiSelectionDetails

instance ToJSON KanjiFilterResult where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiFilterResult
