module Message
  where

import Common
import Protolude
import Data.Aeson

-- Messages

type AppRequest =
  KanjiFilter
  :<|> GetKanjiDetails
  -- :<|>

data KanjiFilter = KanjiFilter
  { textContent :: Text
  , filter :: (Text, FilterOptions)
  , selectedRadicals :: [RadicalId]
  }
  deriving (Generic, Show)

data KanjiFilterResult =
  KanjiFilterResult KanjiList --
                    [RadicalId] -- Valid Radicals
  deriving (Generic, Show)

instance WebSocketMessage AppRequest KanjiFilter where
  type ResponseT AppRequest KanjiFilter = KanjiFilterResult

data GetKanjiDetails =
  GetKanjiDetails KanjiId

data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails
                        VocabDisplay
  deriving (Generic, Show)

instance WebSocketMessage AppRequest GetKanjiDetails where
  type ResponseT AppRequest GetKanjiDetails = KanjiSelectionDetails

instance ToJSON KanjiSelectionDetails where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiSelectionDetails

instance ToJSON KanjiFilterResult where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON KanjiFilterResult
