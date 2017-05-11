{-# LANGUAGE DeriveGeneric #-}

module Message
  where

import Common
import GHC.Generics
import Data.Aeson
import Prelude
import Data.Text

-- Messages

class (ToJSON req, FromJSON req,
       ToJSON (Response req), FromJSON (Response req)
      ) => IsRequest req where
  type Response req :: * -> *

data ClientReq =
  DoKanjiFilter KanjiFilter
  | GetKanjiDetails KanjiId
  deriving (Generic, Show)

data Response =
  KanjiListResp KanjiList
  | KanjiSelectionResp KanjiDetails VocabDisplay
  deriving (Generic, Show)
