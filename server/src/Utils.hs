{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Utils where

import Protolude
import Control.Lens
import qualified Data.Text as T
import Data.Char
import Common
import qualified Model as DB
import Model hiding (KanjiId, KanjiT, VocabT, Kanji)
import Text.Pretty.Simple
import Control.Exception (assert)

import Control.Monad.RWS
import Database.SQLite.Simple
import NLP.Julius.Interface
import Text.MeCab

type HandlerM = RWST HandlerEnv () HandlerState IO

data HandlerEnv = HandlerEnv
  { kanjiDbConn :: Connection
  , srsDbConn :: Connection
  , asrEngine :: RecogMainPtr
  , mecab :: MeCab
  }


data ReviewStatus =
  ReviewPending | ReviewFailure
  deriving (Eq)

-- Nothing -> Both reviews pending
type ReviewState = (Maybe ReviewType, ReviewStatus)

data HandlerState = HandlerState
  { _kanjiSearchResult :: [DB.Kanji]
  , _kanjiDisplayCount :: Int
  , _srsEntries :: Map SrsEntryId SrsEntry
  , _reviewQueue :: Map SrsEntryId ReviewState
  , _undoQueue :: [(Maybe SrsEntry, SrsEntryId
                   , ReviewState, ReviewType)]
  , _reviewStats :: SrsReviewStats
  }

reviewQueueLength = 10 :: Int
undoQueueLength = 20 :: Int
makeLenses ''HandlerState

-- Hiragana ( 3040 - 309f)
-- Katakana ( 30a0 - 30ff)
--  Full-width roman characters and half-width katakana ( ff00 - ffef)
--   CJK unifed ideographs - Common and uncommon kanji ( 4e00 - 9faf)
--   CJK unified ideographs Extension A - Rare kanji ( 3400 - 4dbf)

-- Filter valid Kanji (no hiragana or katakana)
getKanjis :: Text -> [Text]
getKanjis inp = map T.pack $ map (:[]) $ filter isKanji $ T.unpack inp

isJP :: Text -> Bool
isJP = (all f) . T.unpack
  where f c = isKana c || isKanji c

-- 3040 - 30ff
isKana c = c > l && c < h
  where l = chr $ 12352
        h = chr $ 12543

-- 3400 - 9faf
isKanji c = c > l && c < h
 where l = chr $ 13312
       h = chr $ 40879

-- 1034|其の実|そのじつ|0||0:そ;2:じつ||||607|0
-- 204|お化け屋敷|おばけやしき|0||1:ば;3:や;4:しき||||137|1
    -- , _vocabKanjiWriting = Just "一人につき"
    -- , _vocabKanaWriting = "ひとりにつき"
    -- , _vocabFurigana = Just "0-1:ひとり"

getVocabT :: DB.Vocab -> VocabT
getVocabT v =
  case (kw, f) of
    (Just w, Just f) -> VocabT $ makeKanjiKana furs w 0
      where furs1 = map (T.breakOn ":") $ T.splitOn ";" f
            furs :: [((Int, Maybe Int),Text)] -- (Index, Furigana reading)
            furs = map (\(i,f) -> (rangeParse i, T.tail f)) furs1


    (_, _) -> trace (Text.Pretty.Simple.pShowLightBg v) $ VocabT [Kana $ v ^. vocabKanaWriting]

  where kw = v ^. vocabKanjiWriting
        f = v ^. vocabFurigana

rangeParse :: Text -> (Int,Maybe Int)
rangeParse t =
  case T.any ( == '-') t of
    True -> (\(t1,t2) -> (maybe 0 identity $ readt t1, readt $ T.tail t2))
      (T.breakOn "-" t)
    False -> (maybe 0 identity $ readt t, Nothing)
  where
    readt t = readMaybe $ T.unpack t

makeKanjiKana :: [((Int, Maybe Int),Text)] -> Text -> Int -> [KanjiOrKana]
makeKanjiKana [] cs n
  | T.null cs = []
  | otherwise = [Kana cs]

makeKanjiKana (((s, Just e),fur):fs) cs n =
  if s > n
    then
      let
        -- (kana, kanji + rest)
        (ka, kn1) = T.splitAt (s-n) cs
        -- (kanji, rest)
        (kn, rest) = T.splitAt (e-s) kn1
      in [Kana ka, (Kanji (KanjiT kn) fur)] ++ (makeKanjiKana fs rest (e + 1))

    else
      let
        (kn, rest) = assert (s == n) $ T.splitAt (e-s) cs
      in (Kanji (KanjiT kn) fur) : (makeKanjiKana fs rest (e + 1))

makeKanjiKana (((s, Nothing), fur):fs) cs n =
  if s > n
    then
      let
        -- (kana, kanji + rest)
        (ka, kn1) = T.splitAt (s-n) cs
        -- (kanji, rest)
        (kn, rest) = T.splitAt 1 kn1
      in [Kana ka, (Kanji (KanjiT kn) fur)] ++ (makeKanjiKana fs rest (s + 1))

    else
      let
        (kn, rest) = assert (s == n) $ T.splitAt 1 cs
      in (Kanji (KanjiT kn) fur) : (makeKanjiKana fs rest (s + 1))
