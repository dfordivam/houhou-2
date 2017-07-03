{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import Protolude hiding ((&))
import Control.Lens
import qualified Model as DB
import Control.Monad.RWS
import Database.SQLite.Simple
import Message
import Common
import DBInterface
import Utils
import qualified Data.Text as T

type HandlerM = RWST Connection () HandlerState IO

data HandlerState = HandlerState
  { _kanjiSearchResult :: [DB.Kanji]
  , _kanjiDisplayCount :: Int
  }
makeLenses ''HandlerState

runDB :: (DBMonad a) -> HandlerM a
runDB f = do
  conn <- ask
  res <- liftIO $ runReaderT f conn
  return res

-- Pagination,
getKanjiFilterResult :: KanjiFilter -> HandlerM KanjiFilterResult
getKanjiFilterResult (KanjiFilter inpTxt (filtTxt, filtType) rads) = do
  let uniqKanji = ordNub $ getKanjis inpTxt

  -- inpTxt in empty
  -- but filt and rads are non empty

  -- Filt, rads, inpText empty, then return most used kanji list
  -- Preferable sequence of search is
  -- 1. Input text - This will limit the number of kanji to the length of text
  -- 2. Reading filter
  -- 3. Radicals
  let
    fun
      | (T.null inpTxt) && (T.null filtTxt)
        && (null rads) =
        getMostUsedKanjis

      | (T.null inpTxt) && (T.null filtTxt) =
        getKanji =<<
          filterKanjiByRadical (DB.makeKeys rads) []

      | (T.null inpTxt) && (null rads) =
        searchKanjiByReading uniqKanji on ku na

      | (T.null filtTxt) && (null rads) =
        searchKanji uniqKanji

      | (null rads) =
        searchKanjiByReading uniqKanji on ku na

      | (T.null filtTxt) = do
        ks <- searchKanji uniqKanji
        getKanji =<<
          filterKanjiByRadical (DB.makeKeys rads)
            (map primaryKey ks)

      | otherwise = do
        --  | (T.null inpTxt)
        ks <- searchKanjiByReading uniqKanji on ku na
        getKanji =<<
          filterKanjiByRadical (DB.makeKeys rads)
            (map primaryKey ks)

    (on,ku,na) = case filtType of
      OnYomi -> (filtTxt,"HACK","HACK")
      KonYumi -> ("HACK",filtTxt,"HACK")
      Nanori -> ("HACK","HACK",filtTxt)

    getRadicals kanjiList
      | (T.null inpTxt) && (T.null filtTxt)
        && (null rads) = return []
      | otherwise =
        getValidRadicalList $ map primaryKey kanjiList

  kanjiList <- runDB fun
  validRadicals <- runDB (getRadicals kanjiList)
  let
      kanjiResultCount = 20
  state
    (\s ->((), s & kanjiSearchResult .~ kanjiList
          & kanjiDisplayCount .~ kanjiResultCount))

  let displayKanjiList =
        take kanjiResultCount kanjiList
      displayKanjiListKeys =
        fmap primaryKey $ displayKanjiList
      displayKanjiListKIds = DB.getKeys displayKanjiListKeys

  meanings <- runDB $
    mapM getKanjiMeaning $ displayKanjiListKeys

  let l = map convertKanji $
            zip displayKanjiListKIds $
            zip displayKanjiList meanings
      convertKanji
        :: (KanjiId, (DB.Kanji, Maybe DB.KanjiMeaning))
        -> (KanjiId, KanjiT, Maybe RankT, Maybe MeaningT)
      convertKanji (i,(k,m)) =
        (i, KanjiT $ k ^. DB.kanjiCharacter
        , RankT <$> (k ^. DB.kanjiMostUsedRank)
        , MeaningT <$> (DB._kanjiMeaningMeaning <$> m))
  return $ KanjiFilterResult l (DB.getKeys validRadicals)

getLoadMoreKanjiResults :: LoadMoreKanjiResults -> HandlerM KanjiFilterResult
getLoadMoreKanjiResults = undefined

getKanjiDetails :: GetKanjiDetails -> HandlerM KanjiSelectionDetails
getKanjiDetails = undefined
