{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Handlers where

import Protolude hiding ((&))
import Control.Lens
import qualified Model as DB
import Model hiding (KanjiId, KanjiT)
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
getKanjiFilterResult (KanjiFilter inpTxt (Filter filtTxt filtType _) rads) = do
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
      KunYomi -> ("HACK",filtTxt,"HACK")
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
        :: (KanjiId, (DB.Kanji, [DB.KanjiMeaning]))
        -> (KanjiId, KanjiT, Maybe RankT, [MeaningT])
      convertKanji (i,(k,m)) =
        (i, KanjiT $ k ^. DB.kanjiCharacter
        , RankT <$> (k ^. DB.kanjiMostUsedRank)
        , MeaningT <$> (DB._kanjiMeaningMeaning <$> m))
  return $ KanjiFilterResult l (DB.getKeys validRadicals)

getLoadMoreKanjiResults :: LoadMoreKanjiResults -> HandlerM KanjiFilterResult
getLoadMoreKanjiResults = undefined

getKanjiDetails :: GetKanjiDetails -> HandlerM (Maybe KanjiSelectionDetails)
getKanjiDetails (GetKanjiDetails kId _) = do
  ks <- runDB $ getKanji (makeKeys [kId])
  m <- runDB $ getKanjiMeaning (makeKey $ Just kId)
  let
    kd = f <$> headMay ks
    f k =
      KanjiDetails (KanjiT $ k ^. kanjiCharacter)
        (RankT <$> k ^. kanjiMostUsedRank)
        (MeaningT <$> (_kanjiMeaningMeaning <$> m))
        (GradeT <$> k ^. kanjiGrade)
        (JlptLevelT <$> k ^. kanjiJlptLevel)
        (WkLevelT <$> k ^. kanjiWkLevel)
        (OnYomiT <$> k ^. kanjiOnyomi)
        (KunYomiT <$> k ^. kanjiKunyomi)
        (NanoriT <$> k ^. kanjiNanori)

  let f v = do
        ms <- getVocabMeaning (primaryKey v)
        return $ VocabDispItem (getVocabT v)
                (RankT <$> v ^. vocabFreqRank)
                (map (MeaningT . _vocabMeaningMeaning) ms)
                (JlptLevelT <$> v ^. vocabJlptLevel)
                (WkLevelT <$> v ^. vocabWkLevel)
                (WikiRank <$> v ^. vocabWikiRank)
  vs <- runDB $ mapM f =<< getVocab =<< getRelatedVocab (map primaryKey ks)
  return $ KanjiSelectionDetails <$> kd <*> pure vs

getVocabSearch :: VocabSearch -> HandlerM [VocabDispItem]
getVocabSearch (VocabSearch (Filter r _ m)) = do
  let f v = do
        ms <- getVocabMeaning (primaryKey v)
        return $ VocabDispItem (getVocabT v)
                (RankT <$> v ^. vocabFreqRank)
                (map (MeaningT . _vocabMeaningMeaning) ms)
                (JlptLevelT <$> v ^. vocabJlptLevel)
                (WkLevelT <$> v ^. vocabWkLevel)
                (WikiRank <$> v ^. vocabWikiRank)
  runDB $ mapM f =<<  getVocab =<< filterVocab r m
