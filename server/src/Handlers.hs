{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Time.Calendar

type HandlerM = RWST (Connection,Connection) () HandlerState IO

data HandlerState = HandlerState
  { _kanjiSearchResult :: [DB.Kanji]
  , _kanjiDisplayCount :: Int
  , _srsEntries :: Map SrsEntryId SrsEntry
  }
makeLenses ''HandlerState

runKanjiDB :: (DBMonad a) -> HandlerM a
runKanjiDB f = do
  conn <- ask
  res <- liftIO $ runReaderT f (fst conn)
  return res

runSrsDB :: (DBMonad a) -> HandlerM a
runSrsDB f = do
  conn <- ask
  res <- liftIO $ runReaderT f (snd conn)
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

  kanjiList <- runKanjiDB fun
  validRadicals <- runKanjiDB (getRadicals kanjiList)
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

  meanings <- runKanjiDB $
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
  ks <- runKanjiDB $ getKanji (makeKeys [kId])
  m <- runKanjiDB $ getKanjiMeaning (makeKey $ Just kId)
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
  vs <- runKanjiDB $ mapM f =<< getVocab =<< getRelatedVocab (map primaryKey ks)
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
  runKanjiDB $ mapM f =<<  getVocab =<< filterVocab r m

getSrsStats :: GetSrsStats -> HandlerM SrsStats
getSrsStats _ = do
  srsEsMap <- gets (_srsEntries)
  srsEs <- if (null srsEsMap)
    then do
      ss <- runSrsDB $ getSrsEntries
      modify (set srsEntries (Map.fromList $ map (\s-> (primaryKey s, s)) ss))
      return ss
    else return $ map snd $ Map.toList srsEsMap

  curTime <- liftIO getCurrentTime

  let
    today = utctDay curTime
    tomorrow = addDays 1 today
    countGrade g = length $ Protolude.filter (\s -> s ^. srsEntryCurrentGrade == g) srsEs
    pendingReviewCount = length $ Protolude.filter  (\s -> isJust $ (<) curTime <$> (s ^. srsEntryNextAnswerDate))
      srsEs
    reviewsToday = length $ Protolude.filter (\s -> isJust $ (<) tomorrow <$> (utctDay <$> (s ^. srsEntryNextAnswerDate)))
      srsEs
    totalItems = length srsEs
    totalReviews = sum $ map (\s -> s ^. srsEntrySuccessCount + s ^. srsEntryFailureCount) srsEs
    averageSuccess = floor $ ((fromIntegral ((sum $ map _srsEntrySuccessCount srsEs)*100)) / (fromIntegral totalReviews))
    discoveringCount = (countGrade 0, countGrade 1)
    committingCount = (countGrade 2, countGrade 3)
    bolsteringCount = (countGrade 4, countGrade 5)
    assimilatingCount = (countGrade 6, countGrade 7)
    setInStone = countGrade 8
  return SrsStats {..}

getBrowseSrsItems      :: BrowseSrsItems
  -> HandlerM [SrsItem]
getBrowseSrsItems (BrowseSrsItems lvls) = do
  srsEsMap <- gets (_srsEntries)
  let res = Map.elems $ Map.filter
        (\s -> elem (s ^. srsEntryCurrentGrade) lvls)
        srsEsMap
      f s = SrsItem <$> (getKey $ primaryKey s) <*> v
        <*> pure (s ^. srsEntryCurrentGrade)
        where
          v = case (s ^. srsEntryAssociatedKanji,
                   s ^. srsEntryAssociatedVocab) of
                ((Just k), _) -> Just $ Right $ KanjiT k
                (_, (Just v)) -> Just $ Left $
                  VocabT $ [Kana v]
                _ -> Nothing

  return $ catMaybes $ map f res

getGetNextReviewItem   :: GetNextReviewItem
  -> HandlerM (Maybe ReviewItem)
getGetNextReviewItem = undefined
getDoReview            :: DoReview
  -> HandlerM (Maybe ReviewItem)
getDoReview = undefined
getEditSrsItem         :: EditSrsItem
  -> HandlerM ()
getEditSrsItem = undefined
getBulkEditSrsItems :: BulkEditSrsItems
  -> HandlerM ()
getBulkEditSrsItems = undefined
