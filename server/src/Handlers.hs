{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Handlers where

import Protolude hiding ((&))
import Control.Lens
import qualified Model as DB
import Model hiding (KanjiId, KanjiT, VocabT)
import Message
import Common
import DBInterface
import Utils
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Calendar
import Text.Pretty.Simple
import System.Random
import Text.MeCab
import NLP.Romkan

runKanjiDB :: (DBMonad a) -> HandlerM a
runKanjiDB f = do
  conn <- asks kanjiDbConn
  res <- liftIO $ runReaderT f conn
  return res

runSrsDB :: (DBMonad a) -> HandlerM a
runSrsDB f = do
  conn <- asks srsDbConn
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
  srsEs <- if (null srsEsMap)
    then do
      ss <- runSrsDB $ getSrsEntries
      modify (set srsEntries (Map.fromList $ map (\s-> (primaryKey s, s)) ss))
      return ss
    else return $ map snd $ Map.toList srsEsMap
  srsEsMap <- gets (_srsEntries)
  curTime <- liftIO getCurrentTime

  let res = Map.elems $
        Map.filter
          (\s -> not (s ^. srsEntryIsDeleted)) $
        Map.filter
          (\s -> elem (s ^. srsEntryCurrentGrade) lvls)
        srsEsMap
      f s = SrsItem <$> (getKey $ primaryKey s) <*> v
        <*> pure (isJust $ s ^. srsEntrySuspensionDate)
        <*> p (s ^. srsEntryNextAnswerDate)
        where
          v = getVocabOrKanji s
          p Nothing = Just False
          p (Just d) = Just (d < curTime)

  liftIO $ pPrint res
  let r = catMaybes $ map f res
  liftIO $ pPrint r

  return $ r

getVocabOrKanji :: SrsEntry -> Maybe (Either VocabT KanjiT)
getVocabOrKanji s =
  case (s ^. srsEntryAssociatedKanji,
        s ^. srsEntryAssociatedVocab) of
    ((Just k), _) -> Just $ Right $ KanjiT k
    (_, (Just v)) -> Just $ Left $ VocabT $ [Kana v]
    _ -> Nothing

getBulkEditSrsItems :: BulkEditSrsItems
  -> HandlerM [SrsItem]
getBulkEditSrsItems (BulkEditSrsItems ss op filt) = do
  srsEsMap <- gets (_srsEntries)
  curTime <- liftIO getCurrentTime

  case op of
    SuspendSrsItems -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> HandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntrySuspensionDate ?~ curTime
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    ResumeSrsItems -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> HandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            -- s must be already suspended and has a valid review date
            -- Add check for grade <8 ??
            reviewDate :: Maybe UTCTime
            reviewDate = g <$> (s ^? _Just . srsEntrySuspensionDate . _Just)
              <*> (s ^? _Just . srsEntryNextAnswerDate . _Just)
            g susDate prevReviewDate = if susDate > prevReviewDate
              then curTime
              else addUTCTime (diffUTCTime prevReviewDate susDate) curTime
            sNew :: Maybe SrsEntry
            sNew = join $ (\r -> s & _Just . srsEntrySuspensionDate .~ Nothing
                     & _Just . srsEntryNextAnswerDate ?~ r) <$> reviewDate
            sMap' = maybe sMap identity ((\n -> Map.update (const (Just n)) sIdK sMap) <$> sNew)
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    ChangeSrsLevel l -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> HandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntryCurrentGrade .~ l
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    ChangeSrsReviewData d -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> HandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntryNextAnswerDate ?~ d
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    DeleteSrsItems -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> HandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntryIsDeleted .~ True
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

  getBrowseSrsItems filt

getSrsItem :: GetSrsItem
  -> HandlerM (Maybe SrsItemFull)
getSrsItem (GetSrsItem i) = do
  srsEsMap <- gets (_srsEntries)
  let
      res :: SrsEntry -> Maybe SrsItemFull
      res s = g s <$> (getKey $ primaryKey s) <*> v
        where
          v = getVocabOrKanji s
      g :: SrsEntry -> Int -> Either Common.VocabT Common.KanjiT -> SrsItemFull
      g s k v = SrsItemFull k v
        (s ^. srsEntryNextAnswerDate)
        (s ^. srsEntryMeanings)
        (s ^. srsEntryReadings)
        (s ^. srsEntryCurrentGrade)
        (s ^. srsEntryMeaningNote)
        (s ^. srsEntryReadingNote)
        (s ^. srsEntryTags)

  return $ join $ res <$> Map.lookup (makeKey $ Just i) srsEsMap

getEditSrsItem :: EditSrsItem
  -> HandlerM ()
getEditSrsItem (EditSrsItem sItm)= do
  srsEsMap <- gets (_srsEntries)
  curTime <- liftIO getCurrentTime

  let
    sIdK = (makeKey $ Just $ srsItemFullId sItm)
    s = Map.lookup sIdK srsEsMap
    sNew = s
      & _Just . srsEntryNextAnswerDate .~ (srsReviewDate sItm)
      & _Just . srsEntryMeanings .~ (srsMeanings sItm)
      & _Just . srsEntryReadings .~ (srsReadings sItm)
      & _Just . srsEntryCurrentGrade .~ (srsCurrentGrade sItm)
      & _Just . srsEntryMeaningNote .~ (srsMeaningNote sItm)
      & _Just . srsEntryReadingNote .~ (srsReadingNote sItm)
      & _Just . srsEntryTags .~ (srsTags sItm)

    sMap' = Map.update (const sNew) sIdK srsEsMap
  mapM runSrsDB (updateSrsEntry <$> sNew)
  void $ modify (set srsEntries sMap')

-- maintain state from last review session
-- Only append to the reviewQueue
getGetNextReviewItem   :: GetNextReviewItem
  -> HandlerM (Maybe ReviewItem)
getGetNextReviewItem _ = do
  srsEsMap <- gets (_srsEntries)
  reviewQOld <- gets (_reviewQueue)
  curTime <- liftIO getCurrentTime
  let
    pendingReviewItems = Map.keys $ Map.filter
      (\s -> isNothing $ s ^. srsEntrySuspensionDate) $ Map.filter
      (\s -> isJust $ (<) curTime
        <$> (s ^. srsEntryNextAnswerDate))
      srsEsMap

  reviewQ <- liftIO $ getRandomItems pendingReviewItems
    reviewQueueLength
  modify (set (reviewStats . srsReviewStats_pendingCount)
          (length pendingReviewItems))
  let newReviewQ = Map.union reviewQOld reviewQMap
      reviewQMap = Map.fromList $
        map (\v -> (v,(Nothing,ReviewPending))) reviewQ
  void $ modify (set reviewQueue newReviewQ)

  fetchNewReviewItem

-- Assumption - A review item which fails while reviewing
-- will be present in the reviewQueue
-- Therefore its DB entry is not modified till it is
-- succesfully reviewed

-- Notes on undoQueue working
-- Every time the DoReview is called a snapshot of the
-- Old SrsEntry and the old value of (Maybe ReviewType)
-- is stored in queue
getDoReview
  :: DoReview
  -> HandlerM (Maybe ReviewItem)
getDoReview dr = do
  curTime <- liftIO getCurrentTime
  srsEsMap <- gets (_srsEntries)
  reviewQ <- gets (_reviewQueue)
  reviewSts <- gets (_reviewStats)
  undoQ <- gets (_undoQueue)
  let
    f r _ r' = case (r,r') of
      (r, (Nothing,rStatus)) -> Just $ (Just r, rStatus)
      -- returning Nothing will remove the item from Queue
      (MeaningReview, (Just ReadingReview,_)) -> Nothing
      (ReadingReview, (Just MeaningReview,_)) -> Nothing
      _ -> Just r' -- This is some error condition

    g :: SrsEntryId
      -> ReviewType
      -> ReviewState
      -> SrsEntry
      -> HandlerM ()
    g i r rOld sOld =  do
      let
        (rOldItemMaybe,rQ) =
          Map.updateLookupWithKey (f r) i reviewQ
        uQ = take undoQueueLength $
          (sOld <$ sNew, i, rOld, r) : undoQ

        newSrsEntryMap = (\s->Map.insert i s srsEsMap)
          <$> sNew
        reviewComplete = Map.notMember i rQ

        -- Just True -> Review correct
        -- Just False -> Review incorrect
        reviewSuccess :: Maybe Bool
        reviewSuccess = if reviewComplete
          then (== ReviewPending) <$> (snd <$> rOldItemMaybe)
          else Nothing

        sNew =
          let
            g b = getNextReviewDate b curTime
                (sOld ^. srsEntryNextAnswerDate)
                (sOld ^. srsEntryCurrentGrade)

          in (flip fmap) reviewSuccess $ \case
            True -> sOld
              & srsEntrySuccessCount +~ 1
              & srsEntryCurrentGrade +~ 1
              & srsEntryNextAnswerDate ?~ g True
            False -> sOld
              & srsEntryFailureCount +~ 1
              & srsEntryCurrentGrade -~ 1
              & srsEntryNextAnswerDate ?~ g False

        -- Modify pending review count
        statsModify True = over
          (reviewStats . srsReviewStats_correctCount) (+ 1)
        statsModify False = over
          (reviewStats . srsReviewStats_incorrectCount) (+ 1)

      sequence_ $ (\m -> modify (set srsEntries m))
        <$> newSrsEntryMap
      sequence_ $ (\b -> modify (statsModify b))
        <$> reviewSuccess
      mapM runSrsDB (updateSrsEntry <$> sNew)
      void $ modify (set reviewQueue rQ)
      void $ modify (set undoQueue uQ)

  case dr of
    (DoReview i r True) -> do -- Correct Answer
      let
        id = makeKey $ Just i
        rOld = Map.lookup id reviewQ
        sOld = Map.lookup id srsEsMap

        fun :: Maybe (HandlerM ())
        fun = g id r <$> rOld <*> sOld
      sequence_ fun
      fetchNewReviewItem

    (DoReview i r False) -> do -- Wrong Answer
      let
        id = makeKey $ Just i
        f _ (rt,_) = Just (rt,ReviewFailure)
        (rOldItemMaybe,rQ) =
          Map.updateLookupWithKey f id reviewQ

        rOld = maybe (Nothing,ReviewPending) identity
          rOldItemMaybe
        uQ = take undoQueueLength $
          (Nothing, id, rOld, r) : undoQ

      void $ modify (set reviewQueue rQ)
      void $ modify (set undoQueue uQ)
      fetchNewReviewItem

    (UndoReview) -> do
      let
        h = headMay undoQ
        fSrsEntry s = do
          runSrsDB $ updateSrsEntry s
          let m = Map.insert (primaryKey s) s srsEsMap
          modify (set srsEntries m)

        fReviewQ (_,id,st,_) = do
          let rQ = Map.insert id st reviewQ
          modify (set reviewQueue rQ)

        fUndoQ ((_,id,_,rt):qs) = do
          modify (set undoQueue qs)
          return $ getReviewItem srsEsMap reviewSts id rt
        fUndoQ [] = return Nothing

      sequence_ $ fSrsEntry <$> (h ^? _Just . _1 ._Just)
      sequence_ $ fReviewQ <$> h
      fUndoQ undoQ

    (AddAnswer i rt t) -> return Nothing


fetchNewReviewItem :: HandlerM (Maybe ReviewItem)
fetchNewReviewItem = do
  reviewQ <- gets (_reviewQueue)
  if null reviewQ
    then return Nothing
    else fetchNewReviewItemInt reviewQ

fetchNewReviewItemInt reviewQ = do
  srsEsMap <- gets (_srsEntries)
  reviewStats <- gets (_reviewStats)
  (rId:_) <- liftIO $ getRandomItems (Map.keys reviewQ) 1
  rtToss <- liftIO $ randomIO
  let
    rtDone :: Maybe ReviewType
    rtDone = (Map.lookup rId reviewQ) ^? _Just . _1 . _Just

    rt :: ReviewType
    rt = case rtDone of
      Nothing -> if rtToss
        then ReadingReview
        else MeaningReview
      (Just ReadingReview) -> MeaningReview
      (Just MeaningReview) -> ReadingReview

  return $ getReviewItem srsEsMap reviewStats rId rt

getReviewItem
  :: Map SrsEntryId SrsEntry
  -> SrsReviewStats
  -> SrsEntryId
  -> ReviewType
  -> Maybe ReviewItem
getReviewItem srsEsMap reviewStats rId rt =
  let
    s :: Maybe SrsEntry
    s = Map.lookup rId srsEsMap

    i :: Maybe SrsItemId
    i = join $ (getKey <$> (primaryKey <$> s))

    k :: Maybe (Either VocabT KanjiT)
    k = join $ getVocabOrKanji <$> s

    m :: Maybe (Either (MeaningT, MeaningNotesT)
                (ReadingT, ReadingNotesT))
    m = getM <$> s
    getM s = case rt of
      MeaningReview -> Left
        (MeaningT $ s ^. srsEntryMeanings
        , maybe "" identity $ s ^.srsEntryMeaningNote)

      ReadingReview -> Right
        (s ^. srsEntryReadings
        , maybe "" identity $ s ^. srsEntryReadingNote)

    ret = ReviewItem <$> i <*> k <*> m <*> pure reviewStats
  in ret


getRandomItems :: [a] -> Int -> IO [a]
getRandomItems inp s = do
  let l = length inp
      idMap = Map.fromList $ zip [1..l] inp

      loop set = do
        r <- randomRIO (1,l)
        let setN = Set.insert r set
        if Set.size setN >= s
          then return setN
          else loop setN

  set <- loop Set.empty
  return $ catMaybes $
    fmap (\k -> Map.lookup k idMap) $ Set.toList set

getNextReviewDate
  :: Bool
  -> UTCTime
  -> Maybe UTCTime
  -> Int
  -> UTCTime
getNextReviewDate
  success curTime revDate oldGrade =
  let
    addHour h = addUTCTime (h*60*60) curTime
    addDay d = addUTCTime (d*24*60*60) curTime
  in case (oldGrade, success) of
    (0,_) -> addHour 4
    (1,False) -> addHour 4

    (2,False) -> addHour 8
    (1,True) -> addHour 8

    (2,True) -> addDay 1
    (3,False) -> addDay 1

    (3,True) -> addDay 3
    (4,False) -> addDay 3

    (4,True) -> addDay 7
    (5,False) -> addDay 7

    (5,True) -> addDay 14
    (6,False) -> addDay 14

    (6,True) -> addDay 30
    (7,False) -> addDay 30

    (7,True) -> addDay 120
    _ -> curTime -- error

getCheckAnswer :: CheckAnswer -> HandlerM CheckAnswerResult
getCheckAnswer (CheckAnswer reading alt) = do
  pPrint alt
  let  f (c,t) = (,) <$> pure c <*> getKana t
  kanaAlts <- mapM (mapM f) alt
  pPrint kanaAlts
  return $ checkAnswerInt reading kanaAlts

checkAnswerInt :: Text -> [[(Double, Text)]] -> CheckAnswerResult
checkAnswerInt reading kanaAlts =
  case elem reading kanas of
    True -> AnswerCorrect
    False -> AnswerIncorrect "T"
  where
    kanas = mconcat $ map (map snd) kanaAlts

-- Convert Kanji to furigana (hiragana)
getKana :: Text -> HandlerM (Text)
getKana t = do
  m <- asks mecab
  nodes <- liftIO $ parseToNodes m t
  let feats = map nodeFeature nodes
      -- get the last part of the mecab output, this will include '*'
      readings :: [Text]
      readings = catMaybes $ map (headMay . reverse . (T.splitOn ",")) feats
      -- convert valid characters to hiragana
      f = map (toHiragana . toRoma) $ filter isJP readings

  return $ mconcat f
