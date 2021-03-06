{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DBInterface
  ( openKanjiDB
  , openSrsDB
  , close
  , DBMonad
  , primaryKey
  , searchKanji
  , searchKanjiByReading
  , filterKanjiByRadical
  , getMostUsedKanjis
  , getKanjiMeaning
  , getValidRadicalList
  , getKanji
  , getVocab
  , getRelatedVocab
  , getVocabMeaning
  , filterVocab
  , getSrsEntries
  , updateSrsEntry
  ) where

import Model
import DB

import Protolude
import Database.Beam
import Database.SQLite.Simple
import Database.Beam.Sqlite
import qualified Data.Set as Set
import Data.Time (LocalTime)
import qualified Data.Text as T

import Control.Lens

openKanjiDB = open "KanjiDatabase.sqlite"
openSrsDB = open "SrsDatabase.sqlite"

printResult :: (Show a) => a -> IO ()
printResult a = putStrLn $ (show a :: Text)

type DBMonad = ReaderT Connection IO

selectListQuery query = do
  conn <- ask
  liftIO $
    withDatabase conn (runSelectReturningList $ select $ query)
    -- withDatabaseDebug putStrLn conn (runSelectReturningList $ select $ query)

searchKanji :: [Text] -> DBMonad [Kanji]
searchKanji inp = do
  ks <- forM inp (\i -> (selectListQuery $ query i))
  return $ concat ks
  where
    query i =
      filter_ (\k -> (k ^. kanjiCharacter) ==. val_ i) $
        (all_ (_jmdictKanji jmdictDb))

searchKanjiByReading
  :: [Text]
  -> Text
  -> Text
  -> Text
  -> DBMonad [Kanji]
searchKanjiByReading [] on ku na = do
  ks <- selectListQuery query
  return ks
  where
    likeMaybe_ f v = maybe_ (val_ False) ((flip like_) (val_ (v <> "%"))) f
    query =
      filter_
        (\k ->
            (((k ^. kanjiKunyomi) `likeMaybe_` ku) ||.
            ((k ^. kanjiOnyomi) `likeMaybe_` on) ||.
            ((k ^. kanjiNanori) `likeMaybe_` na))) $
      (all_ (_jmdictKanji jmdictDb))

searchKanjiByReading inp on ku na = do
  ks <- forM inp (\i -> (selectListQuery $ query i))
  return $ concat ks
  where
    likeMaybe_ f v = maybe_ (val_ False) ((flip like_) (val_ (v <> "%"))) f
    query i =
      filter_
        (\k ->
           ((k ^. kanjiCharacter) ==. val_ i) &&.
            (((k ^. kanjiKunyomi) `likeMaybe_` ku) ||.
            ((k ^. kanjiOnyomi) `likeMaybe_` on) ||.
            ((k ^. kanjiNanori) `likeMaybe_` na))) $
      (all_ (_jmdictKanji jmdictDb))

filterKanjiByRadical :: [RadicalId] -> [KanjiId] -> DBMonad [KanjiId]
filterKanjiByRadical [] ks = return ks
filterKanjiByRadical (r1:rs) ks = do
  Set.toList <$> if null ks
    then loop rs =<< g r1
    else loop (r1:rs) (Set.fromList ks)
  where
    loop :: [RadicalId] -> Set KanjiId -> DBMonad (Set KanjiId)
    loop [] rSet = return $ rSet
    loop (r1:rs) rSet = do
      ks <- Set.intersection rSet <$> g r1
      if null ks
        then return Set.empty
        else loop rs ks

    g :: RadicalId -> DBMonad (Set KanjiId)
    g rId = do
      Set.fromList <$> selectListQuery query
      where query = (\kr -> kr ^. kanjiRadicalKanji) <$>
              filter_ (\r -> r ^. kanjiRadicalRadical ==. val_ rId)
                (all_ (_jmdictKanjiRadical jmdictDb))
    -- f r1 r2 = do
    --   let query = intersect_ (g r1) (g r2)
    --       g rId = fmap (\r -> r ^. kanjiRadicalKanji) $
    --                 (filter_ (\r -> r ^. kanjiRadicalRadical ==. val_ rId)
    --                 (all_ (_jmdictKanjiRadical jmdictDb)))
    --   selectListQuery query

getKanjiMeaning :: KanjiId -> DBMonad [KanjiMeaning]
getKanjiMeaning kId =
  selectListQuery query
  where
    query =
      filter_ (\k -> (k ^. kanjiMeaningKanji) ==. val_ kId) $
        (all_ (_jmdictKanjiMeaning jmdictDb))

getMostUsedKanjis :: DBMonad ([Kanji])
getMostUsedKanjis =
  getKanji =<< selectListQuery query
  where
    getKanji ks = return $ sortBy f ks
    f i1 i2 = g (i1 ^. kanjiMostUsedRank) (i2 ^. kanjiMostUsedRank)
    g (Just r1) (Just r2) = compare r1 r2
    query =
      filter_ (isJust_ . _kanjiMostUsedRank)$
        (all_ (_jmdictKanji jmdictDb))

getValidRadicalList :: [KanjiId] -> DBMonad [RadicalId]
getValidRadicalList ks = do
  let
      query k = map _kanjiRadicalRadical $
              filter_ (\r -> r ^. kanjiRadicalKanji ==. val_ k)
                (all_ (_jmdictKanjiRadical jmdictDb))
  rs <- forM ks (\k -> selectListQuery (query k))
  return $ (ordNub . concat) rs


getKanji :: [KanjiId] -> DBMonad [Kanji]
getKanji ks = do
  conn <- ask
  let query k = lookup (_jmdictKanji jmdictDb) k
      fun k = liftIO $
        withDatabaseDebug putStrLn conn $ runSelectReturningOne (query k)
  rs <- forM ks fun
  return $ catMaybes rs

getVocab :: [VocabId] -> DBMonad [Vocab]
getVocab ks = do
  conn <- ask
  let query k = lookup (_jmdictVocab jmdictDb) k
      fun k = liftIO $
        withDatabaseDebug putStrLn conn $ runSelectReturningOne (query k)
  rs <- forM ks fun
  return $ catMaybes rs

getRelatedVocab :: [KanjiId] -> DBMonad [VocabId]
getRelatedVocab ks = do
  rs <- forM ks (\k -> (selectListQuery $ query k))
  return $ ordNub $ concat rs
  where
      query k = map _vocabKanjiVocab $
              filter_ (\r -> r ^. vocabKanjiKanji ==. val_ k)
                (all_ (_jmdictVocabKanji jmdictDb))

getVocabMeaning :: VocabId -> DBMonad [VocabMeaning]
getVocabMeaning v = do
  mIds <- selectListQuery query
  conn <- ask
  let
    fun k = liftIO $
        withDatabaseDebug putStrLn conn $
          runSelectReturningOne (lookup (_jmdictVocabMeaning jmdictDb) k)
  ms <- forM mIds fun
  return $ catMaybes ms
  where
    query = map _vocabVocabMeaningMeaning $
      filter_ (\k -> (k ^. vocabVocabMeaningVocab) ==. val_ v) $
        (all_ (_jmdictVocabVocabMeaning jmdictDb))

filterVocabByReading :: Text -> DBMonad [VocabId]
filterVocabByReading r = do
  selectListQuery query
  where
    likeMaybe_ f v = maybe_ (val_ False) ((flip like_) (val_ (v <> "%"))) f
    query = map primaryKey $
      filter_
        (\v ->
            (((v ^. vocabKanjiWriting) `likeMaybe_` r) ||.
            ((v ^. vocabKanaWriting) `like_` val_ (r <> "%")))) $
        (all_ (_jmdictVocab jmdictDb))

filterVocabByMeaning :: Text -> DBMonad [VocabId]
filterVocabByMeaning m = do
  ms <- selectListQuery query
  vs <- forM ms (\i -> selectListQuery $ query2 i)
  return $ ordNub $ concat vs
  where
    query = map primaryKey $ limit_ 50 $
      filter_
        (\v ->
            (((v ^. vocabMeaningMeaning) `like_` val_ (m <> "%")) ||.
            ((v ^. vocabMeaningMeaning) `like_` val_ ("% " <> m <> "%")))) $
        (all_ (_jmdictVocabMeaning jmdictDb))

    query2 i = map _vocabVocabMeaningVocab $
      filter_ (\k -> (k ^. vocabVocabMeaningMeaning) ==. val_ i) $
        (all_ (_jmdictVocabVocabMeaning jmdictDb))

filterVocab :: Text -> Text -> DBMonad [VocabId]
filterVocab r m
  | T.null r && T.null m = return []
  | T.null r = filterVocabByMeaning m
  | T.null m = filterVocabByReading r
  | otherwise = do
    vs <- filterVocabByMeaning m
    vs2 <-filterVocabByReading r
    return $ Set.toList $ Set.intersection (Set.fromList vs) (Set.fromList vs2)

getSrsEntries :: DBMonad [SrsEntry]
getSrsEntries = selectListQuery (all_ (srsDbTable srsDb))

updateSrsEntry ::
  SrsEntry -> DBMonad ()
updateSrsEntry s = do
  conn <- ask
  liftIO $
    void $ withDatabaseDebug putStrLn conn $
      runUpdate $
        save (srsDbTable srsDb) s
