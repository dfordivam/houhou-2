{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DBInterface
  ( openDB
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
  ) where

import Model
import DB

import Protolude
import Database.Beam
import Database.SQLite.Simple
import Database.Beam.Sqlite
import qualified Data.Set as Set

import Control.Lens

openDB = open "KanjiDatabase.sqlite"

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
  liftIO $ print ks
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
  liftIO $ print ks
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

getKanjiMeaning :: KanjiId -> DBMonad (Maybe KanjiMeaning)
getKanjiMeaning kId =
  headMay <$> selectListQuery query
  where
    query = limit_ 1 $
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
