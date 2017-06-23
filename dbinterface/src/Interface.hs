module Interface where

import Model
import DB

import Protolude
import Database.Beam
import Database.SQLite.Simple
import Database.Beam.Sqlite

import Control.Lens

openDB = open "KanjiDatabase.sqlite"

printResult :: (Show a) => a -> IO ()
printResult a = putStrLn $ (show a :: Text)

type DBMonad = ReaderT Connection IO

searchKanji :: [Text] -> DBMonad [Kanji]
searchKanji c = do
  conn <- ask
  withDatabaseDebug putStrLn conn (runSelectReturningList $ select $ query)
  where
    query = do
      k <- all_ (_jmdictKanji jmdictDb)
      guard_ (k ^. kanjiCharacter ==. val_ c)
      return k

searchKanjiByReading
  :: [Text]
  -> Text
  -> Text
  -> Text
  -> DBMonad [Kanji]
searchKanjiByReading ks on ku na = do

filterKanjiByRadical :: [RadicalId] -> [KanjiId] -> DBMonad [KanjiId]

getKanjiMeaning :: [KanjiId] -> DBMonad [Meaning]
getKanjiMeaning

getMostUsedKanjis :: DBMonad ([Kanji])

getKanjiByRadical :: [RadicalId] -> DBMonad [Kanji]
