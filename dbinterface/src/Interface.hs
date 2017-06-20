module Interface where

import Model
import DB
import DBInstances

import Protolude
import Database.Beam
import Database.SQLite.Simple
import Database.Beam.Sqlite

import Control.Lens

openDB = open "KanjiDatabase.sqlite"

printResult :: (Show a) => a -> IO ()
printResult a = putStrLn $ (show a :: Text)

searchKanji :: Connection -> Text -> IO ([Kanji])
searchKanji conn c =
  withDatabaseDebug putStrLn conn (runSelectReturningList $ select $ query)
  where
    query = do
      k <- all_ (_jmdictKanji jmdictDb)
      guard_ (k ^. kanjiCharacter ==. val_ c)
      return k
