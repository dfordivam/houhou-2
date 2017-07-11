{-# LANGUAGE OverloadedStrings #-}
import Data.Time
import Data.Time.Clock
import Database.SQLite.Simple
import           Data.Int (Int8, Int16, Int32, Int64)
import Data.ByteString
import qualified Data.Text
import Data.Text (Text)
import Control.Monad
-- The CreationDate column is tricky to parse as it is marked as bigint and CURRENT_TIMESTAMP
-- so it has either a string/time like 2016-05-30 13:57:28
-- or it can be ticks 636036572893102292
-- So here I am converting all the ticks and bigint to a unified UTCTime

main = do
  -- conn <- open ":memory:"

  -- let timestr = "2012-08-20 20:19:58"
  --     time    = read timestr :: UTCTime
  -- execute_ conn "CREATE TABLE time (t TIMESTAMP)"
  -- execute conn "INSERT INTO time (t) VALUES (?)" (Only time)
  -- [Only t] <- query_ conn "SELECT * FROM time" :: IO [Only UTCTime]
  -- print t

  conn <- open "SrsDatabase.sqlite"
  execute_ conn createTmpTable
  Prelude.putStrLn "Creating temporary table"
  execute_ conn createNewTable
  Prelude.putStrLn "Creating new table"

  execute_ conn insertInTmpTable
  Prelude.putStrLn "Inserting in temporary table"

  val <- query_ conn "SELECT ID,CreationDate, NextAnswerDate,\
       \SuspensionDate, LastUpdateDate \
       \FROM SrsEntrySetTmp" :: IO [(Int, Text,Maybe Int, Maybe Int, Maybe Int)]

  Prelude.putStrLn "Selecting data from temporary table"
  let newVal = fmap fixDate val

  forM newVal (insertInTable conn)
  Prelude.putStrLn "Insert modified data in new table"

  execute_ conn dropOldTable
  execute_ conn dropTmpTable
  Prelude.putStrLn "Dropping old and temporary tables"
  Prelude.putStrLn "Done!"

fixDate :: (Int, Text, Maybe Int, Maybe Int, Maybe Int)
  -> (Int, UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime)
fixDate (i, c, n, s, l) =
  (i, creationTime
  , getTimeFromTicks <$> n
  , getTimeFromTicks <$> s
  , getTimeFromTicks <$> l)
  where
    creationTime =
      case (Data.Text.any (== '-') c) of
        True -> read (Data.Text.unpack c) :: UTCTime
        False -> getTimeFromTicks (read (Data.Text.unpack c) :: Int)

getTimeFromTicks :: Int -> UTCTime
getTimeFromTicks v = addUTCTime (sec) startTime
  where
    -- https://msdn.microsoft.com/en-us/library/system.datetime.ticks(v=vs.110).aspx
    -- The value of this property represents the number of
    -- 100-nanosecond intervals that have elapsed since
    -- 12:00:00 midnight, January 1, 0001
    -- (0:00:00 UTC on January 1, 0001, in the Gregorian calendar)
    sec = fromIntegral $ floor $ ((fromIntegral v)/ (10000000.0))

    startDate = fromGregorian 1 1 1 --
    startTime = UTCTime startDate 0


createTmpTable =
    "create table SrsEntrySetTmp (\
      \[ID] integer NOT NULL PRIMARY KEY AUTOINCREMENT,\
      \[CreationDate] [nvarchar(100)] NOT NULL,\
      \[NextAnswerDate] BIGINT,\
      \[Meanings] [nvarchar(300)] NOT NULL,\
      \[Readings] [nvarchar(100)] NOT NULL,\
      \[CurrentGrade] smallint NOT NULL DEFAULT 0,\
      \[FailureCount] integer NOT NULL DEFAULT 0,\
      \[SuccessCount] integer NOT NULL DEFAULT 0,\
      \[AssociatedVocab] [nvarchar(100)],\
      \[AssociatedKanji] [nvarchar(10)],\
      \[MeaningNote] [nvarchar(1000)],\
      \[ReadingNote] [nvarchar(1000)],\
      \[SuspensionDate] BIGINT,\
      \[Tags] [nvarchar(300)],\
      \[LastUpdateDate] BIGINT,\
      \[IsDeleted] BOOLEAN NOT NULL DEFAULT false\
      \)"

createNewTable =
    "create table SrsEntrySetNew (\n\
      \[ID] integer NOT NULL PRIMARY KEY AUTOINCREMENT,\n\
      \[CreationDate] DATETIME NOT NULL,\n\
      \[NextAnswerDate] DATETIME,\n\
      \[Meanings] [nvarchar(300)] NOT NULL,\n\
      \[Readings] [nvarchar(100)] NOT NULL,\n\
      \[CurrentGrade] smallint NOT NULL DEFAULT 0,\n\
      \[FailureCount] integer NOT NULL DEFAULT 0,\n\
      \[SuccessCount] integer NOT NULL DEFAULT 0,\n\
      \[AssociatedVocab] [nvarchar(100)],\n\
      \[AssociatedKanji] [nvarchar(10)],\n\
      \[MeaningNote] [nvarchar(1000)],\n\
      \[ReadingNote] [nvarchar(1000)],\n\
      \[SuspensionDate] DATETIME,\n\
      \[Tags] [nvarchar(300)],\n\
      \[LastUpdateDate] DATETIME,\n\
      \[IsDeleted] BOOLEAN NOT NULL DEFAULT false\n\
      \)"

insertInTmpTable = "INSERT INTO SrsEntrySetTmp (ID, CreationDate, NextAnswerDate, Meanings, Readings, CurrentGrade, FailureCount, SuccessCount, AssociatedVocab, AssociatedKanji, MeaningNote, ReadingNote, SuspensionDate, Tags, LastUpdateDate, IsDeleted)\
    \SELECT ID, CreationDate, NextAnswerDate, Meanings, Readings, CurrentGrade, FailureCount, SuccessCount, AssociatedVocab, AssociatedKanji, MeaningNote, ReadingNote, SuspensionDate, Tags, LastUpdateDate, IsDeleted FROM SrsEntrySet;"

insertInTable conn (i, c, n, s, l) = execute conn "INSERT INTO SrsEntrySetNew (ID\
 \, CreationDate , NextAnswerDate , Meanings , Readings\
 \, CurrentGrade , FailureCount , SuccessCount , AssociatedVocab\
 \, AssociatedKanji , MeaningNote , ReadingNote , SuspensionDate\
 \, Tags , LastUpdateDate , IsDeleted)\
 \ SELECT\
 \ ?, ?, ?, SrsEntrySetTmp.Meanings , SrsEntrySetTmp.Readings\
 \, SrsEntrySetTmp.CurrentGrade , SrsEntrySetTmp.FailureCount , SrsEntrySetTmp.SuccessCount , SrsEntrySetTmp. AssociatedVocab\
 \, SrsEntrySetTmp.AssociatedKanji , SrsEntrySetTmp.MeaningNote , SrsEntrySetTmp.ReadingNote , ?\
 \, SrsEntrySetTmp.Tags , ?, SrsEntrySetTmp.IsDeleted\
 \ FROM SrsEntrySetTmp WHERE ID = ?;" (i,c,n,s,l,i)

dropOldTable = "drop table SrsEntrySet"
dropTmpTable = "drop table SrsEntrySetTmp"

 -- SELECT ID, CreationDate, NextAnswerDate, Meanings, Readings, CurrentGrade, FailureCount, SuccessCount, AssociatedVocab, AssociatedKanji, MeaningNote, ReadingNote, SuspensionDate, Tags, LastUpdateDate, IsDeleted FROM SrsEntrySet;
