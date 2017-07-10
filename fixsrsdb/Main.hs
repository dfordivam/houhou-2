{-# LANGUAGE OverloadedStrings #-}
import Data.Time (UTCTime, Day)
import Database.SQLite.Simple

main = do
  conn <- open ":memory:"

  let timestr = "2012-08-20 20:19:58"
      time    = read timestr :: UTCTime
  execute_ conn "CREATE TABLE time (t TIMESTAMP)"
  execute conn "INSERT INTO time (t) VALUES (?)" (Only time)
  [Only t] <- query_ conn "SELECT * FROM time" :: IO [Only UTCTime]
  print t
  putStrLn "halo"
