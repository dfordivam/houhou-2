module HandlerTests where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple

import Handlers
import Message
import Common
import Control.Monad.RWS
import Database.SQLite.Simple

tests :: (IO Connection) -> TestTree
tests getConn = testGroup "Handler Tests"
                  [kanjiFilterTest getConn]

runHandler :: (IO Connection) -> Handler a -> IO a
runHandler getConn h = do
  let initState = HandlerState [] 20
  conn <- getConn
  (a, s, w) <- runRWST h conn initState
  return a

kanjiFilterTest :: (IO Connection) -> TestTree
kanjiFilterTest getConn =
  let runH = runHandler getConn
  in testGroup "Kanji Filter"
  [testCase "Empty Filter Query" $ do
     let req = KanjiFilter "" ("", OnYomi) []
         chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')

     (KanjiFilterResult resp rads) <- runH $ getKanjiFilterResult req

     [] @=? rads
     mapM_ (uncurry chk) (zip mostUsedKanjis resp)

  , testCase "Search for valid Kanjis" $ do
     let req = KanjiFilter "先皿 問 題" ("", OnYomi) []
         chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')
         exp = [(KanjiT "先", MeaningT "before")
               ,(KanjiT "皿", MeaningT "dish")
               ,(KanjiT "問", MeaningT "question")
               ,(KanjiT "題", MeaningT "topic")]

     (KanjiFilterResult resp rads) <- runH $ getKanjiFilterResult req

     (length exp) @=? (length resp)
     mapM_ (uncurry chk) (zip exp resp)

  , testCase "Ignore characters apart from Kanjis" $ do
     let req = KanjiFilter "ab 先,34@&皿 あ「＊。￥ 問 題" ("", OnYomi) []
         chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')
         exp = [(KanjiT "先", MeaningT "before")
               ,(KanjiT "皿", MeaningT "dish")
               ,(KanjiT "問", MeaningT "question")
               ,(KanjiT "題", MeaningT "topic")]

     (KanjiFilterResult resp rads) <- runH $ getKanjiFilterResult req

     (length exp) @=? (length resp)
     mapM_ (uncurry chk) (zip exp resp)

  , testCase "Test OnYomi Filter" $ do
     let req = KanjiFilter "" ("にん",OnYomi) []
         chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')
         exp = [(KanjiT "任", MeaningT "responsibility")
               ,(KanjiT "忍", MeaningT "endure")
               ,(KanjiT "認", MeaningT "acknowledge")]

     (KanjiFilterResult resp rads) <- runH $ getKanjiFilterResult req

     (length exp) @=? (length resp)
     mapM_ (uncurry chk) (zip exp resp)

  , testCase "Test KunYomi Filter" $ do
     let req = KanjiFilter "" ("%がた",KonYumi) []
         chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')
         exp = [(KanjiT "田", MeaningT "rice field")
               ,(KanjiT "咫", MeaningT "short")]

     (KanjiFilterResult resp rads) <- runH $ getKanjiFilterResult req

     pPrint resp
     (length exp) @=? (length resp)
     mapM_ (uncurry chk) (zip exp resp)

  , testCase "Test KunYomi Filter with kanji input" $ do
     let req = KanjiFilter "心" ("こころ",KonYumi) []
         chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')
         exp = [(KanjiT "田", MeaningT "rice field")
               ,(KanjiT "咫", MeaningT "short")]

     (KanjiFilterResult resp rads) <- runH $ getKanjiFilterResult req

     pPrint resp
     (length exp) @=? (length resp)
     mapM_ (uncurry chk) (zip exp resp)
  ]

mostUsedKanjis =
  [ (KanjiT "人", MeaningT "person")
  , (KanjiT "一", MeaningT "one")
  , (KanjiT "見", MeaningT "see")
  , (KanjiT "出", MeaningT "exit")
  , (KanjiT "言", MeaningT "say")
  , (KanjiT "子", MeaningT "child")
  , (KanjiT "大", MeaningT "large")
  , (KanjiT "思", MeaningT "think")
  , (KanjiT "彼", MeaningT "he")
  , (KanjiT "手", MeaningT "hand")
  , (KanjiT "分", MeaningT "part")
  , (KanjiT "女", MeaningT "woman")
  , (KanjiT "上", MeaningT "above")
  , (KanjiT "気", MeaningT "spirit")
  , (KanjiT "中", MeaningT "in")
  , (KanjiT "間", MeaningT "interval")
  , (KanjiT "日", MeaningT "day")
  , (KanjiT "自", MeaningT "oneself")
  , (KanjiT "二", MeaningT "two")
  , (KanjiT "行", MeaningT "going")
  ]
