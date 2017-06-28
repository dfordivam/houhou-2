module HandlerTests where

import Test.Tasty
import Test.Tasty.HUnit

import Handlers
import Message
import Common
import DBInterface
import Control.Monad.RWS

tests :: TestTree
tests = testGroup "Handler Tests"
                  [ kanjiFilterTest]

runHandler :: Handler a -> IO a
runHandler h = do
  let initState = HandlerState [] 20
  conn <- openDB
  (a, s, w) <- runRWST h conn initState
  return a

kanjiFilterTest :: TestTree
kanjiFilterTest = testGroup "Kanji Filter"
  [testCase "Check" $
    True @=? True
  , testCase "2" $
    do
      let req = KanjiFilter "" ("", OnYomi) []
          exp = KanjiFilterResult [] []
          chk (k,m) (_,k',_,Just m') = (k,m) @=? (k' ,m')

      (KanjiFilterResult resp rads) <- runHandler $ getKanjiFilterResult req

      [] @=? rads
      mapM_ (uncurry chk) (zip mostUsedKanjis resp)
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
