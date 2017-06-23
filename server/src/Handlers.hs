{-# LANGUAGE TemplateHaskell #-}
module Handlers where

import Protolude
import Control.Lens.TH

type Handler = RWST Connection () HandlerState IO

data HandlerState = HandlerState
  { _kanjiSearchResult :: [DB.Kanji]
  , _kanjiDisplayCount :: Int
  }
makeLenses ''HandlerState

runDB :: (DBMonad a) -> Handler a
runDB f = do
  conn <- asks
  let res = runReaderT f conn
  return res

-- Pagination,
getKanjiFilterResult :: KanjiFilter -> Handler KanjiFilterResult
getKanjiFilterResult (KanjiFilter inpTxt (filtTxt, filtType) rads) = do
  let uniqKanji = uniq $ sort $ getKanji inpTxt

  -- inpTxt in empty
  -- but filt and rads are non empty

  -- Filt, rads, inpText empty, then return most used kanji list
  -- Preferable sequence of search is
  -- 1. Input text - This will limit the number of kanji to the length of text
  -- 2. Reading filter
  -- 3. Radicals
  let
    fun
      | (null inpTxt) && (null filtTxt)
        && (null rads) ->
          liftIO $ getMostUsedKanjis
      | (null inpTxt) && (null filtTxt) ->
          liftIO $ getKanjiByRadical $ coerce rads
      | (null inpTxt) && (null rads) ->
        searchKanjiByReading uniqKanji on ku na
      | (null filtTxt) && (null rads) ->
        searchKanji uniqKanji
      | (null rads)
        searchKanjiByReading uniqKanji on ku na
      | (null filtTxt)
        ks <- searchKanji uniqKanji
        filterKanjiByRadical rads ks
      | otherwise -> runDB $
        --  | (null inpTxt)
        ks <- searchKanjiByReading uniqKanji on ku na
        filterKanjiByRadical rads ks

    (on,ku,na) = case filtType of
      OnYomiT -> (filtTxt,"","")
      KunYomiT -> ("",filtTxt,"")
      NanoriT -> ("","",filtTxt)



  kanjiList <- fun


  validRadicals <- runDB $ getValidRadicalList kanjiList


  state
    (\s ->((), s ^. kanjiSearchResult ~ kanjiList
          & kanjiDisplayCount ~ kanjiResultCount))

  let displayKanjiList = take kanjiResultCount kanjiList

  meanings <- runDB $ getKanjiMeaning displayKanjiList
  let l = map convertKanji $ zip displayKanjiList meanings
      convertKanji
        :: (DB.Kanji, DB.Meaning)
        -> (KanjiId, KanjiT, RankT, MeaningT)
      convertKanji (k,m) =
      r = map RadicalId validRadicals
  return $ KanjiFilterResult l r
