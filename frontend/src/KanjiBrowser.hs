{-# LANGUAGE RecursiveDo #-}
module KanjiBrowser where

import Protolude
import Reflex.Dom
import Message
import Common
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Aeson
import Control.Monad.Primitive

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Reflex.WebSocket.WithWebSocket.Base
import Reflex.WebSocket.WithWebSocket.Shared

kanjiBrowseWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
kanjiBrowseWidget = do

  ev <- getPostBuild
  let
    filterEv = never
    filterEvWithPostBuild = leftmost [KanjiFilter "" ("", OnYomi) [] <$ ev,
                                      filterEv]

  filterResultEv <- getWebSocketResponse filterEvWithPostBuild

  let
    (kanjiListEv, validRadicalsEv)
      = splitE $ (\(KanjiFilterResult a b) -> (a,b)) <$> filterResultEv

  filterEv1 <- kanjiFilterWidget validRadicalsEv

  kanjiSelectionEv <- kanjiListWidget kanjiListEv

  kanjiDetailsEv <- getWebSocketResponse
    (GetKanjiDetails <$> kanjiSelectionEv)

  kanjiDetailsWidget kanjiDetailsEv

  return ()
-- Widget to show kanjifilter

kanjiFilterWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t [RadicalId]
  -> WithWebSocketT Message.AppRequest t m (Event t KanjiFilter)
kanjiFilterWidget validRadicalsEv = do
  sentenceTextArea <- textArea def
  readingTextInput <- textInput def
  readingSelectionDropDown <- dropdown
    KonYumi
    (constDyn ((KonYumi =: "Kunyomi") <> (OnYomi =: "Onyomi ")))
    def

  selectedRadicals <- radicalMatrix validRadicalsEv
  let
    kanjiFilter = KanjiFilter <$> (value sentenceTextArea)
          <*> (zipDynWith (,) (value readingTextInput)
              (value readingSelectionDropDown))
          <*> selectedRadicals

  return $ updated kanjiFilter

radicalMatrix
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t [RadicalId] -> m (Dynamic t [RadicalId])
radicalMatrix evValid = do

  -- validRadicals :: Dynamic t (Set RadicalId)
  validRadicals <- holdDyn (Map.keysSet radicalTable) (Set.fromList <$> evValid)

  let
    renderMatrix = do
      el "ul" $ mapM showRadical (Map.toList radicalTable)

    showRadical (i,(RadicalDetails r)) = do
      let valid = Set.member i <$> validRadicals
          sel = pure False
            -- Set.member i <$> selectedRadicals
          -- (Valid, Selected)
          cl (False,_) = "secondary label"
          cl (True, True) = "primary label"
          cl _ = ""
          attr = (\c -> ("class" =: c)) <$>
                   (cl <$> zipDyn valid sel)

      (e,_) <- el' "li" $
        elDynAttr "span" attr $ text (show r)
      let ev = attachPromptlyDynWithMaybe f valid
                 (domEvent Click e)
          f True _ = Just ()
          f _ _ = Nothing
      return (i <$ ev)

  ev <- renderMatrix

  let
    h :: RadicalId -> Set RadicalId -> Set RadicalId
    h i s = if Set.member i s then Set.delete i s else Set.insert i s
  selectedRadicals <- foldDyn h Set.empty (leftmost ev)

  return $ toList <$> selectedRadicals


kanjiListWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t KanjiList -> m (Event t KanjiId)
kanjiListWidget listEv = do
  let kanjiTable itms = do
        el "table" $ do
          let listItem itm@(i, _, _, _) = do
                (e, _) <- el' "tr" $ el "td" $ display (pure itm)
                return (i <$ domEvent Click e)
          evs <- mapM listItem itms
          return $ leftmost evs
  d <- widgetHold (return never) (kanjiTable <$> listEv)
  let e = switchPromptlyDyn d
  return e
  -- show list

kanjiDetailsWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t KanjiSelectionDetails -> m ()
kanjiDetailsWidget ev = do
  let f (KanjiSelectionDetails k v) = do
        display (pure k)
        display (pure v)
        return ()
  void $ widgetHold (return ()) (f <$> ev)

kanjiDetailWindow :: KanjiDetails -> m ()
kanjiDetailWindow k = undefined
vocabListWindow :: VocabDisplay -> m ()
vocabListWindow v = undefined
