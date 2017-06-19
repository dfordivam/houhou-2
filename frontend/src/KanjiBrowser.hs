module KanjiBrowser where

import Reflex.Dom
import Message
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Data.Monoid

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Reflex.Dom.Contrib.WithWebSocket.Class

kanjiBrowseWidget = do

  rec
    let
      (validRadicalsEv,kanjiListEv)
        = splitE (\KanjiFilterResult a b -> (a,b)) <$> filterResultEv
    filterEv <- kanjiFilterWidget validRadicalsEv

    filterResultEv <- getWebSocketResponse (DoKanjiFilter <$> filterEv)

    kanjiSelectionEv <- kanjiListWidget kanjiListEv

    kanjiDetailsEv <- getWebSocketResponse
      (GetKanjiDetails <$> kanjiSelectionEv)

    kanjiDetailsWidget kanjiDetailsEv

  return ()
-- Widget to show kanjifilter

kanjiFilterWidget :: Event t [RadicalId] -> m (Event t KanjiFilter)
kanjiFilterWidget = do
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

radicalMatrix :: Event t [RadicalId] -> m (Dynamic t [RadicalId])
radicalMatrix evValid = do

  let
    renderMatrix = do
      el "ul" $ mapM showRadical (toList radicalTable)

    showRadical (i,r) =
      let valid = member i <$> validRadicals
          sel = member i <$> selectedRadicals
          -- (Valid, Selected)
          cl (False,_) = "secondary label"
          cl (True, True) = "primary label"
          cl _ = ""
          attr = (\c -> ("class" =: c)) <$>
                   (cl <$> combineDyn valid sel)

      (e,_) <- el' "li" $
        elDynAttr "span" attr $ text (show r)
      let ev = attachPromptlyDynWithMaybe f valid
                 (domEvent Click e)
          f (True,_) = Just ()
          f _ = Nothing
      return (i <$ ev)

  validRadicals :: Dynamic t (Set RadicalId)
  validRadicals <- holdDyn (keys radicalTable) evValid

  ev <- renderMatrix validRadicals

  selectedRadicals :: Dynamic t (Set RadicalId)
  selectedRadicals <- foldDyn h empty ev
  let h s i = if member i s then delete i s else insert i s

  return $ toList <$> selectedRadicals


kanjiListWidget :: Event t KanjiList -> m (Event t KanjiId)
kanjiListWidget listEv = do
  let kanjiTable itms = do
        el "table" $ do
          let listItem itm@(i, _, _, _) = do
                (e, _) <- el' "tr" $ el "td" $ show itm
                return (i <$ domEvent Click e)
          mapM listItem itms
  leftmost =<< widgetHold (return never) (kanjiTable <$> listEv)
  -- show list

kanjiDetailsWidget :: Event t KanjiSelectionDetails -> m ()
kanjiDetailsWidget (KanjiSelectionDetails k v) = do
  kanjiDetailWindow k
  vocabListWindow v

kanjiDetailWindow :: KanjiDetails -> m ()
kanjiDetailWindow k = divClass "" $ text (show k)
vocabListWindow :: VocabDisplay -> m ()
vocabListWindow v = divClass "" $ text (show v)
