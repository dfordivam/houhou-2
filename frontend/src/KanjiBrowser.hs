{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
module KanjiBrowser where

import Protolude hiding (link)
import Reflex.Dom
import Message
import Common
import Radicals
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Aeson
import Control.Monad.Primitive

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
data VisibleWidget = KanjiFilterVis | KanjiDetailPageVis
  deriving (Eq)

kanjiBrowseWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
kanjiBrowseWidget = divClass "ui internally celled grid" $ divClass "row" $ do

  ev <- getPostBuild

  rec
    let
      filterEvWithPostBuild = leftmost [def <$ ev,
                                        filterEv]

    filterResultEv <- getWebSocketResponse filterEvWithPostBuild

    let
      (kanjiListEv, validRadicalsEv)
        = splitE $ (\(KanjiFilterResult a b) -> (a,b)) <$> filterResultEv

      f (KanjiFilter "" (Filter "" _ "") []) _ = Map.keys radicalTable
      f _ r = r

      validRadicals = attachPromptlyDynWith f filterDyn validRadicalsEv

    filterDyn <- holdDyn def filterEv


    filterEv <- divClass "twelve wide column" $ do

      rec
        let visEv = leftmost [KanjiFilterVis <$ closeEv, KanjiDetailPageVis <$ kanjiDetailsEv]
        vis <- holdDyn KanjiFilterVis visEv
        let

        -- Show either the filter options or the kanji details page
        filterEv <- handleVisibility KanjiFilterVis vis $
          kanjiFilterWidget validRadicals

        maybeKanjiDetailsEv <- getWebSocketResponse
          ((flip GetKanjiDetails) def <$> kanjiSelectionEv)

        closeEv <- handleVisibility KanjiDetailPageVis vis $ do
          l <- linkClass "Close Details Page" "ui top attached button"
          return (_link_clicked l)

        let kanjiDetailsEv = fmapMaybe identity maybeKanjiDetailsEv
        handleVisibility KanjiDetailPageVis vis $
          kanjiDetailsWidget kanjiDetailsEv

      return filterEv

    kanjiSelectionEv <-
      divClass "four wide column" $ do
        kanjiListWidget kanjiListEv

  return ()
-- Widget to show kanjifilter

handleVisibility
  :: (PostBuild t m, DomBuilder t m, Eq a)
  => a -> Dynamic t a -> m v -> m v
handleVisibility v dv mv = elDynAttr "div" (f <$> dv) mv
  where
    f dv =
      if v == dv
        then Map.empty
        else ("style" =: "display: none;")

kanjiFilterWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t [RadicalId]
  -> WithWebSocketT Message.AppRequest t m (Event t KanjiFilter)
kanjiFilterWidget validRadicalsEv = do
  sentenceTextArea <- divClass "row" $ textArea def

  (readingTextInput, readingSelectionDropDown, meaningTextInput)
    <- divClass "row" $ do
    t <- textInput def
    d <- dropdown
      KunYomi
      (constDyn ((KunYomi =: "Kunyomi") <> (OnYomi =: "Onyomi ")))
      def
    m <- textInput def
    return (t,d,m)

  let filterDyn = Filter <$> (value readingTextInput)
                    <*> (value readingSelectionDropDown)
                    <*> (value meaningTextInput)

  selectedRadicals <- radicalMatrix validRadicalsEv
  let
    -- selectedRadicals = constDyn []
    kanjiFilter = KanjiFilter <$> (value sentenceTextArea)
          <*> filterDyn
          <*> selectedRadicals

  return $ updated kanjiFilter

radicalMatrix
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t [RadicalId] -> m (Dynamic t [RadicalId])
radicalMatrix evValid = do

  -- validRadicals :: Dynamic t (Set RadicalId)
  evDelayed <- delay 1 evValid
  validRadicals <- holdDyn (Map.keysSet radicalTable) (Set.fromList <$> evDelayed)

  rec
    let
      renderMatrix = do
        divClass "ui grid" $ mapM showRadical (Map.toList radicalTable)

      showRadical (i,(RadicalDetails r _)) = do
        let valid =
              Set.member i <$> validRadicals
            sel =
              -- pure False
              Set.member i <$> selectedRadicals
            -- (Valid, Selected)
            cl (_,True) = "ui icon green button"
            cl (True, False) = "ui icon button"
            cl (False,False) = "ui icon disabled button"
            attr = (\c -> ("class" =: c)) <$>
                     (cl <$> zipDyn valid sel)

        (e,_) <- elAttr' "div" ("class" =: "column") $
          elDynAttr "button" attr $ text r
        let ev =
              attachDynWithMaybe f valid
                   (domEvent Click e)
            f True _ = Just ()
            f _ _ = Nothing
        return (i <$ ev)

    ev <- renderMatrix

    let
      h :: RadicalId -> Set RadicalId -> Set RadicalId
      h i s = if Set.member i s then Set.delete i s else Set.insert i s
    selectedRadicals <- foldDyn h Set.empty (traceEventWith show $ leftmost ev)

  return $ toList <$> selectedRadicals


kanjiListWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => Event t KanjiList -> m (Event t KanjiId)
kanjiListWidget listEv = do
  let kanjiTable itms = do
        el "table" $ do
          let listItem itm@(i, k, r, m) = do
                (e, _) <- el' "tr" $ do
                  el "td" $ text $ (unKanjiT k)
                  el "td" $ text $ maybe ""
                    (\r1 -> "Rank: " <> show r1) (unRankT <$> r)
                  el "td" $ text $ T.intercalate "," $ map unMeaningT m
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
        kanjiDetailWindow k
        vocabListWindow v
        return ()
  void $ widgetHold (return ()) (f <$> ev)

kanjiDetailWindow :: (DomBuilder t m) => KanjiDetails -> m ()
kanjiDetailWindow (KanjiDetails k r m g j w on ku no) = do
  divClass "ui grid container row" $ do
    divClass "ui divider four wide column" $ do
      divClass "row" $ do
        elClass "i" "bordered big icon" $
          text (unKanjiT k)
      divClass "row" $ do
        text "Radicals here"

    divClass "eight wide column" $ do
      divClass "row" $ do
        text $ T.intercalate "," $ map unMeaningT m

      divClass "row" $ do
        textMay (tshow <$> (unRankT <$> r))

      divClass "row" $ do
        textMay (unOnYomiT <$> on)
        textMay (unKunYomiT <$> ku)

vocabListWindow :: _ => [VocabDispItem] -> m ()
vocabListWindow vs = do
  let
    dispVocab
      (VocabDispItem v r m j w wi) = divClass "row" $ do
      divClass "ui six wide column grid" $ do
        displayVocabT v

      divClass "ui six wide column grid" $ do
        divClass "row" $ do
          text $ T.intercalate "," $ map unMeaningT m

        divClass "row" $ do
          textMay (tshow <$> (unRankT <$> r))

        divClass "row" $ do
          textMay (tshow <$> (unWikiRank <$> wi))
          textMay (tshow <$> (unWkLevelT <$> w))

  divClass "ui grid container" $ do
    forM_ vs dispVocab

displayVocabT :: _ => VocabT -> m ()
displayVocabT (VocabT ks) = do
  let
    f k = case k of
      (Kanji k f) -> divClass "column wide two" $ do
        divClass "row" $ text f
        divClass "row" $ text $ unKanjiT k
      (Kana t) -> divClass "column wide two" $ do
        divClass "row" $ text ""
        divClass "row" $ text t
  mapM_ f ks

tshow = T.pack . show
textMay (Just v) = text v
textMay Nothing = text ""
