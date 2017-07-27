{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KanjiBrowser where

import Protolude hiding (link, (&))
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
import Reflex.Dom.SemanticUI
import Data.Time.Clock

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

textMay (Just v) = text v
textMay Nothing = text ""

vocabSearchWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
vocabSearchWidget = divClass "ui grid" $ divClass "row" $ do

  vocabResEv <- divClass "row" $ do
    reading <- uiTextInput (constDyn def) def
    meaning <- uiTextInput (constDyn def) def

    let vsDyn = VocabSearch <$> (Filter
                  <$> value reading
                  <*> pure KunYomi
                  <*> value meaning)
    getWebSocketResponse (updated vsDyn)

  void $ widgetHold (return ()) (vocabListWindow <$> vocabResEv)

srsWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
srsWidget =  divClass "ui grid" $ do

  showStats
  browseSrsItemsWidget
  return ()

showStats
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
showStats = do
  ev <- getPostBuild
  s <- getWebSocketResponse (GetSrsStats () <$ ev)
  void $ widgetHold (return ()) (showStatsWidget <$> s)

showStatsWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => SrsStats -> m ()
showStatsWidget s = do
  startReview <- divClass "ui grid row" $ do
    ev <- divClass "four wide centered column" $ do
      divClass "two wide centered column" $
        divClass "ui huge label" $
          text $ tshow (pendingReviewCount s)
      divClass "two wide centered column" $ do
        divClass "ui grid centered row" $
          divClass "ui large label" $
            text "reviews pending"

        divClass "ui grid centered row" $
          uiButton (constDyn def) (text "Start reviewing")

    statsCard "Reviews Today" (reviewsToday s)
    statsCard "Total Items" (totalItems s)
    statsCard "Total Reviews" (totalReviews s)
    statsCard "Average Success" (averageSuccess s)
    return ev

  divClass "ui grid row" $ do
    progressStatsCard "Discovering" "D1" "D2"
      (discoveringCount s)
    progressStatsCard "Committing" "C1" "C2"
      (committingCount s)
    progressStatsCard "Bolstering" "B1" "B2"
      (bolsteringCount s)
    progressStatsCard "Assimilating" "A1" "A2"
      (assimilatingCount s)
    divClass "three wide centered column" $ do
      divClass "ui grid centered row" $
        divClass "ui huge label" $
          text $ tshow (setInStone s)
      divClass "ui grid centered row" $
        divClass "ui large label" $
          text "Set in Stone"

  browseEv <- uiButton (constDyn def) (text "Browse Srs Items")
  return ()

statsCard t val = divClass "three wide centered column" $ do
  divClass "ui grid centered row" $
    divClass "ui huge label" $
      text $ tshow val
  divClass "ui grid centered row" $
    divClass "ui large label" $
      text t

progressStatsCard l l1 l2 (v1,v2) =
  divClass "two wide centered column" $ do
    divClass "ui grid centered row" $
      divClass "ui huge label" $
        text $ tshow (v1 + v2)
    divClass "ui grid centered row" $
      divClass "ui large label" $
        text l
    divClass "ui grid row" $ do
      divClass "one wide centered column" $ do
        divClass "ui grid centered row" $
          divClass "ui label" $ text l1
        divClass "ui grid centered row" $
          divClass "ui label" $ text $ tshow v1

      divClass "one wide centered column" $ do
        divClass "ui grid centered row" $
          divClass "ui label" $ text l2
        divClass "ui grid centered row" $
          divClass "ui label" $ text $ tshow v2

-- Fetch all srs items then apply the filter client side
-- fetch srs items for every change in filter
--
browseSrsItemsWidget
  :: (MonadWidget t m, DomBuilderSpace m ~ GhcjsDomSpace, PrimMonad m)
  => WithWebSocketT Message.AppRequest t m ()
browseSrsItemsWidget = do

  -- ev <- getPostBuild
  -- initItemList <- getWebSocketResponse $ BrowseSrsItems [0] <$ ev

  let
    srsLevels :: _ => [(SrsLevel, DropdownItemConfig m)]
    srsLevels = map (\g -> (g, DropdownItemConfig (tshow g) (text $ tshow g))) [0..8]


  -- UI
  divClass "ui grid row" $ do
    -- Filter Options
    (filteredList, selectAllToggleCheckBox) <-
      divClass "ui grid row" $ do
        -- Selection buttons
        selectAllToggleCheckBox <- divClass "two wide column centered" $ do

          uiCheckbox (text "Select All") $
            def -- & setValue .~ allSelected

        -- Level
        -- (levels :: Dynamic t [SrsLevel])
        levels
          <- divClass "four wide column centered" $
             divClass "field" $ el "label" $
               uiDropdownMulti srsLevels [DOFSelection] $
                 def & dropdownConf_initialValue .~ []

         -- Kanji/Vocab
         -- Pending review

        filteredList <- getWebSocketResponse $
          BrowseSrsItems <$> updated levels
        return (filteredList, selectAllToggleCheckBox)

    let
      -- itemEv :: Event t [SrsItem]
      -- initItemList = never
      itemEv = filteredList -- leftmost [initItemList, filteredList]

      checkBoxSelAllEv = updated $
        value selectAllToggleCheckBox

      checkBoxList es =
        divClass "eight wide column centered" $ do
          el "label" $ text "Select Items to do bulk edit"
          evs <- elAttr "div" (("class" =: "ui list")
                               <> ("style" =: "height: 400px; overflow-y: scroll")) $
            forM es checkBoxEl

          let f (v, True) s = Set.insert v s
              f (v, False) s = Set.delete v s
          selList <- foldDyn f Set.empty (leftmost evs)

          return $ Set.toList <$> selList

      f (Left (VocabT ((Kana k):_))) = k
      f (Right (KanjiT k)) = k
      checkBoxEl (SrsItem i v _) = divClass "item" $ do
        c1 <- uiCheckbox (text $ f v) $
          def & setValue .~ checkBoxSelAllEv
        return $ (,) i <$> updated (value c1)

    -- List and selection checkBox
    selList <- divClass "ui grid row" $ do
      widgetHold (checkBoxList []) (checkBoxList <$> itemEv)

    -- Action buttons
    selList <- divClass "ui grid row" $ do

      suspendEv <- divClass "two wide column centered" $
        uiButton (constDyn def) (text "Suspend")

      deleteEv <- divClass "two wide column centered" $
        uiButton (constDyn def) (text "Delete")

      changeLvlSel <- divClass "two wide column centered" $
        uiDropdown srsLevels [DOFSelection] $
          def & dropdownConf_initialValue .~ Just 0
      changeLvlDyn <- foldDynMaybe const 0 $
        updated changeLvlSel
      changeLvlEv <- divClass "two wide column centered" $
        uiButton (constDyn def) (text "Change Level")

      reviewDateChange <- divClass "two wide column centered" $
        uiButton (constDyn def) (text "Change Review Date")

      reviewDate <- liftIO getCurrentTime
      let bEditOp = leftmost
            [DeleteSrsItems <$ deleteEv
            , SuspendSrsItems <$ suspendEv
            , ChangeSrsLevel <$> tagPromptlyDyn changeLvlDyn changeLvlEv
            , ChangeSrsReviewData reviewDate
              <$ reviewDateChange]
      getWebSocketResponse $ uncurry BulkEditSrsItems <$>
        (attachDyn (join selList) bEditOp)

    void $ divClass "ui row" $
      uiButton (constDyn def) (text "Close Widget")
  return ()
