{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module KanjiBrowser where

import Protolude hiding (link, (&))
import Reflex.Dom
import Reflex.Time
import Message
import Common
import Radicals
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Fix

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
import Data.Time.Calendar
import NLP.Romkan (toHiragana)
--
type AppMonadT t m = WithWebSocketT Message.AppRequest t m
type AppMonad t m = (MonadWidget t m
                    , PrimMonad m)

data VisibleWidget = KanjiFilterVis | KanjiDetailPageVis
  deriving (Eq)

kanjiBrowseWidget
  :: AppMonad t m
  => AppMonadT t m ()
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
  :: (DomBuilder t m, PostBuild t m, Eq a)
  => a -> Dynamic t a -> m v -> m v
handleVisibility v dv mv = elDynAttr "div" (f <$> dv) mv
  where
    f dv =
      if v == dv
        then Map.empty
        else ("style" =: "display: none;")

kanjiFilterWidget
  :: AppMonad t m
  => Event t [RadicalId]
  -> AppMonadT t m (Event t KanjiFilter)
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
  :: ( DomBuilder t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , MonadFix m
     )
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
  :: (DomBuilder t m, MonadHold t m)
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
  :: (DomBuilder t m, MonadHold t m)
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

vocabListWindow :: DomBuilder t m => [VocabDispItem] -> m ()
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

displayVocabT :: DomBuilder t m => VocabT -> m ()
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
  :: AppMonad t m
  => AppMonadT t m ()
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

data SrsWidgetView =
  ShowStatsWindow | ShowReviewWindow | ShowBrowseSrsItemsWindow
  deriving (Eq)

srsWidget
  :: AppMonad t m
  => AppMonadT t m ()
srsWidget = divClass "ui container" $ do
  let

  rec
    let
      visEv = leftmost [ev1,ev2,ev3]
    vis <- holdDyn ShowStatsWindow visEv

    ev1 <- handleVisibility ShowStatsWindow vis $
      showStats

    ev2 <- handleVisibility ShowBrowseSrsItemsWindow vis $
      browseSrsItemsWidget

    ev3 <- handleVisibility ShowReviewWindow vis $
      reviewWidget
  return ()

showStats
  :: AppMonad t m
  => AppMonadT t m (Event t SrsWidgetView)
showStats = do
  ev <- getPostBuild
  s <- getWebSocketResponse (GetSrsStats () <$ ev)
  retEvDyn <- widgetHold (return never) (showStatsWidget <$> s)
  return $ switchPromptlyDyn $ retEvDyn

showStatsWidget
  :: (MonadWidget t m)
  => SrsStats -> m (Event t SrsWidgetView)
showStatsWidget s = do
  startReviewEv <- divClass "ui grid row" $ do
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
  return $ leftmost [ShowReviewWindow <$ startReviewEv
                    , ShowBrowseSrsItemsWindow <$ browseEv]

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

-- TODO Fix this srsLevels
srsLevels :: _ => [(SrsLevel, DropdownItemConfig m)]
srsLevels = map (\g -> (g, DropdownItemConfig (tshow g) (text $ tshow g))) [0..8]

-- Fetch all srs items then apply the filter client side
-- fetch srs items for every change in filter
--
browseSrsItemsWidget
  :: AppMonad t m
  => AppMonadT t m (Event t SrsWidgetView)
browseSrsItemsWidget = do
  -- Widget declarations
  let

    filterOptionsWidget =
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

        return (BrowseSrsItems <$> updated levels, selectAllToggleCheckBox)

    checkBoxList selAllEv es =
      divClass "eight wide column centered" $ do
        el "label" $ text "Select Items to do bulk edit"
        evs <- elAttr "div" (("class" =: "ui middle aligned selection list")
                             <> ("style" =: "height: 400px; overflow-y: scroll")) $
          forM es $ checkBoxListEl selAllEv

        let f (v, True) s = Set.insert v s
            f (v, False) s = Set.delete v s
        selList <- foldDyn f Set.empty (leftmost evs)

        return $ Set.toList <$> selList

    checkBoxListEl selAllEv (SrsItem i v sus pend) = divClass "item" $ do
      let
        f (Left (VocabT ((Kana k):_))) = k
        f (Right (KanjiT k)) = k
        c = if sus
          then divClass "ui basic grey label"
          else if pend
            then divClass "ui basic violet label"
            else divClass "ui basic black label"
        editButton = uiButton (constDyn def) (text "Edit")
        wrap cb = do
          divClass "right floated content" $ do
            ev <- editButton
            openEditSrsItemWidget $ i <$ ev
          divClass "content" $ cb
      c1 <- wrap $ uiCheckbox (c $ (text $ f v)) $
        def & setValue .~ selAllEv
      return $ (,) i <$> updated (value c1)

  -- UI
  divClass "ui grid row" $ do
    -- Filter Options
    (browseSrsFilterEv, selectAllToggleCheckBox) <-
      filterOptionsWidget

    filteredList <- getWebSocketResponse browseSrsFilterEv
    browseSrsFilterDyn <- holdDyn (BrowseSrsItems []) browseSrsFilterEv
    rec
      let
        itemEv = leftmost [filteredList, afterEditList]

        checkBoxSelAllEv = updated $
          value selectAllToggleCheckBox

      -- List and selection checkBox
      selList <- divClass "ui grid row" $ do
        widgetHold (checkBoxList never [])
          (checkBoxList checkBoxSelAllEv <$> itemEv)

      -- Action buttons
      afterEditList <-
        bulkEditWidgetActionButtons browseSrsFilterDyn $ join selList
    return ()

  closeEv <- divClass "ui row" $
    uiButton (constDyn def) (text "Close Widget")
  return $ ShowStatsWindow <$ closeEv

bulkEditWidgetActionButtons
  :: AppMonad t m
  => Dynamic t BrowseSrsItems
  -> Dynamic t [SrsItemId]
  -> AppMonadT t m (Event t [SrsItem])
bulkEditWidgetActionButtons filtOptsDyn selList = divClass "ui grid row" $ do
  currentTime <- liftIO getCurrentTime

  suspendEv <- divClass "two wide column centered" $
    uiButton (constDyn def) (text "Suspend")

  resumeEv <- divClass "two wide column centered" $
    uiButton (constDyn def) (text "Resume")

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

  dateDyn <- divClass "two wide column centered" $ datePicker currentTime
  let bEditOp = leftmost
        [DeleteSrsItems <$ deleteEv
        , SuspendSrsItems <$ suspendEv
        , ResumeSrsItems <$ resumeEv
        , ChangeSrsLevel <$> tagPromptlyDyn changeLvlDyn changeLvlEv
        , ChangeSrsReviewData <$> tagPromptlyDyn dateDyn reviewDateChange]
  getWebSocketResponse $ (\((s,b),e) -> BulkEditSrsItems s e b) <$>
    (attachDyn ((,) <$> selList <*> filtOptsDyn) bEditOp)

datePicker
  :: (MonadWidget t m)
  => UTCTime -> m (Dynamic t UTCTime)
datePicker defTime = divClass "ui grid row " $ do
  let dayList = makeList <$> [1..31]
      monthList = makeList <$> [1..12]
      yearList = makeList <$> [2000..2030]
      makeList x = (x, DropdownItemConfig (tshow x) (text $ tshow x))
      (currentYear, currentMonth, currentDay)
        = (\(UTCTime d _) -> toGregorian d) defTime
      mycol = divClass "column"
        --elAttr "div" (("class" =: "column") <> ("style" =: "min-width: 2em;"))
  day <- mycol $ uiDropdown dayList [DOFSearch, DOFSelection] $
    def & dropdownConf_placeholder .~ "Day"
        & dropdownConf_initialValue ?~ (currentDay)
  month <- mycol $ uiDropdown monthList [DOFSearch, DOFSelection] $
    def & dropdownConf_placeholder .~ "Month"
        & dropdownConf_initialValue ?~ (currentMonth)
  year <- mycol $ uiDropdown yearList [DOFSearch, DOFSelection] $
    def & dropdownConf_placeholder .~ "Year"
        & dropdownConf_initialValue ?~ (currentYear)
  let f y m d = maybe (utctDay defTime) identity $ fromGregorian <$> y <*> m <*> d
  return $ UTCTime <$> (f <$> year <*> month <*> day) <*> pure 1

openEditSrsItemWidget
  :: (AppMonad t m)
  => Event t (SrsItemId)
  -> AppMonadT t m ()
openEditSrsItemWidget ev = do
  srsItEv <- getWebSocketResponse $ GetSrsItem <$> ev

  let
      modalWidget :: (AppMonad t m) => Maybe SrsItemFull -> AppMonadT t m ()
      modalWidget (Just s) = do
        ev <- getPostBuild
        uiModal (ShowModal <$ ev) (editWidget s)
      modalWidget Nothing = do
        ev <- getPostBuild
        uiModal (ShowModal <$ ev) (text $ "Some Error")


      f (Left (VocabT ((Kana k):_))) = k
      f (Right (KanjiT k)) = k

      editWidget :: AppMonad t m => SrsItemFull -> AppMonadT t m ()
      editWidget s = do
        rec
          (sNew, saveEv) <- editWidgetView s ev
          ev <- getWebSocketResponse $ EditSrsItem <$> tagDyn sNew saveEv
        return ()

      editWidgetView
        :: MonadWidget t m
        => SrsItemFull
        -> Event t ()
        -> m (Dynamic t SrsItemFull, Event t ())
      editWidgetView s savedEv = divClass "ui raised very padded container segment" $ do
        elClass "h3" "ui dividing header" $ do
          text $ "Edit " <> (f $ srsItemFullVocabOrKanji s)

        reviewDateDyn <- divClass "ui grid row" $ do
          reviewDataPicker (srsReviewDate s)

        (m,r) <- divClass "ui two column grid" $ do
          meaningTxtInp <- divClass "column" $ divClass "inline field" $ do
            divClass "ui large blue right pointing label" $ text "Meaning"
            uiTextInput (constDyn def) $ def &
              textInputConfig_initialValue .~ (srsMeanings s)

          readingTxtInp <- divClass "column" $ divClass "inline field" $ do
            divClass "ui large blue right pointing label" $ text "Reading"
            uiTextInput (constDyn def) $ def &
              textInputConfig_initialValue .~ (srsReadings s)

          return (meaningTxtInp, readingTxtInp)

        (mn,rn) <- divClass "ui two column grid" $ do
          meaningNotesTxtInp <- divClass "column" $ do
            divClass "ui large blue bottom pointing label" $ text "Meaning Notes"
            divClass "ui form" $ divClass "field" $ do
              textArea $ def &
                textAreaConfig_initialValue .~
                  (maybe "" identity (srsMeaningNote s))

          readingNotesTxtInp <- divClass "column" $ do
            divClass "ui large blue bottom pointing label" $ text "Reading Notes"
            divClass "ui form" $ divClass "field" $ do
              textArea $ def &
                textAreaConfig_initialValue .~
                  (maybe "" identity (srsReadingNote s))

          return (meaningNotesTxtInp, readingNotesTxtInp)

        tagsTxtInp <- divClass "ui grid row" $ do
          divClass "column" $ divClass "inline field" $ do
            divClass "ui large blue right pointing label" $ text "Tags"
            uiTextInput (constDyn def) $ def &
              textInputConfig_initialValue .~
                (maybe "" identity (srsTags s))

        saveEv <- divClass "ui grid row" $ do
          let savedIcon = elClass "i" "big green checkmark icon" $ return ()
          ev <- uiButton (constDyn def) (text "Save")
          widgetHold (return ()) (savedIcon <$ savedEv)
          return ev

        let ret = SrsItemFull (srsItemFullId s) (srsItemFullVocabOrKanji s)
                    <$> reviewDateDyn <*> (value m) <*> (value r)
                    <*> pure (srsCurrentGrade s) <*> g mn <*> g rn
                    <*> g tagsTxtInp
            g v = gg <$> value v
            gg t
              | T.null t = Nothing
              | otherwise = Just t

        return (ret, saveEv)

      reviewDataPicker :: (MonadWidget t m) =>
        Maybe UTCTime -> m (Dynamic t (Maybe UTCTime))
      reviewDataPicker inp = do
        currentTime <- liftIO getCurrentTime

        let
          addDateW = do
            uiButton (constDyn def) (text "Add Next Review Date")

          selectDateW = do
            divClass "ui two column grid" $ do
              newDateDyn <- divClass "column" $ datePicker defDate
              removeDate <- divClass "column" $
                uiButton (constDyn def) (text "Remove Review Date")
              return (removeDate, newDateDyn)

          defDate = maybe currentTime identity inp

        rec
          vDyn <- holdDyn (isJust inp) (leftmost [False <$ r, True <$ a])
          a <- handleVisibility False vDyn addDateW
          (r,d) <- handleVisibility True vDyn selectDateW
        let
            f :: Reflex t => (Dynamic t a) -> Bool -> Dynamic t (Maybe a)
            f d True = Just <$> d
            f _ _ = pure Nothing
        return $ join $ f d <$> vDyn

  void $ widgetHold (return ()) (modalWidget <$> srsItEv)

reviewWidget
  :: (AppMonad t m)
  => AppMonadT t m (Event t SrsWidgetView)
reviewWidget = do
  let

  let attr = ("class" =: "ui middle aligned center aligned grid")
             <> ("style" =: "height: 50rem;")

  ev <- getPostBuild
  initEv <- getWebSocketResponse $ GetNextReviewItem <$ ev

  closeEv <- elAttr "div" attr $ divClass "column" $ do
    closeEv <- divClass "fluid" $
      uiButton (constDyn def) (text "Close Review")

    rec
      let reviewItemEv = fmapMaybeCheap identity $
            leftmost [initEv, nextReviewItemEv]

          nrEv = switchPromptlyDyn drDyn
      nextReviewItemEv <- getWebSocketResponse $ nrEv

      drDyn <- widgetHold (return never) $
        reviewWidgetView <$> reviewItemEv

    return closeEv

  return $ ShowStatsWindow <$ closeEv

reviewWidgetView
  :: AppMonad t m
  => ReviewItem -> AppMonadT t m (Event t DoReview)
reviewWidgetView ri@(ReviewItem i k n s) = do
  let
    statsRowAttr = ("class" =: "ui right aligned container")
              <> ("style" =: "height: 15rem;")
    statsTextAttr = ("style" =: "font-size: large;")

    showStats s = do
      let colour c = ("style" =: ("color: " <> c <>";" ))
      elAttr "span" (colour "black") $
        text $ tshow (_srsReviewStats_pendingCount s)  <> " "
      elAttr "span" (colour "green") $
        text $ tshow (_srsReviewStats_correctCount s) <> " "
      elAttr "span" (colour "red") $
        text $ tshow (_srsReviewStats_incorrectCount s)

  divClass "row" $ elAttr "div" statsRowAttr $ do
    elAttr "span" statsTextAttr $
      showStats s

  let kanjiRowAttr = ("class" =: "row")
         <> ("style" =: "height: 10rem;")
      kanjiTextAttr = ("style" =: "font-size: 5rem;")

  elAttr "div" kanjiRowAttr $
    elAttr "span" kanjiTextAttr $ do
      let
        f (Left (VocabT ((Kana k):_))) = k
        f (Right (KanjiT k)) = k
      text $ f k

  (dr,inpTextValue) <- inputFieldWidget ri

  let notesRowAttr = ("class" =: "ui left aligned container")
         <> ("style" =: "height: 10rem;")
      notesTextAttr = ("style" =: "font-size: large;")
      notes = case n of
        (Left (_,mn)) -> mn
        (Right (_,rn)) -> rn

  divClass "row" $ elAttr "div" notesRowAttr $ do
    elClass "h3" "" $ text "Notes:"
    elAttr "p" notesTextAttr $ text notes

  evB <- divClass "row" $ divClass "ui three column grid" $ do
    ev1 <- divClass "column" $
      uiButton (constDyn def) (text "Undo")
    ev2 <- divClass "column" $
      uiButton (constDyn def) (text "Add Meaning")
    ev3 <- divClass "column" $
      uiButton (constDyn def) (text "Edit")
    openEditSrsItemWidget (i <$ ev3)
    let
        rt = case n of
          (Left _) -> MeaningReview
          (Right _) -> ReadingReview
    return $ leftmost
      [UndoReview <$ ev1
      , AddAnswer i rt <$> tagDyn inpTextValue ev2]
  return $ leftmost [evB,dr]

inputFieldWidget
  :: _
  => ReviewItem
  -> m (Event t DoReview, Dynamic t Text)
inputFieldWidget ri@(ReviewItem i k n s) = do
  let
    style = "text-align: center;" <> color
    color = if rt == MeaningReview
      then "background-color: palegreen;"
      else "background-color: aliceblue;"
    rt = case n of
      (Left _) -> MeaningReview
      (Right _) -> ReadingReview
    inputField ev = do
      let inpFieldAttr = constDyn $ def
            & uiInput_fluid ?~ UiFluid
          tiAttr = def
            & textInputConfig_setValue .~ ev
            & textInputConfig_attributes
            .~ constDyn ("style" =: style)
      divClass "row" $
        -- elClass "form" "ui large form" $
          divClass "field" $ do
            uiTextInput inpFieldAttr tiAttr

    showResult b = do
      let res = if b then "Correct" else "Incorrect"
      divClass "row" $ text $ "Result: " <> res

  rec
    inpField <- inputField inpTxtEv
    (dr, inpTxtEv, resEv) <-
      reviewInputFieldHandler inpField ri
  widgetHold (return ()) (showResult <$> resEv)
  return (dr, value inpField)

reviewInputFieldHandler
 :: (MonadFix m,
     MonadHold t m,
     Reflex t)
 => TextInput t
 -> ReviewItem
 -> m (Event t DoReview, Event t Text, Event t Bool)
reviewInputFieldHandler ti (ReviewItem i k n s) = do
  let enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      correct = checkAnswer n <$> value ti
      h _ NewReview = ShowAnswer
      h _ ShowAnswer = NextReview
      h _ _ = NewReview
  dyn <- foldDyn h NewReview enterPress
  let
    sendResult = ffilter (== NextReview) (tagDyn dyn enterPress)
    dr = DoReview i rt <$> tagDyn correct sendResult

    rt = case n of
      (Left _) -> MeaningReview
      (Right _) -> ReadingReview

    hiragana = case rt of
      MeaningReview -> never
      ReadingReview -> toHiragana <$> (ti ^. textInput_input)
  return (dr, hiragana, tagDyn correct enterPress)

-- TODO
-- For meaning reviews allow minor mistakes
checkAnswer :: (Either (MeaningT, MeaningNotesT) (ReadingT, ReadingNotesT))
            -> Text
            -> Bool
checkAnswer (Left (MeaningT m,_)) t = elem t answers
  where answers = T.splitOn "," m
checkAnswer (Right (r,_)) t = elem t answers
  where answers = T.splitOn "," r

data ReviewState = NewReview | ShowAnswer | NextReview
  deriving (Eq)
