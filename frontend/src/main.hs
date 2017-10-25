{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Protolude hiding (link)
import Reflex.Dom
import KanjiBrowser
import AudioCapture

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.SemanticUI
import Message (AppRequest)
import Control.Monad.Primitive
import qualified Data.Map as Map
import Control.Lens
import Control.Monad.Fix

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import MFCC
import Data.Aeson
import AudioProcessor

main = mainWidget $ do
  let url = "ws://localhost:3000/"
  audioEv <- audioCaptureWidget
  let conf = WebSocketConfig
        ((:[]) <$> procAudioEv) never True
      procAudioEv = traceEventWith (\bs -> "Processed Event" <> (show $ BS.length bs))
        $ sendAudio <$> audioEv
  webSocket "ws://localhost:3001/" conf
  --widgetHold (return ()) (audioProcessor <$> audioEv)
  withWSConnection
    url
    never -- close event
    True -- reconnect
    topWidget
  return ()

-- audioProcessor :: Float32Array -> m ()
-- audioProcessor arr = do
--   liftIO $ putStrLn ( "Recieved audioProcessor event" :: Protolude.Text)
--   let floatArray = parseFloatData arr
--   liftIO $ putStrLn ( "data:" <> show floatArray :: Protolude.Text)

topWidget
  :: AppMonad t m => AppMonadT t m ()
topWidget = divClass "ui container" $ do
  -- navigation with visibility control
  tabDisplayUI "ui three item menu" "item active" "item" $
    Map.fromList
      [(0, ("SRS", srsWidget))
      ,(1, ("Kanji", kanjiBrowseWidget))
      , (2, ("Vocab", vocabSearchWidget))]

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplayUI :: forall m k t . (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to top <div> element
  -> Text               -- ^ Class applied to currently active <div> element
  -> Text               -- ^ Class applied to currently non-active <div> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplayUI ulClass activeClass nonActiveClass tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "div" ("class" =: ulClass) $ do
        tabClicksList :: [Event t k] <- Map.elems <$> imapM (\k (s,_) -> headerBarLink s k $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks
  el "div" $ do
    iforM_ tabItems $ \k (_, w) -> do
      let isSelected = demuxed currentTab $ Just k
          attrs = ffor isSelected $ \s -> if s then Map.empty else Map.singleton "style" "display:none;"
      elDynAttr "div" attrs w
    return ()
  where
    headerBarLink :: Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      let attrs = fmap (\b -> if b then Map.singleton "class" activeClass else Map.singleton "class" nonActiveClass) isSelected
      elDynAttr "div" attrs $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)
