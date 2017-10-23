module AudioCapture where

import Protolude hiding (on)

import Control.Monad.Primitive
import qualified Data.Map as Map
import Control.Lens
import Control.Monad.Fix

import GHCJS.DOM.AudioBuffer hiding (getGain)
import GHCJS.DOM.ScriptProcessorNode
import GHCJS.DOM.AudioProcessingEvent
import GHCJS.DOM.AudioNode
import GHCJS.DOM.Types hiding (ByteString)
import GHCJS.DOM.EventM
import GHCJS.DOM.AudioContext hiding (getSampleRate)
import GHCJS.DOM.Window
import GHCJS.DOM.MediaDevices
import GHCJS.DOM.WebSocket
import GHCJS.DOM.Navigator
import GHCJS.DOM
import Language.Javascript.JSaddle.Value
import Language.Javascript.JSaddle.Types
import JavaScript.Object
import Data.IORef

-- import qualified JavaScript.TypedArray as TA
--import qualified JavaScript.TypedArray.ArrayBuffer as TA
-- import GHCJS.Buffer (toByteString, createFromArrayBuffer)

import Reflex.Dom
-- import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer)
-- import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)

import qualified Data.Vector.Unboxed as VU
import AudioProcessor

audioCaptureWidget = do
  text "AudioCaptureWidget"
  liftIO $ putStrLn ( "testing" :: Protolude.Text)
  (ev, triggerEvFun) <- newTriggerEvent
  let process = do
        mediaStr <- audioSetup
        processor <- getScriptProcessorNode mediaStr
        countRef <- liftIO $ newIORef 0
        remove <- liftIO $ on processor audioProcess (onAudioProcess countRef triggerEvFun)
        putStrLn ("MediaStream Setup Done" :: Protolude.Text)
        liftIO $ forkIO $ threadDelay 10000000 >> remove
        putStrLn ("MediaStream Stopped" :: Protolude.Text)
  process
  return (ev)

audioSetup :: MonadDOM m => m (MediaStream)
audioSetup = do
  win <- currentWindowUnchecked
  nav <- getNavigator win

  devices <- getMediaDevices nav

  v <- liftIO $ do
    o <- create
    t <- toJSVal True
    setProp "audio" t o
    toJSVal (ValObject o)

  let constraints = MediaStreamConstraints v
  GHCJS.DOM.MediaDevices.getUserMedia devices (Just constraints)

getScriptProcessorNode :: MonadDOM m => MediaStream -> m (ScriptProcessorNode)
getScriptProcessorNode mediaStream = do
  -- newAudioContext :: MonadDOM m => m AudioContext
  context <- newAudioContext

  strSrc <- createMediaStreamSource context mediaStream

  let
    -- 256, 512, 1024, 2048, 4096, 8192 or 16384.
    bufferSize = 16384
  processor <- createScriptProcessor context bufferSize (Just 1) (Just 1)

  connect strSrc processor Nothing Nothing
  dest <- getDestination context
  connect processor dest Nothing Nothing
  return processor

onAudioProcess
  :: IORef Int
  -> (AudioRecordedData -> IO ())
  -> EventM ScriptProcessorNode AudioProcessingEvent ()
onAudioProcess countRef triggerEvFun = do
  -- putStrLn ("Start Audio Process" :: Protolude.Text)
  aEv <- ask
  callBackListener aEv countRef triggerEvFun

callBackListener :: MonadDOM m
  => AudioProcessingEvent
  -> IORef Int
  -> (AudioRecordedData -> IO ())
  -> m ()
callBackListener e countRef triggerEvFun = do
  -- getInputBuffer :: MonadDOM m => AudioProcessingEvent -> m AudioBuffer
  buf <- getInputBuffer e
  count <- liftIO $ atomicModifyIORef countRef (\c -> (c + 1, c))

  rate <- getSampleRate buf
  -- putStrLn $ ("Sample Rate:" <> show rate :: Protolude.Text)
  -- getChannelData :: MonadDOM m => AudioBuffer -> Word -> m AudioRecordedData
  dd <- getChannelData buf 0

  liftIO $ consoleLog (unFloat32Array dd)
  let
    -- dbytelen = js_float32array_bytelength dd
    dlen = js_float32array_length dd
    doff = js_float32array_byteoffset dd
    -- dbuf = js_float32array_buffer dd

  putStrLn $ ("Dlen:" <> show dlen :: Protolude.Text)
  putStrLn $ ("Count:" <> show count :: Protolude.Text)

  let makeVector i = do
        v <- (indexArr i dd)
        case v * (2 ^ 15) of
          0 -> return 0
          x -> return $ (fromInteger (floor x))
  dvec <- liftIO $ mapM makeVector [0..(dlen - 1)]
  putStrLn $ ("Data 0:" <> (show $ length $ filter (== 0) dvec) :: Protolude.Text)
  liftIO $ triggerEvFun (dvec, count)

  -- send wsConn (ArrayBuffer $ unFloat32Array d)
foreign import javascript unsafe "console['log']($1)" consoleLog :: JSVal -> IO ()
foreign import javascript unsafe
  "($1).length" js_float32array_length :: Float32Array -> Int
foreign import javascript unsafe
  "($1).byteLength" js_float32array_bytelength :: Float32Array -> Int
-- foreign import javascript unsafe
--   "($1).buffer" js_float32array_buffer :: Float32Array -> TA.ArrayBuffer
foreign import javascript unsafe
  "($1).byteoffset" js_float32array_byteoffset :: Float32Array -> Int

indexArr :: Int -> Float32Array -> IO (Double)
indexArr = js_indexD
{-# INLINE indexArr #-}

-- indexD :: Int -> Float32Array -> State# s -> (# State# s, Double #)
-- indexD a i = \s -> js_indexD a i s
-- {-# INLINE indexD #-}

foreign import javascript unsafe
  "$2[$1]" js_indexD
  :: Int -> Float32Array -> IO Double
