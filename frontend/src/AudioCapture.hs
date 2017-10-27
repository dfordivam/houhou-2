module AudioCapture where

import Protolude hiding (on)

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

import Reflex.Dom

import qualified Data.Vector.Unboxed as VU
import AudioProcessor

audioCaptureWidget = do
  text "AudioCaptureWidget"
  liftIO $ putStrLn ( "testing" :: Protolude.Text)
  (ev, triggerEvFun) <- newTriggerEvent
  let process = do
        mediaStr <- audioSetup
        processor <- getScriptProcessorNode mediaStr
        countRef <- liftIO $ newIORef (0,0)
        liftIO $ forkIO $ do
          threadDelay 10000000
          remove <- on processor audioProcess (onAudioProcess countRef triggerEvFun)
          threadDelay 3000000 >> remove
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
  :: IORef (Int, Int)
  -> (AudioRecordedData -> IO ())
  -> EventM ScriptProcessorNode AudioProcessingEvent ()
onAudioProcess countRef triggerEvFun = do
  -- putStrLn ("Start Audio Process" :: Protolude.Text)
  aEv <- ask
  callBackListener aEv countRef triggerEvFun

callBackListener :: MonadDOM m
  => AudioProcessingEvent
  -> IORef (Int, Int)
  -> (AudioRecordedData -> IO ())
  -> m ()
callBackListener e countRef triggerEvFun = do
  buf <- getInputBuffer e
  (count, rem) <- liftIO $ readIORef countRef

  -- TODO Use the sample rate
  rate <- getSampleRate buf
  -- putStrLn $ ("Sample Rate:" <> show rate :: Protolude.Text)
  dd <- getChannelData buf 0

  -- liftIO $ consoleLog (unFloat32Array dd)
  let
    dlen = js_float32array_length dd

  putStrLn $ ("Dlen:" <> show dlen :: Protolude.Text)
  putStrLn $ ("Count:" <> show count :: Protolude.Text)

  let makeVector i = do
        v <- (indexArr (i * 3) dd)
        -- Change the range from -1, 1 to 16 bit pcm
        return $! (v * 2^15)
      -- Downsample 48kHz to 16kHz
      vecLen = ceiling $ (fromIntegral dlen) / 3

  dvec <- liftIO $ VU.generateM vecLen makeVector

  let (b,e) = isValidSample dvec
      (sendData, remN) = if b
        then (True, 3)
        else if rem > 0
             then (True, rem - 1)
             else (False, 0)

  liftIO $ writeIORef countRef (count + 1, remN)

  -- putStrLn $ ("Data energy:" <> (show $ e) :: Protolude.Text)
  -- when sendData $
  liftIO $ triggerEvFun (count, downsampleAudio rate dvec)

-- foreign import javascript unsafe "console['log']($1)" consoleLog :: JSVal -> IO ()
foreign import javascript unsafe
  "($1).length" js_float32array_length :: Float32Array -> Int

indexArr :: Int -> Float32Array -> IO (Double)
indexArr = js_indexD
{-# INLINE indexArr #-}

foreign import javascript unsafe
  "$2[$1]" js_indexD
  :: Int -> Float32Array -> IO Double

-- Multirate Downsampling code
-- Code copied from dsp package and enhanced for Unboxed Vector usage
cic_interpolate :: Int -> Int -> Int -> VU.Vector Double -> VU.Vector Double
cic_interpolate r m n vec =
  case VU.length vec of
    0 -> vec
    _ -> ((integrate_chain n) . (upsampleV r) . (comb_chain m n)) vec
  where
    integrate_chain n = apply integrateV n
    comb_chain m n = apply (combV m) n

    combV :: Int -> VU.Vector Double -> VU.Vector Double
    combV m vec = VU.zipWith (-) vec delayedVec
      where delayedVec = (VU.++) (VU.replicate m 0.0) vec

    integrateV :: VU.Vector Double -> VU.Vector Double
    integrateV vec = VU.postscanl' (+) 0 vec

    -- | @upsample@ inserts n-1 zeros between each sample, eg,
    --
    -- @upsample 2 [ 1, 2, 3 ] == [ 1, 0, 2, 0, 3, 0 ]@

    upsampleV :: Int -> VU.Vector Double -> VU.Vector Double
    upsampleV n vec = VU.update zeroVec indVec
      where zeroVec = VU.replicate (n * (VU.length vec)) 0.0
            indVec = VU.imap (\i a -> (n * i, a)) vec

    apply :: (a -> a) -> Int -> (a -> a)
    apply f 1 = f
    apply f n = f . apply f (n - 1)

-- | @downsample@ throws away every n'th sample, eg,
--
-- @downsample 2 [ 1, 2, 3, 4, 5, 6 ] == [ 1, 3, 5 ]@

downsample n vec = VU.generate len genF
  where len = floor ((fromIntegral $ VU.length vec)/(fromIntegral n))
        genF i = (VU.!) vec (i * n)
