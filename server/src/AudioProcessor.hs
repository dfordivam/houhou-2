module AudioProcessor where

import Protolude
import Message
import Utils


import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import Data.Binary.Builder (toLazyByteString)
import Data.Binary
import Data.Binary.Put
import Data.Vector.Binary
import qualified Data.Aeson as Aeson
import Data.Binary.IEEE754

import NLP.Julius.Interface
import qualified Data.Vector.Unboxed as VU
import Data.IORef
import Text.Pretty.Simple

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

-- type SamplingRate = Int
-- type MelFilterBank = [Double]
-- type DataFromClient = (Int, [MelFilterBank])
-- type AudioFromClient = (Int, [Float])

-- processAudioData :: ByteString -> Handle -> IO ()
-- processAudioData bs fh = do
--   let d = decode (BSL.fromStrict bs) :: Maybe AudioFromClient
--       count = fst <$> d
--       audioD = snd <$> d
--   putStrLn $ ("Got Data:" <> show count :: Text)

--   let
--     buil (Just ad) = Just $ mconcat $ map putInt16le $ map floor
--       $ ad
--     buil Nothing = Nothing

--   mapM_ (BSL.hPut fh) ((toLazyByteString . execPut) <$> (buil audioD))
--   return ()

-- processAudioData :: ByteString -> Handle -> IORef ByteString -> IO ()
-- processAudioData bs fh melDataRef = do
--   let d = Aeson.decode (BSL.fromStrict bs) :: Maybe DataFromClient
--       count = fst <$> d
--       melD = snd <$> d
--   putStrLn $ ("Got Data:" <> show count :: Text)

--   let
--     buil (Just melVs) = Just $ mconcat (map melBuil melVs)
--     buil Nothing = Nothing
--     melBuil melV = mconcat $ map putFloat32le $ map realToFrac melV

--   appendFile "data" (show melD)
--   putStrLn $ ("Got Data:" <> show (fmap (fmap length) (melD)) :: Text)
--   let bsData = maybe "" (BSL.toStrict)
--         $ (toLazyByteString . execPut) <$> (buil melD)

--   modifyIORef' melDataRef (\d -> BS.append d bsData)
--   bsAll <- readIORef melDataRef
--   when (BS.length bsAll > 39000)
--     $ doAsr bsAll >> writeIORef melDataRef ""

--   mapM_ (BSL.hPut fh) ((toLazyByteString . execPut) <$> (buil melD))
--   return ()

getCheckAnswerAudio :: CheckAnswerAudio
  -> HandlerM CheckAnswerResult
getCheckAnswerAudio (CheckAnswerAudio reading audioData) = do
  let melBsBuil = mconcat $ map putFloat32le
        $ map (\i -> (fromIntegral i) / 100000) $ audioData
      melBs = BSL.toStrict $ (toLazyByteString . execPut) melBsBuil
  v <- asks asrEngine
  confData <- liftIO $ computeConfusionDataFromMelData v melBs
  let found = V.any (any (\(_,s) -> (T.unpack reading) == s)) confData
  liftIO $ T.putStrLn $ showConfData confData
  return $ if found
    then AnswerCorrect
    else AnswerIncorrect $ showConfData confData

showConfData :: ConfusionData -> Text
showConfData cd = T.unlines $ V.toList $ V.imap f cd
  where
    f i ne = "Row " <> tshow i <> ": " <> showNE ne
    showNE :: NE.NonEmpty (Float, [Char]) -> Text
    showNE ne = mconcat $ NE.toList $
      map (\(f,s) -> " <" <> tshow f
        <>  ", " <> T.pack s <> " >, ") ne

tshow :: (Show s) => s -> Text
tshow = T.pack . show

-- doDecode :: ByteString -> AudioRecordedData
-- doDecode bs = loop bs
--   where loop bs = case f bs of
--                     (Done bs _ v) -> V.cons v (loop bs)
--                     _ -> V.empty

--         f bs = runGetIncremental getInt16le `pushChunk` bs
