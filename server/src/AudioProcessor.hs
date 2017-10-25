module AudioProcessor where

import Protolude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import Data.Binary.Builder (toLazyByteString)
import Data.Binary
import Data.Binary.Put
import Data.Vector.Binary
import Data.Aeson as Aeson
import Data.Binary.IEEE754

import qualified Data.Vector.Unboxed as VU

type SamplingRate = Int
type MelFilterBank = [Double]
type DataFromClient = (Int, [MelFilterBank])
type AudioFromClient = (Int, VU.Vector Double)

processAudioData :: ByteString -> Handle -> IO ()
processAudioData bs fh = do
  let d = Aeson.decode (BSL.fromStrict bs) :: Maybe AudioFromClient
      count = fst <$> d
      audioD = snd <$> d
  putStrLn $ ("Got Data:" <> show count :: Text)

  let
    buil (Just ad) = Just $ mconcat $ map putInt16le $ map floor
      $ VU.toList ad
    buil Nothing = Nothing

  mapM_ (BSL.hPut fh) ((toLazyByteString . execPut) <$> (buil audioD))
  return ()
-- processAudioData :: ByteString -> Handle -> IO ()
-- processAudioData bs fh = do
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
--   mapM_ (BSL.hPut fh) ((toLazyByteString . execPut) <$> (buil melD))
--   return ()

-- doDecode :: ByteString -> AudioRecordedData
-- doDecode bs = loop bs
--   where loop bs = case f bs of
--                     (Done bs _ v) -> V.cons v (loop bs)
--                     _ -> V.empty

--         f bs = runGetIncremental getInt16le `pushChunk` bs
