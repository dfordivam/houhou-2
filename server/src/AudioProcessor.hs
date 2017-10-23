module AudioProcessor where

import Protolude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import Data.Binary.Builder
import Data.Binary.Get

import qualified Data.Vector.Unboxed as V

type SamplingRate = Int
type AudioRecordedData = V.Vector Int16

processAudioData :: ByteString -> Handle -> IO ()
processAudioData bs fh = do
  putStrLn $ ("Processing Audio Data" :: Text)
  -- print bs
  let
      -- (vec,count) = (\x -> (fst <$> x , snd <$> x)) $ decodeStrict bs
      -- downSam = downsampleAudio 1 <$> vec
      -- vec = Just $ doDecode bs
      -- pcmData = vec -- convertPcm <$> downSam
      -- pcmBs = (BSL.toStrict . BS.toLazyByteString . mconcat . ( map BS.int16LE)) <$> pcmData
      -- pcmBuil = (mconcat . ( map BS.int16LE)) <$> pcmData
  -- putStrLn $ ("Count " <> show (count :: Maybe Int) :: Text)
  -- mapM_ (BS.hPutBuilder fh ) pcmBuil
  -- mapM_ (const ( putStrLn $ ("Processed Bytes" :: Text))) pcmBs
  return ()

doDecode :: ByteString -> AudioRecordedData
doDecode bs = loop bs
  where loop bs = case f bs of
                    (Done bs _ v) -> V.cons v (loop bs)
                    _ -> V.empty

        f bs = runGetIncremental getInt16le `pushChunk` bs

downsampleAudio :: SamplingRate -> AudioRecordedData -> AudioRecordedData
downsampleAudio rate samp = downsample 20 $
  V.map (\x -> floor $ x /9) $ cic_interpolate 9 1 2 $ V.map (fromInteger . toInteger) samp

-- convertPcm :: AudioRecordedData -> [Int16]
-- convertPcm = map (\d -> (fromInteger (floor (d*(2^15)))))

cic_interpolate :: Int -> Int -> Int -> V.Vector Double -> V.Vector Double
cic_interpolate r m n vec =
  case V.length vec of
    0 -> vec
    _ -> ((integrate_chain n) . (upsampleV r) . (comb_chain m n)) vec
integrate_chain n = apply integrateV n
comb_chain m n = apply (combV m) n

combV :: Int -> V.Vector Double -> V.Vector Double
combV m vec = V.zipWith (-) vec delayedVec
  where delayedVec = (V.++) (V.replicate m 0.0) vec

integrateV :: V.Vector Double -> V.Vector Double
integrateV vec = V.postscanl' (+) 0 vec

-- | @upsample@ inserts n-1 zeros between each sample, eg,
--
-- @upsample 2 [ 1, 2, 3 ] == [ 1, 0, 2, 0, 3, 0 ]@

upsampleV :: Int -> V.Vector Double -> V.Vector Double
upsampleV n vec = V.update zeroVec indVec
  where zeroVec = V.replicate (n * (V.length vec)) 0.0
        indVec = V.imap (\i a -> (n * i, a)) vec

apply :: (a -> a) -> Int -> (a -> a)
apply f 1 = f
apply f n = f . apply f (n - 1)

-- | @downsample@ throws away every n'th sample, eg,
--
-- @downsample 2 [ 1, 2, 3, 4, 5, 6 ] == [ 1, 3, 5 ]@

downsample n vec = V.generate len genF
  where len = floor ((fromIntegral $ V.length vec)/(fromIntegral n))
        genF i = (V.!) vec (i * n)
