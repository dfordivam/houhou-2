{-# LANGUAGE BangPatterns #-}
module AudioProcessor where

import Protolude
import Data.Binary
import Data.Vector.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed as VU
import MFCC
import Data.Tagged
import qualified Data.Aeson as Aeson

type SamplingRate = Float
type AudioRecordedData = (Int, AudioDataFromPCM)

-- downsampleAudio :: SamplingRate -> AudioRecordedData -> AudioRecordedData
-- downsampleAudio rate samp = cic_decimate 320 1 1 $
--   cic_interpolate 147 1 1 samp

processAudio :: AudioRecordedData -> ByteString
processAudio (c, d) = BSL.toStrict $
  Aeson.encode ((c,melBankData) :: (Int, [[Double]]))
  where
    !melB = topAPI d
    melB2 = fmap (VU.toList) melB
    melBankData =
      trace ("MelData: " ++ (show $ length melB2)) melB2

sendAudio :: AudioRecordedData -> ByteString
sendAudio (c,d) = BSL.toStrict $
  encode (c, floatD)
  where
    floatD :: [Float]
    floatD = map realToFrac $ VU.toList $ untag d

downsampleAudio :: SamplingRate -> VU.Vector Double -> AudioDataFromPCM
downsampleAudio rate d = Tagged $ d
  -- VU.map (\x -> (x * 2^15)) $ d
  -- downsample 3 d
  -- Tagged $ VU.map (\x -> (x * 2^15) / 9)
  --   $ downsample 20 $ cic_interpolate 9 1 2 $ d

isValidSample :: VU.Vector Double -> (Bool, Double)
isValidSample vec = (energy > 0.005, energy)
  where energy = (VU.foldl (\x y -> abs x + abs y) 0 vec) /
          (fromIntegral $ VU.length vec)
-- convertPcm :: AudioRecordedData -> [Int16]
-- convertPcm = map (\d -> (fromInteger (floor (d*(2^15)))))

cic_interpolate :: Int -> Int -> Int -> VU.Vector Double -> VU.Vector Double
cic_interpolate r m n vec =
  case VU.length vec of
    0 -> vec
    _ -> ((integrate_chain n) . (upsampleV r) . (comb_chain m n)) vec
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
