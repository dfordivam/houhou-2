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

processAudio :: AudioRecordedData -> ByteString
processAudio (c, d) = BSL.toStrict $
  Aeson.encode ((c,melBankData) :: (Int, [[Double]]))
  where
    !melB = topAPI d
    melB2 = fmap (VU.toList) melB
    melBankData =
      trace ("MelData: " ++ (show $ length melB2)) melB2

computeMel :: AudioRecordedData -> [Float]
computeMel (c,d) = fmap realToFrac $
  mconcat $ filter (\v -> not $ any
     (\f -> (f < 0) || isNaN f || (not $ isIEEE f)) v)
  $ fmap VU.toList $ topAPI d

sendAudio :: AudioRecordedData -> ByteString
sendAudio (c,d) = BSL.toStrict $
  encode (c, floatD)
  where
    floatD :: [Float]
    floatD = map realToFrac $ VU.toList $ untag d

  --
downsampleAudio :: SamplingRate -> VU.Vector Double -> AudioDataFromPCM
downsampleAudio rate d = Tagged $ d
  -- VU.map (\x -> (x * 2^15)) $ d
  -- downsample 3 d
  -- Tagged $ VU.map (\x -> (x * 2^15) / 9)
  --   $ downsample 20 $ cic_interpolate 9 1 2 $ d

-- XXX Do this in JS itself
isValidSample :: VU.Vector Double -> (Bool, Double)
isValidSample vec = (energy > 0.005, energy)
  where energy = (VU.foldl (\x y -> abs x + abs y) 0 vec) /
          (fromIntegral $ VU.length vec)
