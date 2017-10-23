module AudioProcessor where

import Protolude
import DSP.Multirate.CIC
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as BSL

type SamplingRate = Int
type AudioRecordedData = ([Int16], Int)

-- downsampleAudio :: SamplingRate -> AudioRecordedData -> AudioRecordedData
-- downsampleAudio rate samp = cic_decimate 320 1 1 $
--   cic_interpolate 147 1 1 samp

processAudio :: AudioRecordedData -> ByteString
processAudio (d, c) = BSL.toStrict $ toLazyByteString
  $ mconcat $ map putInt16le d
