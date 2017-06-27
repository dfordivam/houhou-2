module Utils where

import Protolude
import qualified Data.Text as T
import Data.Char

-- Hiragana ( 3040 - 309f)
-- Katakana ( 30a0 - 30ff)
--  Full-width roman characters and half-width katakana ( ff00 - ffef)
--   CJK unifed ideographs - Common and uncommon kanji ( 4e00 - 9faf)
--   CJK unified ideographs Extension A - Rare kanji ( 3400 - 4dbf)

-- Filter valid Kanji (no hiragana or katakana)
getKanjis :: Text -> [Text]
getKanjis inp = map T.pack $ map (:[]) $ filter isKanji $ T.unpack inp
  where isKanji c = c > l && c < h
        l = chr $ 13312 -- 0x3400
        h = chr $ 40879 -- 0x9faf
