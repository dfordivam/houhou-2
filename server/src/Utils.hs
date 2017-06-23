module Utils where

-- Hiragana ( 3040 - 309f)
-- Katakana ( 30a0 - 30ff)
--  Full-width roman characters and half-width katakana ( ff00 - ffef)
--   CJK unifed ideographs - Common and uncommon kanji ( 4e00 - 9faf)
--   CJK unified ideographs Extension A - Rare kanji ( 3400 - 4dbf)

-- Filter valid Kanji (no hiragana or katakana)
getKanjis :: Text -> [Text]
getKanjis
