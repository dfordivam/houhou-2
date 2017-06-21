module DB where

import Model

import Protolude
import Database.Beam

data JMdictDb f = JMdictDb {
  _jmdictKanji :: f (TableEntity KanjiT)
  , _jmdictVocab :: f (TableEntity VocabT)
  , _jmdictRadical :: f (TableEntity RadicalT)
  , _jmdictKanjiRadical :: f (TableEntity KanjiRadicalT)
  }
  deriving (Generic)

instance Database JMdictDb

jmdictDb :: DatabaseSettings be JMdictDb
jmdictDb = defaultDbSettings `withDbModification`
  dbModification
  { _jmdictKanji = modifyTable (\_ -> "KanjiSet") $
      tableModification
      {
         _kanjiId             = fieldNamed "ID"
       , _kanjiCharacter      = fieldNamed "Character"
       , _kanjiStrokeCount    = fieldNamed "StrokeCount"
       , _kanjiGrade          = fieldNamed "Grade"
       , _kanjiMostUsedRank   = fieldNamed "MostUsedRank"
       , _kanjiJlptLevel      = fieldNamed "JlptLevel"
       , _kanjiOnyomi         = fieldNamed "OnYomi"
       , _kanjiKunyomi        = fieldNamed "KunYomi"
       , _kanjiNanori         = fieldNamed "Nanori"
       , _kanjiUnicodeValue   = fieldNamed "UnicodeValue"
       , _kanjiNewpaperRank   = fieldNamed "NewspaperRank"
       , _kanjiWkLevel        = fieldNamed "WkLevel"
      }
  , _jmdictVocab = modifyTable (\_ -> "VocabSet") $
      tableModification
      {
         _vocabId             = fieldNamed "ID"
       , _vocabKanjiWriting   = fieldNamed "KanjiWriting"
       , _vocabKanaWriting    = fieldNamed "KanaWriting"
       , _vocabIsCommon       = fieldNamed "IsCommon"
       , _vocabFreqRank       = fieldNamed "FrequencyRank"
       , _vocabFurigana       = fieldNamed "Furigana"
       , _vocabJlptLevel      = fieldNamed "JlptLevel"
       , _vocabWkLevel        = fieldNamed "WkLevel"
       , _vocabWikiRank       = fieldNamed "WikiRank"
       , _vocabGroupId        = fieldNamed "GroupId"
       , _vocabIsMain         = fieldNamed "IsMain"
      }
  , _jmdictRadical = modifyTable (\_ -> "RadicalSet") $ tableModification
      {
         _radicalId             = fieldNamed "ID"
       , _radicalCharacter      = fieldNamed "Character"
      }
  , _jmdictKanjiRadical = modifyTable (\_ -> "KanjiRadical") $ tableModification
      {
         _kanjiRadicalKanji     = KanjiId $ fieldNamed "Kanji_ID"
       , _kanjiRadicalRadical   = RadicalId $ fieldNamed "Radicals_ID"
      }
  }
