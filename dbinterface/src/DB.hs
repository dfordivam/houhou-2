module DB where

import Model
import DBInstances

import Protolude
import Database.Beam

data JMdictDb f = JMdictDb {
  _jmdictKanji :: f (TableEntity KanjiT)
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
  }
