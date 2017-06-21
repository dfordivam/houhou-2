{-# LANGUAGE TemplateHaskell #-}

module Model where

import Protolude
import Database.Beam
import Control.Lens.TH

data KanjiT f = Kanji {
    _kanjiId             :: C f (Auto Int)
  , _kanjiCharacter      :: C f (Text)
  , _kanjiStrokeCount    :: C f (Maybe Int)
  , _kanjiGrade          :: C f (Maybe Int)
  , _kanjiMostUsedRank   :: C f (Maybe Int)
  , _kanjiJlptLevel      :: C f (Maybe Int)
  , _kanjiOnyomi         :: C f (Maybe Text)
  , _kanjiKunyomi        :: C f (Maybe Text)
  , _kanjiNanori         :: C f (Maybe Text)
  , _kanjiUnicodeValue   :: C f (Int)
  , _kanjiNewpaperRank   :: C f (Maybe Int)
  , _kanjiWkLevel        :: C f (Maybe Int)
  }
  deriving (Generic)

makeLenses ''KanjiT

type Kanji = KanjiT Identity
deriving instance Show Kanji
deriving instance Show KanjiId

instance Table KanjiT where
    data PrimaryKey KanjiT f = KanjiId (Columnar f (Auto Int)) deriving Generic
    primaryKey = KanjiId . _kanjiId

type KanjiId = PrimaryKey KanjiT Identity

instance Beamable KanjiT
instance Beamable (PrimaryKey KanjiT)

data VocabT f = Vocab {
    _vocabId             :: C f (Auto Int)
  , _vocabKanjiWriting   :: C f (Maybe Text)
  , _vocabKanaWriting    :: C f (Text)
  , _vocabIsCommon       :: C f (Bool)
  , _vocabFreqRank       :: C f (Maybe Int)
  , _vocabFurigana       :: C f (Maybe Text)
  , _vocabJlptLevel      :: C f (Maybe Int)
  , _vocabWkLevel        :: C f (Maybe Int)
  , _vocabWikiRank       :: C f (Maybe Int)
  , _vocabGroupId        :: C f (Int)
  , _vocabIsMain         :: C f (Bool)
  }
  deriving (Generic)

makeLenses ''VocabT

type Vocab = VocabT Identity
deriving instance Show Vocab
deriving instance Show VocabId

instance Table VocabT where
    data PrimaryKey VocabT f = VocabId (Columnar f (Auto Int)) deriving Generic
    primaryKey = VocabId . _vocabId

type VocabId = PrimaryKey VocabT Identity

instance Beamable VocabT
instance Beamable (PrimaryKey VocabT)

data RadicalT f = Radical {
    _radicalId             :: C f (Auto Int)
  , _radicalCharacter      :: C f (Text)
  }
  deriving (Generic)

makeLenses ''RadicalT

type Radical = RadicalT Identity
deriving instance Show Radical
deriving instance Show RadicalId

instance Table RadicalT where
    data PrimaryKey RadicalT f = RadicalId (Columnar f (Auto Int)) deriving Generic
    primaryKey = RadicalId . _radicalId

type RadicalId = PrimaryKey RadicalT Identity

instance Beamable RadicalT
instance Beamable (PrimaryKey RadicalT)

data KanjiRadicalT f = KanjiRadical {
    _kanjiRadicalKanji             :: (PrimaryKey KanjiT f)
  , _kanjiRadicalRadical           :: (PrimaryKey RadicalT f)
  }
  deriving (Generic)

makeLenses ''KanjiRadicalT

type KanjiRadical = KanjiRadicalT Identity
deriving instance Show KanjiRadical
deriving instance Show KanjiRadicalId

instance Table KanjiRadicalT where
    data PrimaryKey KanjiRadicalT f =
      KanjiRadicalId (PrimaryKey KanjiT f) (PrimaryKey RadicalT f) deriving Generic
    primaryKey = KanjiRadicalId <$> _kanjiRadicalKanji <*> _kanjiRadicalRadical

type KanjiRadicalId = PrimaryKey KanjiRadicalT Identity

instance Beamable KanjiRadicalT
instance Beamable (PrimaryKey KanjiRadicalT)

-- KanjiRadicalDB                sql=KanjiRadical
--     kanji         KanjiId     sql=Kanji_ID
--     radical       RadicalId   sql=Radicals_ID
--     Primary kanji radical
--     deriving Show

-- KanjiStrokesDB                sql=KanjiStrokes
--     Id                        sql=ID
--     framesSvg     ByteString  sql=FramesSvg

-- KanjiMeaningDB                sql=KanjiMeaningSet
--     Id                        sql=ID
--     kanji         KanjiId     sql=Kanji_ID
--     language      Text Maybe  sql=Language
--     meaning       Text        sql=Character
--     deriving Show

-- VocabCategoryDB               sql=VocabCategorySet
--     Id                        sql=ID
--     name          Text        sql=ShortName
--     label         Text        sql=Label
--     deriving Show

-- VocabMeaningDB                sql=VocabMeaningSet
--     Id                        sql=ID
--     meaning       Text        sql=Meaning
--     deriving Show

-- VocabEntityVocabMeaningDB     sql=VocabEntityVocabMeaning
--     vocab         VocabId     sql=VocabEntity_ID
--     meaning    VocabMeaningId sql=Meanings_ID
--     Primary vocab meaning
--     deriving Show

-- VocabMeaningVocabCategoryDB   sql=VocabMeaningVocabCategory
--     meaning   VocabMeaningId  sql=VocabMeaningVocabCategory_VocabCategory_ID
--     category  VocabCategoryId sql=Categories_ID
--     Primary meaning category
--     deriving Show

-- VocabCategoryVocabEntityDB    sql=VocabCategoryVocabEntity
--     vocab     VocabId         sql=VocabCategoryVocabEntity_VocabCategory_ID
--     category  VocabCategoryId sql=Categories_ID
--     Primary category vocab
--     deriving Show

-- KanjiEntityVocabEntityDB      sql=KanjiEntityVocabEntity
--     vocab         VocabId     sql=Vocabs_ID
--     kanji         KanjiId     sql=Kanji_ID
--     Primary kanji vocab
--     deriving Show
