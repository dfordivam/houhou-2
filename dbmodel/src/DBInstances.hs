module DBInstances where

import Model

import Protolude
import Database.Beam

instance Table KanjiT where
    data PrimaryKey KanjiT f = KanjiId (Columnar f (Auto Int)) deriving Generic
    primaryKey = KanjiId . _kanjiId

type KanjiId = PrimaryKey KanjiT Identity

instance Beamable KanjiT
instance Beamable (PrimaryKey KanjiT)

instance Table VocabT where
    data PrimaryKey VocabT f = VocabId (Columnar f (Auto Int)) deriving Generic
    primaryKey = VocabId . _vocabId

type VocabId = PrimaryKey VocabT Identity

instance Beamable VocabT
instance Beamable (PrimaryKey VocabT)
