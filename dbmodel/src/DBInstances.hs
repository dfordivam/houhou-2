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
