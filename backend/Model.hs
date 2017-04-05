{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.Sql
import Language.Haskell.TH.Syntax
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

share [mkPersist sqlSettings ]
-- (mkPersistSettings $ ConT ''SqlReadBackend)]
    $(persistFileWith lowerCaseSettings "config/models-readonly")
