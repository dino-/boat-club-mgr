{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.User


{-
adminMembershipNum :: Int
adminMembershipNum = 0
-}
adminMembershipNum :: MembershipNum
adminMembershipNum = MembershipNum 0


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
