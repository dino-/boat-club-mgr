{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.User
  ( MembershipNum (..)
  )
  where

import ClassyPrelude.Yesod
--import Control.Arrow ( (&&&) )


newtype MembershipNum = MembershipNum Int
  deriving (Eq, Read, Show)
derivePersistField "MembershipNum"
