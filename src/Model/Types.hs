{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Types where

import ClassyPrelude.Yesod


newtype MbrNum = MbrNum Int
  deriving (Eq, Read, Show)
derivePersistField "MbrNum"
