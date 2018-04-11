{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Types where

import ClassyPrelude.Yesod


newtype MbrNum = MbrNum { getMbrNum :: Int }
  deriving (Eq, Num, Read, Show)
derivePersistField "MbrNum"
