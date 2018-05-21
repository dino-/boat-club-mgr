{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Types
  ( MbrNum (..)
  , formatMbrNum
  )
  where

import ClassyPrelude.Yesod


newtype MbrNum = MbrNum { getMbrNum :: Int }
  deriving (Eq, Num, Read, Show)
derivePersistField "MbrNum"


formatMbrNum :: Maybe MbrNum -> Text
formatMbrNum (Just (MbrNum n)) = tshow n
formatMbrNum Nothing = "-"
