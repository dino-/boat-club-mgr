{-# LANGUAGE NoImplicitPrelude #-}

module Helper.Membership
  ( emptyMembership
  , membershipForm
  , membershipFormEmpty
  )
  where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), bfs, renderBootstrap3)

import Import
import Model.Types ( MbrNum (..), getMbrNum )


emptyMembership :: Membership
emptyMembership = Membership
  { membershipMbrNum = MbrNum 0
  }


membershipForm :: Membership -> Form Membership
membershipForm mbrsh = do
  renderBootstrap3 BootstrapBasicForm $ Membership
    <$> areq membershipNumField (bfs ("Membership Number" :: Text))
      (Just . membershipMbrNum $ mbrsh)

  where
    membershipNumErrMsg :: Text
    membershipNumErrMsg = "Membership numbers must be > 0"

    membershipNumField = convertField MbrNum getMbrNum
      . checkBool (> 0) membershipNumErrMsg $ intField


membershipFormEmpty :: Form Membership
membershipFormEmpty = membershipForm emptyMembership
