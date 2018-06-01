{-# LANGUAGE NoImplicitPrelude #-}

module Helper.UserForm
  ( userForm
  , userFormEmpty
  )
  where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), bfs, renderBootstrap3)

import Import
import Helper.User ( emptyUser )
import Model.Types ( MbrNum (..), getMbrNum )


userForm :: User -> Form User
userForm user = do
  renderBootstrap3 BootstrapBasicForm $ User
    <$> aopt membershipNumField (bfs ("Membership Number" :: Text))
      (Just . userMbrNum $ user)
    <*> pure (userEmail user)
    <*> pure (userPassword user)
    <*> pure (userVerkey user)
    <*> pure (userVerified user)
    <*> areq textField (bfs ("Given name" :: Text)) (Just . userGivenName $ user)
    <*> areq textField (bfs ("Surname" :: Text)) (Just . userSurName $ user)

  where
    membershipNumErrMsg :: Text
    membershipNumErrMsg = "Membership numbers must be > 0"

    membershipNumField = convertField MbrNum getMbrNum
      . checkBool (> 0) membershipNumErrMsg $ intField


userFormEmpty :: Form User
userFormEmpty = userForm emptyUser
