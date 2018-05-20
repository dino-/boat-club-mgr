{-# LANGUAGE NoImplicitPrelude #-}

module Helper.User
  ( emptyUser
  )
  where

import Import.NoFoundation


emptyUser :: User
emptyUser = User
  { userMbrNum = Nothing
  , userEmail = ""
  , userPassword = Nothing
  , userVerkey = Nothing
  , userVerified = False
  , userGivenName = ""
  , userSurName = ""
  }
