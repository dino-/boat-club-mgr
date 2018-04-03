{-# LANGUAGE NoImplicitPrelude #-}

module Model.UserHelper
  ( defaultUser
  )
  where

import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import Import.NoFoundation


defaultUser :: User
defaultUser = User
  { userEmail = ""
  , userPassword = Nothing
  , userVerkey = Nothing
  , userVerified = False
  -- , userMemberId = 0  -- Huh?
  , userCreated = posixSecondsToUTCTime . realToFrac $ (0 :: Int)
  , userPrimary = False
  , userGivenName = ""
  , userSurName = ""
  , userPhonePrimary = Nothing
  , userPhoneSecondary = Nothing
  , userStreetAddr = ""
  , userCity = ""
  , userState = ""
  , userZip = ""
  }
