{-# LANGUAGE NoImplicitPrelude #-}

module Model.UserHelper
  ( defaultUser
  )
  where

import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

import Import.NoFoundation
import Model.User ( MembershipNum )


defaultUser :: MembershipNum -> User
defaultUser membershipNum = User
  { userEmail = ""
  , userPassword = Nothing
  , userVerkey = Nothing
  , userVerified = False
  , userMembershipNum = membershipNum
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
