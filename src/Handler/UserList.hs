{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.UserList where

import Colonnade ( Colonnade, Headed, headed )
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Colonnade ( Cell, encodeCellTable, textCell )

import Import
import Model.Types ( formatMbrNum )


userTable :: Colonnade Headed (Entity User) Cell
userTable = mconcat
  [ headed "Membership #" (textCell . formatMbrNum . userMbrNum . entityVal)
  , headed "First Name"   (textCell . userGivenName . entityVal)
  , headed "Last Name"    (textCell . userSurName . entityVal)
  , headed "Email"        (textCell . userEmail . entityVal)
  ]


getUserListR :: Handler Html
getUserListR = do
  eusers <- runDB $ selectList [] [Asc UserSurName, Asc UserGivenName]

  defaultLayout $ do
    [whamlet|
    <p>
    ^{encodeCellTable (HA.class_ "table table-striped") userTable eusers}
    |]
