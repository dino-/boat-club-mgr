module Handler.UserDelete where

import Import


getUserDeleteR :: UserId -> Handler Html
getUserDeleteR userId = do
  runDB $ delete userId
  redirect UserListR
