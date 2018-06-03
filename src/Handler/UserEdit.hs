{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.UserEdit where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm), bfs
  , renderBootstrap3)

import Import
import Helper.User ( emptyUser )
import Model.Types ( MbrNum (..), getMbrNum )


getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
  muser <- runDB $ get userId
  user <- maybe (redirect UserListR) return muser

  isAdmin' <- isAdminB
  (widget, enctype) <- generateFormPost $ userForm user isAdmin'
  formWrapper widget enctype Nothing userId


postUserEditR :: UserId -> Handler Html
postUserEditR userId = do
  muser <- runDB $ get userId
  user <- maybe (redirect UserListR) return muser

  isAdmin' <- isAdminB
  ((result, widget), enctype) <- runFormPost $ userForm user isAdmin'

  case result of
    FormSuccess userFromForm -> do
      -- Form values must be merged back into the existing record so we don't lose info
      _ <- runDB $ replace userId $ user
        { userMbrNum = userMbrNum userFromForm
        , userGivenName = userGivenName userFromForm
        , userSurName = userSurName userFromForm
        }
      setMessage $ toHtml ("User account successfully edited" :: Text)
      redirect HomeR
    _ -> formWrapper widget enctype (Just "Invalid input, let's try again.") userId


userForm :: User -> Bool -> Form User
userForm user isAdmin' = do
  let disabledAttr = if isAdmin' then [] else [("disabled", "disabled")]
  let fs = bfs ("Membership Number" :: Text)

  renderBootstrap3 BootstrapBasicForm $ User
    <$> aopt membershipNumField (fs { fsAttrs = fsAttrs fs ++ disabledAttr })
      (Just . userMbrNum $ user)
    <*> pure (userEmail emptyUser)
    <*> pure (userPassword emptyUser)
    <*> pure (userVerkey emptyUser)
    <*> pure (userVerified emptyUser)
    <*> areq textField (bfs ("First name" :: Text)) (Just . userGivenName $ user)
    <*> areq textField (bfs ("Last name" :: Text)) (Just . userSurName $ user)

  where
    membershipNumErrMsg :: Text
    membershipNumErrMsg = "Membership numbers must be > 0"

    membershipNumField = convertField MbrNum getMbrNum
      . checkBool (> 0) membershipNumErrMsg $ intField


formWrapper :: Widget -> Enctype -> Maybe Text -> UserId -> HandlerFor App Html
formWrapper widget enctype mbErrMsg userId = defaultLayout
  [whamlet|
    <h3>Edit account info
    $maybe errMsg <- mbErrMsg
      <p>#{errMsg}
    $nothing
      <p>
    <form role=form method=post action=@{UserEditR userId} enctype=#{enctype}>
      ^{widget}
      <a href="@{HomeR}" .btn .btn-default>Cancel
      <button type="submit" .btn .btn-primary>Ok
  |]
