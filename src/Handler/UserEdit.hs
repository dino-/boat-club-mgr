{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.UserEdit where

import Import
import Helper.UserForm ( userForm, userFormEmpty )


getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
  mUser <- runDB $ get userId
  user <- maybe (redirect UserListR) return mUser

  (widget, enctype) <- generateFormPost $ userForm user
  formWrapper widget enctype Nothing userId


postUserEditR :: UserId -> Handler Html
postUserEditR userId = do
  ((result, widget), enctype) <- runFormPost $ userFormEmpty
  case result of
    FormSuccess user -> do
      _ <- runDB $ replace userId user
      redirect HomeR
    _ -> formWrapper widget enctype (Just "Invalid input, let's try again.") userId


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
