{-# LANGUAGE QuasiQuotes #-}

module Authentication
  ( ourEmailLoginHandler
  , ourForgotPasswordHandler
  )
  where

import Control.Applicative ( (<$>), (<*>) )
import Data.Text (Text)
import Yesod.Auth
import Yesod.Auth.Email
import qualified Yesod.Auth.Message as Msg
import Yesod.Core
import Yesod.Form


data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }


ourEmailLoginHandler :: YesodAuthEmail master => (Route Auth -> Route master) -> WidgetT master IO ()
ourEmailLoginHandler toParent = do
  (widget, enctype) <- liftWidgetT $ generateFormPost loginForm

  [whamlet|
    <form method="post" action="@{toParent loginR}", enctype=#{enctype}>
      <div id="emailLoginForm">
        ^{widget}
        <div>
          <button type=submit .btn .btn-success>
            _{Msg.LoginViaEmail}
          &nbsp;
          <a href="@{toParent registerR}" .btn .btn-default>
            _{Msg.RegisterLong}
          &nbsp;
          <a href="@{toParent forgotPasswordR}" .btn .btn-default>
            Forgot password
  |]

  where
    loginForm extra = do

      emailMsg <- renderMessage' Msg.Email
      (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

      passwordMsg <- renderMessage' Msg.Password
      (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

      let userRes = UserLoginForm Control.Applicative.<$> emailRes
            Control.Applicative.<*> passwordRes
      let widget = do
            [whamlet|
              #{extra}
              <div>
                ^{fvInput emailView}
              <div>
                ^{fvInput passwordView}
            |]

      return (userRes, widget)

    emailSettings emailMsg = do
      FieldSettings {
        fsLabel = SomeMessage Msg.Email,
        fsTooltip = Nothing,
        fsId = Just "email",
        fsName = Just "email",
        fsAttrs = [("autofocus", ""), ("placeholder", emailMsg)]
      }
    passwordSettings passwordMsg =
      FieldSettings {
        fsLabel = SomeMessage Msg.Password,
        fsTooltip = Nothing,
        fsId = Just "password",
        fsName = Just "password",
        fsAttrs = [("placeholder", passwordMsg)]
      }
    renderMessage' msg = do
      langs <- languages
      master <- getYesod
      return $ renderAuthMessage master langs msg


data ForgotPasswordForm = ForgotPasswordForm { _forgotEmail :: Text }


ourForgotPasswordHandler :: YesodAuthEmail master => HandlerT Auth (HandlerT master IO) Html
ourForgotPasswordHandler = do
  (widget, enctype) <- lift $ generateFormPost forgotPasswordForm
  toParent <- getRouteToParent

  lift $ authLayout $ do
    setTitleI Msg.PasswordResetTitle
    [whamlet|
      <p>Enter your e-mail address below, and a password reset e-mail will be sent to you.
      <form method=post action=@{toParent forgotPasswordR} enctype=#{enctype}>
        <div id="forgotPasswordForm">
          ^{widget}
          <button .btn .btn-success>_{Msg.SendPasswordResetEmail}
    |]

  where
    forgotPasswordForm extra = do
      (emailRes, emailView) <- mreq emailField emailSettings Nothing

      let forgotPasswordRes = ForgotPasswordForm <$> emailRes
      let widget = do
            [whamlet|
              #{extra}
              ^{fvLabel emailView}
              ^{fvInput emailView}
            |]
      return (forgotPasswordRes, widget)

    emailSettings =
      FieldSettings {
        fsLabel = SomeMessage Msg.Email,
        fsTooltip = Nothing,
        fsId = Just "forgotPassword",
        fsName = Just "email",
        fsAttrs = [("autofocus", "")]
      }

