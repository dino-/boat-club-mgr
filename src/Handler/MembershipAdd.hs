{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.MembershipAdd
  ( getMembershipAddR, postMembershipAddR )
  where

import Import
import Helper.Membership ( emptyMembership, membershipForm,
  membershipFormEmpty )


getMembershipAddR :: Handler Html
getMembershipAddR = do
  -- Get membership number one past the highest in the db at this time
  mbEntity <- runDB $ selectFirst [] [Desc MembershipMbrNum]
  let nextAvailMbrNum = (maybe adminMbrNum (+ 1)) $
        membershipMbrNum . entityVal <$> mbEntity

  (widget, enctype) <- generateFormPost $ membershipForm
    (emptyMembership { membershipMbrNum = nextAvailMbrNum })
  formWrapper widget enctype Nothing


postMembershipAddR :: Handler Html
postMembershipAddR = do
  ((result, widget), enctype) <- runFormPost membershipFormEmpty
  case result of
    FormSuccess membership -> do
      membershipExists <- runDB $ (> 0) <$> count [MembershipMbrNum ==. membershipMbrNum membership]
      if membershipExists
        then formWrapper widget enctype $ Just "A membership with this number already exists, let's try again."
        else do
          runDB $ insert_ membership
          setMessage $ toHtml ("New membership successfully added" :: Text)
          redirect HomeR
    _ -> formWrapper widget enctype $ Just "Invalid input, let's try again."


formWrapper :: Widget -> Enctype -> Maybe Text -> HandlerFor App Html
formWrapper widget enctype mbErrMsg = defaultLayout
  [whamlet|
    <h3>Add a new membership
    $maybe errMsg <- mbErrMsg
      <p>#{errMsg}
    $nothing
      <p>
    <form role=form method=post action=@{MembershipAddR} enctype=#{enctype}>
      ^{widget}
      <a href="@{HomeR}" .btn .btn-default>Cancel
      <button type="submit" .btn .btn-primary>Ok
  |]
