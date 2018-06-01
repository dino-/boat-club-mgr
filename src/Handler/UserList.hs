{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.UserList where

import Import
import Model.Types ( formatMbrNum )


getUserListR :: Handler Html
getUserListR = do
  eusers <- runDB $ selectList [] [Asc UserSurName, Asc UserGivenName]

  defaultLayout $ do
    deleteDialogMarkup
    deleteDialogCode
    userTable eusers


deleteDialogMarkup :: WidgetFor App ()
deleteDialogMarkup =
  toWidget [hamlet|
    <div class="modal fade" id="delModal" tabindex="-1" role="dialog" aria-laballedby="delModalLabel">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;
            <h4 class="modal-title">Confirm user delete
          <div class="modal-body">
            <p id="delbody">Dummy dialog text
          <div class="modal-footer">
            <button type="button" class="btn btn-default" data-dismiss="modal">Cancel
            <a id="delbutton" href="#" type="button" class="btn btn-primary">Delete
  |]


deleteDialogCode :: WidgetFor App ()
deleteDialogCode =
  toWidget [julius|
    $('#delModal').on('show.bs.modal', function (event) {
      var modal = $(this)

      // Link that triggered the modal
      var link = $(event.relatedTarget)

      // Set the body text
      var userName = link.data('useremail')
      modal.find('#delbody').text(
        "".concat("Delete '", userName, "', are you sure?"))

      // Set the Delete button's url
      var delUrl = link.data('delurl')
      modal.find('#delbutton').attr('href', delUrl)
    })
  |]


userTable :: (MonoFoldable (t (Entity User)), Foldable t) =>
  t (Entity User) -> WidgetFor App ()
userTable eusers =
  [whamlet|
  <p>
  $if null eusers
    <p>No people.
  $else
    <table .table .table-striped>
      <thead>
        <th>Membership #</th><th>First Name</th>
        <th>Last Name</th><th>Email</th><th></th>
      <tbody>
      $forall user <- eusers
        <tr>
          <td>#{formatMbrNum $ userMbrNum $ entityVal user}
          <td>#{userGivenName $ entityVal user}
          <td>#{userSurName $ entityVal user}
          <td>#{userEmail $ entityVal user}
          <td>^{mkMenuCell user}
  |]


mkMenuCell :: Entity User -> WidgetFor App ()
mkMenuCell (Entity userId user) = [whamlet|
    <div .dropdown>
      <span .glyphicon .glyphicon-option-vertical .dropdown-toggle
        type=button #dropdownUser data-toggle=dropdown aria-haspopup=true
        aria-expanded=true aria-hidden=true>
      <ul .dropdown-menu aria-labelledby=dropdownUser>
        <li><a href="@{UserEditR userId}">Edit</a>
        <li><a href="#" data-toggle=modal data-target=#delModal data-useremail="#{userEmail user}" data-delurl=@{UserDeleteR userId}>Delete</a>
  |]
