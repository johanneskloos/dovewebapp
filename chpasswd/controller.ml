open View

let cookie_name = "session"

let event_login_login db user pass (cgi: Netcgi.cgi) =
  match Model.session_login db ~user ~pass with
  | None -> view_login db LoginFailed
  | Some sessionid ->
    match Model.session_retrieve db sessionid with
    | Some session ->
      cgi # set_header ~set_cookies:[Netcgi.Cookie.make cookie_name sessionid] ();
      view_admin db session []
    | None -> view_login db LoginFailed

let event_login_forgot db user =
  let token = Model.user_create_token db user in
  Mails.send_token_email db ~user ~token;
  view_login db (TokenSent user)

let event_login db (cgi: Netcgi.cgi) =
  match cgi#argument_value "op" with
  | "login" ->
    event_login_login db
      (cgi#argument_value "user")
      (cgi#argument_value "pass")
      cgi
  | "forgot" ->
    event_login_forgot db (cgi#argument_value "user")
  | _ -> raise Not_found

let event_admin_logout db session =
  Model.session_logout db session;
  view_login db NoMessage

let event_admin_change_password db session pass pass2 =
  if pass <> pass2 then
    view_admin db session [FPasswordMismatch]
  else begin
    Model.user_update_password db session ~user:session.auth_user ~pass;
    view_admin db session [SUpdPassword session.auth_user]
  end

let event_admin_change_email db session mail =
  let mail = if mail = "" then None else Some mail in
  Model.user_update_alternative_email db session ~user:session.auth_user ~mail;
  view_admin db session [SUpdEMail { user = session.auth_user; mail }]

let event_admin_delete db session confirm =
  if not confirm then
    view_admin db session [FDeleteNotConfirmed session.Model.auth_user]
  else begin
    Model.user_delete db session session.auth_user;
    event_admin_logout db session
  end

let event_admin_create db session user pass email admin =
  let altemail = if email = "" then None else Some email
  and level = if admin then Model.Admin else Model.User in
  if pass = "" then
    let token = Model.user_create_nopw db session ~user ~altemail ~level
    in match altemail with
    | Some mail ->
      Mails.send_account_email mail token;
      view_admin db session [SCreatedUserSentToken { user; mail; level }]
    | None ->
      view_admin db session [SCreatedUserWithToken { user; token; level }]
  else begin
    Model.user_create_pw db session ~user ~pass ~altemail ~level;
    view_admin db session [SCreatedUser { user; level }]
  end

let event_admin_mass_update db session (cgi: Netcgi.cgi) =
  failwith "Not implemented"

let event_admin db (cgi: Netcgi.cgi) =
  let sessionid =
    cgi # environment # cookie cookie_name |> Netcgi.Cookie.value
  in match Model.session_retrieve db sessionid with
  | None -> view_login db LoginFailed
  | Some session ->
    match cgi#argument_value "op" with
    | "logout" ->
      event_admin_logout db session
    | "setpw" ->
      event_admin_change_password db session
	(cgi#argument_value "pass")
	(cgi#argument_value "pass2")
    | "setmail" ->
      event_admin_change_email db session (cgi#argument_value "mail")
    | "delete" ->
      event_admin_delete db session (cgi#argument_exists "delete_confirm")
    | "create" ->
      event_admin_create db session
	(cgi#argument_value "user")
	(cgi#argument_value "pass")
	(cgi#argument_value "mail")
	(cgi#argument_exists "admin")
    | "massupdate" ->
      event_admin_mass_update db session cgi
    | _ ->
      view_admin db session []
