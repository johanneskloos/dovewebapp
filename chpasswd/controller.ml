open View

let cookie_name = "session"

let validate_string pred str =
  try
    for i = 0 to String.length str - 1 do
      if not (pred (String.unsafe_get str i)) then raise Exit
    done;
    true
  with Exit -> false

(* Valid user names follow the RFC2822 dot-atom syntax.
   We are a bit more generous than RFC2822 here and allow
   multiple '.' characters in a row. *)
let valid_user_chars = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '='
  | '?' | '^' | '_' | '`' | '{' | '|' | '}' | '~' | '.' -> true
  | _ -> false

let validate_user str =
  str <> "" && validate_string valid_user_chars str

(* Passwords can have almost any character in them, except
   line breaks and NUL. *)
let valid_pass_chars = function
  | '\x00' | '\r' | '\n' -> false
  | _ -> true

let validate_pass str = validate_string valid_pass_chars str

let validate_email str =
  match Netaddress.parse str with
  | [`Mailbox _] -> true (* We could parse a single e-mail address *)
  | _ -> false
  | exception _ -> false

exception InvalidUser of string
exception InvalidAddress of string
exception InvalidPass
exception ArgumentMissing of string
exception ArgumentEmpty of string

let get_user (cgi: Netcgi.cgi) field =
  try
    let user = (cgi # argument field) # value in
    if validate_user user then user else raise (InvalidUser user)
  with Not_found -> raise (ArgumentMissing field)

let get_nonempty_string_option (cgi: Netcgi.cgi) field =
  try
    let value = (cgi # argument field) # value in
    if value = "" then None else Some value
  with Not_found -> raise (ArgumentMissing field)

let get_nonempty_string cgi field =
  match get_nonempty_string_option cgi field with
  | Some value -> value
  | None -> raise (ArgumentEmpty field)

let get_mail_option (cgi: Netcgi.cgi) field =
  match get_nonempty_string_option cgi field with
  | Some mail ->
    if validate_email mail then Some mail else raise (InvalidAddress mail)
  | None -> None

let get_pass cgi field =
  let pass = get_nonempty_string cgi field in
  if validate_pass pass then pass else raise InvalidPass

let get_admin (cgi: Netcgi.cgi) field =
  let open Model in
  if cgi # argument_exists field then Admin else User

module Make
    (ModelImpl: Model.S)(ViewImpl: View.S with type model = ModelImpl.db) =
struct
  open ViewImpl
  open Model

  let event_login_login db user pass (cgi: Netcgi.cgi) =
    match ModelImpl.session_login db ~user ~pass with
    | None -> ([], view_login db LoginFailed)
    | Some sessionid ->
      match ModelImpl.session_retrieve db sessionid with
      | Some session ->
	([Netcgi.Cookie.make cookie_name sessionid], view_admin db session [])
      | None -> ([], view_login db LoginFailed)

  let event_login_forgot db user =
    let token = ModelImpl.user_create_token db user
    and email = ModelImpl.user_get_email db user in
    Mails.send_token_email ~email ~user ~token;
    view_login db (TokenSent user)

  let event_login db (cgi: Netcgi.cgi) =
    match cgi#argument_value "op" with
    | "login" ->
      event_login_login db
	(get_user cgi "user")
	(get_pass cgi "pass")
	cgi
    | "forgot" ->
      ([], event_login_forgot db (get_user cgi "user"))
    | _ -> ([], view_login db NoMessage)

  let event_admin_logout db session =
    ModelImpl.session_logout db session;
    view_login db NoMessage

  let event_admin_change_password db session pass pass2 =
    if pass <> pass2 then
      view_admin db session [FPasswordMismatch]
    else begin
      ModelImpl.user_update_password db session ~user:session.auth_user ~pass;
      view_admin db session [SUpdPassword session.Model.auth_user]
    end

  let event_admin_change_password_forgot db token session pass pass2 =
    if pass <> pass2 then
      view_forgot_form db ~user:session.Model.auth_user ~token true
    else begin
      ModelImpl.user_update_password db session ~user:session.auth_user ~pass;
      view_admin db session [SUpdPassword session.Model.auth_user]
    end

  let event_admin_change_email db session mail =
    let mail = if mail = "" then None else Some mail in
    ModelImpl.user_update_alternative_email db session ~user:session.auth_user ~mail;
    view_admin db session [SUpdEMail { user = session.Model.auth_user; mail }]

  let event_admin_delete db session confirm =
    if not confirm then
      view_admin db session [FDeleteNotConfirmed session.Model.auth_user]
    else begin
      ModelImpl.user_delete db session session.auth_user;
      event_admin_logout db session
    end

  let event_admin_create db session user pass altemail level =
    if pass = "" then
      let token = ModelImpl.user_create_nopw db session ~user ~altemail ~level
      in match altemail with
      | Some mail ->
	Mails.send_account_email mail token;
	view_admin db session [SCreatedUserSentToken { user; mail; level }]
      | None ->
	view_admin db session [SCreatedUserWithToken { user; token; level }]
    else begin
      ModelImpl.user_create_pw db session ~user ~pass ~altemail ~level;
      view_admin db session [SCreatedUser { user; level }]
    end

  let event_admin_mass_update db session (cgi: Netcgi.cgi) =
    failwith "Not implemented"

  let event_admin db (cgi: Netcgi.cgi) =
    let sessionid =
      cgi # environment # cookie cookie_name |> Netcgi.Cookie.value
    in match ModelImpl.session_retrieve db sessionid with
    | None -> view_login db LoginFailed
    | Some session ->
      match cgi#argument_value "op" with
      | "logout" ->
	event_admin_logout db session
      | "setpw" ->
	event_admin_change_password db session
	  (get_pass cgi "pass")
	  (get_pass cgi "pass2")
      | "setmail" ->
	event_admin_change_email db session (cgi#argument_value "mail")
      | "delete" ->
	event_admin_delete db session (cgi#argument_exists "delete_confirm")
      | "create" ->
	event_admin_create db session
	  (get_user cgi "user")
	  (get_pass cgi "pass")
	  (get_mail_option cgi "mail")
	  (get_admin cgi "admin")
      | "massupdate" ->
	event_admin_mass_update db session cgi
      | _ ->
	view_admin db session []

  let event_forgot db (cgi: Netcgi.cgi) =
    let user = get_user cgi "user"
    and token = get_nonempty_string cgi "token" in
    if cgi#argument_exists "pass" then
      match ModelImpl.session_from_token db ~user ~token with
      | Some auth ->
	event_admin_change_password_forgot db token auth
	  (get_nonempty_string cgi "pass")
	  (get_nonempty_string cgi "pass2")
      | None ->
	view_login db LoginFailed
    else
      view_forgot_form db ~user ~token false

  let db_cached = ref None

  let handle_request (connect: unit -> ModelImpl.db) (cgi: Netcgi.cgi) =
    let db = match !db_cached with
      | Some db -> db
      | None ->
	let db = connect () in
	db_cached := Some db; db
    in try
      let (cookies, page) = match cgi#argument_value "page" with
	| "admin" -> ([], event_admin db cgi)
	| "forgot" -> ([], event_forgot db cgi)
	| _ -> event_login db cgi
      in cgi # set_header
	~status:`Ok 
	~content_type:"text/html"
	~content_length:(String.length page)
	~set_cookies:cookies
	~cache:`No_cache ();
      cgi # out_channel # output_string page;
      cgi # out_channel # commit_work ()
    with e ->
      cgi # set_header
	~status:`Internal_server_error
	~content_type:"text/plain"
	~cache:`No_cache ();
      cgi # out_channel # output_string (Printexc.to_string e);
      cgi # out_channel # commit_work ()
end
