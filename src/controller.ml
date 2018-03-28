open View

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

exception InvalidUser of string
let validate_user str =
  if (str <> "" && validate_string valid_user_chars str) then
    str
  else
    raise (InvalidUser str)

(* Passwords can have almost any character in them, except
   line breaks and NUL. *)
let valid_pass_chars = function
  | '\x00' | '\r' | '\n' -> false
  | _ -> true

exception InvalidPass
let validate_pass str =
  if (validate_string valid_pass_chars str) then
    str
  else
    raise InvalidPass

let validate_pass_opt str =
  match str with
  | Some str -> Some (validate_pass str)
  | None -> None

exception InvalidAddress of string
let validate_email str =
  match Netaddress.parse str with
  | [`Mailbox _] -> str
  | _ -> raise (InvalidAddress str)
  | exception _ -> raise (InvalidAddress str)

let validate_email_option = function
  | None -> None
  | Some str -> Some (validate_email str)

exception ArgumentMissing of string
exception ArgumentEmpty of string

module Make
    (ModelImpl: Model.S)
    (ViewImpl: View.S with type model = ModelImpl.db)
    (MailImpl: Mails.Strategy) =
struct
  open ViewImpl
  open Model
  module Mail = Mails.Make(MailImpl)
  type controller = {
    mailer: MailImpl.t;
    db: ModelImpl.db
  }

  let event_login_login ctrl view user pass =
    match ModelImpl.session_login ctrl.db ~user ~pass with
    | None -> view_login ctrl.db view LoginFailed
    | Some sessionid ->
      match ModelImpl.session_retrieve ctrl.db sessionid with
      | Some session ->
        view_open_session view sessionid;
        view_admin ctrl.db view session []
      | None -> view_login ctrl.db view LoginFailed

  let send_token_email ctrl ~user ~token =
    let email = ModelImpl.user_get_email ctrl.db user in
    Mail.send_token_email ctrl.mailer ~email ~user ~token

  let event_login_forgot ctrl view user =
    let token = ModelImpl.user_create_token ctrl.db user in
    send_token_email ctrl user token;
    view_login ctrl.db view (TokenSent user)

  let event_login ctrl cgi=
    match get_login_operation cgi with
    | Login ->
      event_login_login ctrl cgi
        (validate_user (get_login_user cgi))
        (validate_pass (get_login_pass cgi))
    | Forgot ->
      event_login_forgot ctrl cgi (validate_user (get_login_user cgi))
    | NoOperation -> view_login ctrl.db cgi NoMessage

  let event_admin_logout ctrl view session =
    ModelImpl.session_logout ctrl.db session;
    view_close_session view;
    view_login ctrl.db view NoMessage

  let event_admin_change_password ctrl view session pass pass2 =
    if pass <> pass2 then
      view_admin ctrl.db view session [FPasswordMismatch]
    else begin
      ModelImpl.user_update_password
        ctrl.db session ~user:session.auth_user ~pass;
      view_admin ctrl.db view session [SUpdPassword session.Model.auth_user]
    end

  let event_admin_change_password_forgot ctrl view token session pass pass2 =
    if pass <> pass2 then
      view_forgot_form ctrl.db view ~user:session.Model.auth_user ~token true
    else begin
      ModelImpl.user_update_password
        ctrl.db session ~user:session.auth_user ~pass;
      view_admin ctrl.db view session [SUpdPassword session.Model.auth_user]
    end

  let event_admin_change_email ctrl view session mail =
    ModelImpl.user_update_alternative_email
      ctrl.db session ~user:session.auth_user ~mail;
    view_admin ctrl.db view session
      [SUpdEMail { user = session.Model.auth_user; mail }]

  let event_admin_delete ctrl view session confirm =
    if not confirm then
      view_admin ctrl.db view session
        [FDeleteNotConfirmed session.Model.auth_user]
    else begin
      ModelImpl.user_delete ctrl.db session session.auth_user;
      event_admin_logout ctrl view session
    end

  let event_admin_create ctrl view session user pass altemail level =
    Model.need_admin session;
    match pass with
    | None ->
      let token = ModelImpl.user_create_nopw
          ctrl.db session ~user ~altemail ~level
      in begin match altemail with
        | Some mail ->
          Mail.send_account_email ctrl.mailer mail token;
          view_admin ctrl.db view session
            [SCreatedUserSentToken { user; mail; level }]
        | None ->
          view_admin ctrl.db view session
            [SCreatedUserWithToken { user; token; level }]
      end
    | Some pass  ->
      ModelImpl.user_create_pw ctrl.db session ~user ~pass ~altemail ~level;
      view_admin ctrl.db view session [SCreatedUser { user; level }]

  let event_admin_mass_update ctrl view session tasks =
    Model.need_admin session;
    let tokens = ModelImpl.user_task_run ctrl.db session tasks
    and translate = function
      | TaskSetPassword { user } -> SUpdPassword user
      | TaskSetEMail { user; mail } -> SUpdEMail { user; mail }
      | TaskCreateToken user -> SSentToken user
      | TaskDeleteToken user -> SDeletedToken user
      | TaskSetAdmin { user; level } -> SUpdAdmin { user; level }
      | TaskDelete user -> SDeletedUser user
    in
    List.iter (fun {user; token} -> send_token_email ctrl ~user ~token)
      tokens;
    view_admin ctrl.db view session (List.map translate tasks)

  let event_admin ctrl cgi =
    let sessionid = get_admin_sessionid cgi in
    match ModelImpl.session_retrieve ctrl.db sessionid with
    | None -> view_login ctrl.db cgi LoginFailed
    | Some session ->
      match get_admin_operation cgi with
      | Logout ->
        event_admin_logout ctrl cgi session
      | SetPass ->
        event_admin_change_password ctrl cgi session
          (validate_pass (get_admin_chpass_pass1 cgi))
          (validate_pass (get_admin_chpass_pass2 cgi))
      | SetMail ->
        event_admin_change_email ctrl cgi session
          (validate_email_option (get_admin_chmail_mail cgi))
      | Delete ->
        event_admin_delete ctrl cgi session
          (get_admin_delete_confirm cgi)
      | Create ->
        event_admin_create ctrl cgi session
          (validate_user (get_admin_create_user cgi))
          (validate_pass_opt (get_admin_create_pass cgi))
          (validate_email_option (get_admin_create_mail cgi))
          (get_admin_create_level cgi)
      | MassUpdate ->
        event_admin_mass_update ctrl cgi session
          (get_admin_mass_update cgi)
      | NoOperation ->
        view_admin ctrl.db cgi session []

  let event_forgot ctrl cgi =
    let user = validate_user (get_forgot_user cgi)
    and token = get_forgot_token cgi in
    match get_forgot_pass1 cgi, get_forgot_pass2 cgi with
    | None, _ | _, None ->
      view_forgot_form ctrl.db cgi ~user ~token false
    | Some pass1, Some pass2 when pass1 = pass2 ->
      begin match ModelImpl.session_from_token ctrl.db ~user ~token with
        | Some auth ->
          event_admin_change_password_forgot ctrl cgi token auth
            (validate_pass pass1) (validate_pass pass2)
        | None ->
          view_login ctrl.db cgi LoginFailed
      end
    | Some pass1, pass2 ->
      view_forgot_form ctrl.db cgi ~user ~token true

end
