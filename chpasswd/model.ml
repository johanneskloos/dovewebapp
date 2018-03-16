open Database
type db = Database.db
(** Model *)
type level = User | Admin
exception AuthorizationNeeded of level
exception ExternalFailure of string

type authdata = {
  auth_session: string option;
  auth_user: string;
  auth_level: level
}
let need_same_user authdata user =
  if authdata.auth_user <> user && authdata.auth_level <> Admin then
    raise (AuthorizationNeeded User)
let need_admin authdata =
  if authdata.auth_level <> Admin then
    raise (AuthorizationNeeded Admin)

let sql_now = "strftime('%s', 'now')"
let sql_later delay = "strftime('%s', 'now', '" ^ delay ^ "')"

let sql_insert_session =
  "INSERT INTO sessions (sessionid, session_expires, username) " ^
  "VALUES (?, " ^ sql_later !Config.sessions_timeout ^ ", ?)"
let sql_expire_sessions =
  "DELETE FROM sessions WHERE session_expires < " ^ sql_now
let sql_delete_session =
  "DELETE FROM sessions WHERE sessionid = ?"

let sql_retrieve_authorization =
  "SELECT users.username, admin FROM users, sessions " ^
  "WHERE sessionid = ? AND session_expires >= " ^ sql_now
let sql_check_token =
  "SELECT 1 FROM users WHERE username = ? AND token = ? " ^
  "AND token_expires >= " ^ sql_now
let sql_retrieve_token =
  "SELECT token FROM users WHERE username = ? " ^
  "AND token_expires >= " ^ sql_now

let sql_set_password =
  "UPDATE users SET password = ?, token = NULL, token_expires = NULL " ^
  "WHERE username = ?"
let sql_set_alternative_email =
  "UPDATE users SET alternative_email = ? WHERE username = ?"
let sql_set_admin =
  "UPDATE users SET admin = ? WHERE username = ?"
let sql_set_token =
  "UPDATE users SET token = ?, token_expires = " ^
  sql_later !Config.token_lifetime ^ " WHERE username = ?"
let sql_delete_token =
  "UPDATE users SET token = NULL, token_expires = NULL WHERE username = ?"

let sql_delete_user = "DELETE FROM users WHERE username = ?"
let sql_insert_user_password =
  "INSERT INTO users (username, password, alternative_email, admin) " ^
  "VALUES (?, ?, ?, ?)"
let sql_insert_user_token =
  "INSERT INTO users (username, token, alternative_email, admin, token_expires) " ^
  "VALUES (?, ?, ?, ?, " ^ sql_later !Config.token_lifetime ^ ")"

let sql_list_users =
  "SELECT username, token, token_expires, alternative_email, admin " ^
  "FROM users"

let sql_expire_tokens =
  "UPDATE users SET token = NULL, token_expires = NULL " ^
  "WHERE token_expires < " ^ sql_now

let sql_get_email =
  "SELECT alternative_email FROM users WHERE username = ?"

let session_login db ~user ~pass =
  if Doveadm.auth ~user ~pass then
    let token = Token.generate () in
    execute_update db sql_insert_session [str token; str user];
    Some token
  else
    None

let session_logout db session =
  match session.auth_session with
  | Some sessionid ->
    execute_update db sql_delete_session [str sessionid]
  | None -> ()

let session_retrieve db sessionid =
  execute_select_at_most_one db sql_retrieve_authorization [str sessionid]
    (fun stmt ->
       let user = get_str stmt 1
       and admin = get_bool stmt 2
       in { auth_session = Some sessionid;
	    auth_user = user;
	    auth_level = if admin then Admin else User })

let session_from_token db ~user ~token =
  execute_select_at_most_one db sql_check_token [str user; str token]
    (fun _ ->
       { auth_session = None; auth_user = user; auth_level = User })

let user_update_password db session ~user ~pass =
  need_same_user session user;
  match Doveadm.password_encode ~user ~pass with
  | Ok hash -> execute_update db sql_set_password [str hash; str user]
  | Error msg -> raise (ExternalFailure msg)

let user_update_alternative_email db session ~user ~mail =
  need_same_user session user;
  execute_update db sql_set_alternative_email [stropt mail; str user]

let user_delete db session user =
  need_same_user session user;
  execute_update db sql_delete_user [str user]

let user_create_token db user =
  transaction_bracket db @@
  fun db ->
  match execute_select_at_most_one db sql_retrieve_token [str user]
	  (fun stmt -> get_str stmt 1)
  with
  | Some token -> token
  | None ->
    let token = Token.generate () in
    execute_update db sql_set_token [str token; str user];
    token

let user_update_admin db session ~user ~level =
  need_admin session;
  let is_admin = match level with User -> false | Admin -> true in
  execute_update db sql_set_admin [bool is_admin; str user]

let user_delete_token db session user =
  need_admin session;
  execute_update db sql_delete_token [str user]

let user_create_nopw db session ~user ~altemail ~level =
  need_admin session;
  let is_admin = match level with User -> false | Admin -> true
  and token = Token.generate () in
  execute_update db sql_insert_user_token
    [str user; str token; stropt altemail; bool is_admin];
  token

let user_create_pw db session ~user ~pass ~altemail ~level =
  need_admin session;
  let is_admin = match level with User -> false | Admin -> true
  and pass = match Doveadm.password_encode ~user ~pass with
    | Ok hash -> hash
    | Error msg -> raise (ExternalFailure msg)
  in execute_update db sql_insert_user_password
    [str user; str pass; stropt altemail; bool is_admin]

type user_entry = {
  user_name: string;
  user_token: string option;
  user_expires: float option;
  user_alt_email: string option;
  user_level: level
}

let option_map f = function Some x -> Some (f x) | None -> None

let user_list db session =
  need_admin session;
  let user_collect stmt users =
    let user_name = get_str stmt 1
    and user_token = get_stropt stmt 2
    and user_expires = option_map Int64.to_float (get_int64opt stmt 3)
    and user_alt_email = get_stropt stmt 4
    and user_level = if get_bool stmt 4 then Admin else User in
    { user_name; user_token; user_expires; user_alt_email;
      user_level } :: users in
  execute_select db sql_list_users [] user_collect [] |> List.rev

let expire db =
  execute_update db sql_expire_sessions [];
  execute_update db sql_expire_tokens []

type task =
  | TaskSetPassword of { user: string; pass: string }
  | TaskSetEMail of { user: string; mail: string option }
  | TaskCreateToken of string
  | TaskDeleteToken of string
  | TaskSetAdmin of { user: string; level: level }
  | TaskDelete of string
type token_info = { user: string; token: string }
let user_task_run db session tasks =
  need_admin session;
  transaction_bracket db @@
  fun db ->
  List.fold_left (fun tokens ->
      function
      | TaskSetPassword { user; pass } ->
	user_update_password db session ~user ~pass;
	tokens
      | TaskSetEMail { user; mail } ->
	user_update_alternative_email db session ~user ~mail;
	tokens
      | TaskCreateToken user ->
	{ user; token = user_create_token db user } :: tokens
      | TaskDeleteToken user ->
	user_delete_token db session user;
	tokens
      | TaskSetAdmin { user; level } ->
	user_update_admin db session user level;
	tokens
      | TaskDelete user ->
	user_delete db session user;
	tokens)
    [] tasks

let user_get_email db user =
  match
    execute_select_at_most_one db sql_get_email [str user]
      (fun stmt -> get_stropt stmt 1)
  with
  | Some result -> result
  | None -> None

