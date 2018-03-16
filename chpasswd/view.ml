type login_messages = TokenSent of string | LoginFailed | NoMessage
let view_login db msg =
  let msg_text = match msg with
    | NoMessage -> ""
    | LoginFailed -> "<span class=\"failure\">Login failed!</span>"
    | TokenSent user ->
      "<span class=\"info\">Password reset token sent to " ^ user ^ ".</span>"
  in
  Template.from_file ~models:[
    ("message", Jg_types.Tstr msg_text)
  ] "login.html"

type admin_messages =
    SUpdPassword of string
  | SUpdEMail of { user: string; mail: string option }
  | SUpdAdmin of { user: string; level: Model.level }
  | SSentToken of string
  | SDeletedToken of string
  | SCreatedUser of { user: string; level: Model.level }
  | SCreatedUserSentToken of
      { user: string; mail: string; level: Model.level }
  | SCreatedUserWithToken of
      { user: string; token: string; level: Model.level }
  | SDeletedUser of string
  | FDatabase of string
  | FAuth of Model.level
  | FExternal of string
  | FDeleteAllAdmin
  | FDeleteCurrent
  | FDeleteNotConfirmed of string
  | FPasswordMismatch

type message_type = Info of string | Error of string

let level_to_string =
  function Model.User -> "regular user" | Admin -> "administrator"

let format_message = function
  | SUpdPassword user ->
    Info ("Updated pasword for " ^ user)
  | SUpdEMail { user; mail = Some mail } ->
    Info ("Updated alternative e-mail for " ^ user ^ " to " ^ mail)
  | SUpdEMail { user; mail = None } ->
    Info ("Removed alternative e-mail for " ^ user)
  | SUpdAdmin { user; level = User } ->
    Info ("Made " ^ user ^ " a regular user")
  | SUpdAdmin { user; level = Admin } ->
    Info ("Made " ^ user ^ " an administrator")
  | SSentToken user ->
    Info ("Sent password reset token to " ^ user)
  | SDeletedToken user ->
    Info ("Deleted password reset token for " ^ user)
  | SCreatedUser { user; level } ->
    Info ("Created user " ^ user ^ "(password set, " ^
	  level_to_string level ^")")
  | SCreatedUserSentToken { user; mail; level } ->
    Info ("Created user " ^ user ^
	  "(password reset mail sent to " ^ mail ^ ", " ^
	  level_to_string level ^")")
  | SCreatedUserWithToken { user; token; level } ->
    Info ("Created user " ^ user ^
	  "(token: <span class=\"token\">" ^ token ^
	  "</span>, " ^ level_to_string level ^")")
  | SDeletedUser user ->
    Info ("Deleted user " ^ user)
  | FDatabase err ->
    Error ("Database error: " ^ err)
  | FExternal err ->
    Error ("Error in external program: " ^ err)
  | FDeleteAllAdmin -> Error "Would delete all admin accounts"
  | FDeleteCurrent -> Error "Would delete loggied-in account"
  | FDeleteNotConfirmed user ->
    Error ("Deletion of " ^ user ^ " not confirmed")
  | FPasswordMismatch ->
    Error "Passwords do not match"
  | FAuth Model.User ->
    Error "Not logged in as correct user or administrator"
  | FAuth Model.Admin ->
    Error "This operation needs administrator privleges"

let format_messages msgs =
  let open Jg_types in
  let (infos, errors) =
    List.fold_left (fun (infos, errors) msg ->
	match format_message msg with
	| Info info -> (info :: infos, errors)
	| Error err -> (infos, err :: errors))
      ([], []) msgs in
  [("infos", Tlist (List.map (fun s -> Tstr s) infos));
   ("errors", Tlist (List.map (fun s -> Tstr s) errors))]

let format_users users =
  let open Jg_types in
  let open Model in
  let format_user
      { user_name; user_token; user_expires; user_alt_email; user_level }=
    Tobj [
      ("user", Tstr user_name);
      ("email", match user_alt_email with
	| Some addr -> Tstr addr
	| None -> Tnull);
      ("level", match user_level with
	| User -> Tstr "user"
	| Admin -> Tstr "admin");
      ("token", match user_token, user_expires with
	| Some token, Some timeout ->
	  Tobj [("token", Tstr token);
		("expires", Tstr ("TBD"))]
	| None, None -> Tnull
	| _, _ -> Tobj ["message", Tstr "inconsistent token state"])
    ]
  in [("users", Tlist (List.map format_user users))]

let view_admin_user db user msgs =
  Template.from_file ~models:([
      ("user", Jg_types.Tstr user);
      ("alt_email", match Model.user_get_email db user with
	| Some addr -> Jg_types.Tstr addr
	| None -> Jg_types.Tnull)
    ] @ format_messages msgs) "admin_user.html"

let view_admin_admin db user msgs users =
  Template.from_file ~models:([
      ("user", Jg_types.Tstr user);
      ("alt_email", match Model.user_get_email db user with
	| Some addr -> Jg_types.Tstr addr
	| None -> Jg_types.Tstr "")
    ] @ format_messages msgs @ format_users users) "admin_admin.html"

let view_admin db auth msgs =
  let open Model in
  match auth.auth_level with
  | User -> view_admin_user db auth.auth_user msgs
  | Admin -> view_admin_admin db auth.auth_user msgs
	       (Model.user_list db auth)

