open View
type message_type =
    Info of string * (string * Jg_types.tvalue) list
  | Error of string * (string * Jg_types.tvalue) list

let level_to_string =
  function Model.User -> "user" | Admin -> "admin"
let level_to_tvalue =
  let open Jg_types in
  function Model.User -> Tbool false | _ -> Tbool true

let format_message =
  let open Jg_types in function
    | SUpdPassword user ->
      Info ("upd_password", [ ("user", Tstr user) ])
    | SUpdEMail { user; mail = Some mail } ->
      Info ("upd_email", [ ("user", Tstr user); ("mail", Tstr mail) ])
    | SUpdEMail { user; mail = None } ->
      Info ("upd_email", [ ("user", Tstr user) ])
    | SUpdAdmin { user; level = User } ->
      Info ("set_user", [ ("user", Tstr user)])
    | SUpdAdmin { user; level = Admin } ->
      Info ("set_admin", [ ("user", Tstr user)])
    | SSentToken user ->
      Info ("token_sent", [ ("user", Tstr user)])
    | SDeletedToken user ->
      Info ("token_deleted", [ ("user", Tstr user)])
    | SCreatedUser { user; level } ->
      Info ("created", [ ("user", Tstr user);
                         ("level", level_to_tvalue level) ])
    | SCreatedUserSentToken { user; mail; level } ->
      Info ("created", [ ("user", Tstr user);
                         ("level", level_to_tvalue level);
                         ("mail", Tstr mail) ])
    | SCreatedUserWithToken { user; token; level } ->
      Info ("created", [ ("user", Tstr user);
                         ("level", level_to_tvalue level);
                         ("token", Tstr token) ])
    | SDeletedUser user ->
      Info ("user_deleted", [("user",Tstr user)])
    | FDatabase err ->
      Error ("err_db", [("detail", Tstr err)])
    | FExternal err ->
      Error ("err_ext", [("detail", Tstr err)])
    | FDeleteAllAdmin ->
      Error ("err_delete_all_admin", [])
    | FDeleteCurrent ->
      Error ("err_delete_logged_in", [])
    | FDeleteNotConfirmed user ->
      Error ("err_delete_unconfirmed", [("user",Tstr user)])
    | FPasswordMismatch ->
      Error ("err_pw_mismatch", [])
    | FAuth Model.User ->
      Error ("err_auth_user", [])
    | FAuth Model.Admin ->
      Error ("err_auth_admin", [])

let format_messages msgs =
  let open Jg_types in
  let (infos, errors) =
    List.fold_left (fun (infos, errors) msg ->
        let fmt key data = Tobj (("key", Tstr key) :: data) in
        match format_message msg with
        | Info (key, data) -> (fmt key data :: infos, errors)
        | Error (key, data) -> (infos, fmt key data :: errors))
      ([], []) msgs in
  [("infos", Tlist infos);
   ("errors", Tlist errors)]

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
                ("expires", Tstr (Time.format_timeout timeout))]
        | None, None -> Tnull
        | _, _ -> Tobj ["message", Tstr "inconsistent token state"])
    ]
  in [("users", Tlist (List.map format_user users))]

type page_status = StatOk | StatAuth | StatError [@@deriving show]

module type Strategy = sig
  type view
  val get_named_argument_opt: view -> string -> string option
  val enumerate_arguments: view -> string list

  val get_session_data: view -> string option
  val set_session_data: view -> string -> unit
  val reset_session_data: view -> unit

  val output_page: view -> page_status -> string -> unit
end
exception UnknownOperation of string

module Make(ModelImpl: Model.S)(Strat: Strategy):
  View.S with type model = ModelImpl.db and  type view = Strat.view =
struct
  open Controller
  open Strat
  type model = ModelImpl.db
  type view = Strat.view

  let get_named_argument view field =
    match get_named_argument_opt view field with
    | Some data -> data
    | None -> raise (ArgumentMissing field)

  let get_bool view field =
    get_named_argument_opt view field <> None

  let get_level view field = 
    match get_named_argument_opt view field with
    | None -> Model.User
    | Some _ -> Model.Admin

  let get_login_operation view =
    match get_named_argument_opt view "operation" with
    | Some "login" -> Login
    | Some "forgot" -> Forgot
    | None -> NoOperation
    | Some op -> raise (UnknownOperation op)

  let get_admin_operation view =
    match get_named_argument_opt view "operation" with
    | Some "logout" -> Logout
    | Some "set_pass" -> SetPass
    | Some "set_mail" -> SetMail
    | Some "delete" -> Delete
    | Some "create" -> Create
    | Some "mass_update" -> MassUpdate
    | None -> NoOperation
    | Some op -> raise (UnknownOperation op)

  let get_named_argument_nullopt view field =
    match get_named_argument_opt view field with
    | None | Some "" -> None
    | Some value -> Some value

  let get_login_user view = get_named_argument view "user"
  let get_login_pass view = get_named_argument view "pass"
  let get_admin_chpass_pass1 view = get_named_argument view "pass1"
  let get_admin_chpass_pass2 view = get_named_argument view "pass2"
  let get_admin_chmail_mail view = get_named_argument_nullopt view "mail"
  let get_admin_delete_confirm view = get_bool view "confirm"
  let get_admin_create_user view = get_named_argument view "user"
  let get_admin_create_pass view = get_named_argument_nullopt view "pass"
  let get_admin_create_mail view = get_named_argument_nullopt view "mail"
  let get_admin_create_level view = get_level view "admin"
  let get_forgot_user view = get_named_argument view "user"
  let get_forgot_token view = get_named_argument view "token"
  let get_forgot_pass1 view = get_named_argument_opt view "pass1"
  let get_forgot_pass2 view = get_named_argument_opt view "pass2"

  let get_admin_sessionid view =
    match get_session_data view with
    | Some session -> session
    | None -> raise Not_found

  let rec filter_map (f: 'a -> 'b option) = function
    | [] -> []
    | x::l -> match f x with
      | Some y -> y :: filter_map f l
      | None -> filter_map f l

  let get_starts_with prefix str =
    let plen = String.length prefix
    and slen = String.length str in
    if String.sub str 0 plen = prefix then
      Some (String.sub str plen (slen - plen))
    else
      None

  let get_name_field = get_starts_with "user:"

  let get_actions view user =
    let token_mk = get_bool view ("mktok:" ^ user)
    and token_rm = get_bool view ("rmtok:" ^ user)
    and mail_old = get_named_argument_nullopt view ("omail:" ^ user)
    and mail_new = get_named_argument_nullopt view ("nmail:" ^ user)
    and pass_new = get_named_argument_nullopt view ("pass:" ^ user)
    and level_old = get_level view ("olevel:" ^ user)
    and level_new = get_level view ("nlevel:" ^ user)
    and delete = get_bool view ("delete:" ^ user) in
    let open Model in
    if delete then [TaskDelete user] else
      (if token_mk then [TaskCreateToken user]
       else if token_rm then [TaskDeleteToken user]
       else []) @
      (if mail_old <> mail_new
       then [TaskSetEMail {user; mail = mail_new}]
       else []) @
      (if level_old <> level_new
       then [TaskSetAdmin {user; level = level_new}]
       else []) @
      (match pass_new with
       | Some pass -> [TaskSetPassword {user; pass}]
       | None -> [])

  let get_admin_mass_update view =
    let users = filter_map get_name_field
        (enumerate_arguments view)
    in List.map (get_actions view) users |> List.flatten

  let format_login db msg =
    let msg_data =
      let open Jg_types in match msg with
      | NoMessage -> Tnull
      | LoginFailed -> Tobj [("key", Tstr "login_failed")]
      | TokenSent user ->
        Tobj [("key", Tstr "token_sent"); ("user", Tstr user)]
    in
    Template.from_file ~models:[
      ("message", msg_data)
    ] "login.html"

  let format_admin_user db user msgs =
    Template.from_file ~models:([
        ("user", Jg_types.Tstr user);
        ("alt_email", match ModelImpl.user_get_email db user with
          | Some addr -> Jg_types.Tstr addr
          | None -> Jg_types.Tnull)
      ] @ format_messages msgs) "admin_user.html"

  let format_admin_admin db user msgs users =
    Template.from_file ~models:([
        ("user", Jg_types.Tstr user);
        ("alt_email", match ModelImpl.user_get_email db user with
          | Some addr -> Jg_types.Tstr addr
          | None -> Jg_types.Tnull)
      ] @ format_messages msgs @ format_users users) "admin_admin.html"

  let format_admin db auth msgs =
    let open Model in
    match auth.auth_level with
    | User -> format_admin_user db auth.auth_user msgs
    | Admin -> format_admin_admin db auth.auth_user msgs
                 (ModelImpl.user_list db auth)

  let format_forgot_form db ~user ~token pw_mismatch =
    Template.from_file ~models:([
        ("user", Jg_types.Tstr user);
        ("token", Jg_types.Tstr token);
        ("pw_mismatch", Jg_types.Tbool pw_mismatch)
      ]) "forgot.html"

  let view_open_session = set_session_data
  let view_close_session = reset_session_data

  let do_output view cont =
    try output_page view StatOk (cont ())
    with e ->
      let errmsg = Printexc.to_string e in
      output_page view StatError errmsg

  let view_login model view msg =
    do_output view (fun () -> format_login model msg)
  let view_admin model view session msgs =
    do_output view (fun () -> format_admin model session msgs)
  let view_forgot_form model view ~user ~token mismatch =
    do_output view (fun () -> format_forgot_form model ~user ~token mismatch)
end

