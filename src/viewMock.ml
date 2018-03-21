module Make(ModelImpl: Model.S): sig
  type history_item =
      Login of
        { model: ModelImpl.db;
          message: View.login_messages }
    | Admin of
        { model: ModelImpl.db;
          session: Model.authdata;
          messages: View.admin_messages list }
    | Forgot of
        { model: ModelImpl.db;
          user: string;
          token: string;
          badpw: bool }
  [@@deriving show]

  type viewstate = {
    login_operation: View.login_operation;
    login_user: string option;
    login_pass: string option;
    admin_operation: View.admin_operation;
    admin_chpass_pass1: string option;
    admin_chpass_pass2: string option;
    admin_chmail_mail: string option;
    admin_delete_confirm: bool;
    admin_create_user: string option;
    admin_create_pass: string option;
    admin_create_mail: string option;
    admin_create_level: Model.level;
    admin_mass_update: Model.task list;
    forgot_user: string option;
    forgot_token: string option;
    forgot_pass1: string option;
    forgot_pass2: string option;
    mutable session: string option;
    mutable history: history_item list
  }

  include View.S with type model = ModelImpl.db
                  and type view = viewstate
  val make:
    ?login_operation:View.login_operation ->
    ?login_user:string ->
    ?login_pass:string ->
    ?admin_operation:View.admin_operation ->
    ?admin_chpass_pass1:string ->
    ?admin_chpass_pass2:string ->
    ?admin_chmail_mail:string ->
    ?admin_delete_confirm:bool ->
    ?admin_create_user:string ->
    ?admin_create_pass:string ->
    ?admin_create_mail:string ->
    ?admin_create_level: Model.level ->
    ?admin_mass_update: Model.task list ->
    ?forgot_user:string ->
    ?forgot_token:string ->
    ?forgot_pass1:string ->
    ?forgot_pass2:string ->
    ?session:string ->
    unit -> view
end =
struct
  open Model
  open View

  type model = ModelImpl.db
  type history_item =
      Login of { model: (model [@opaque]); message: login_messages }
    | Admin of { model: (model [@opaque]); session: authdata;
                 messages: admin_messages list }
    | Forgot of { model: (model [@opaque]); user: string; token: string;
                  badpw: bool }
  [@@deriving show]

  type viewstate = {
    login_operation: login_operation;
    login_user: string option;
    login_pass: string option;
    admin_operation: admin_operation;
    admin_chpass_pass1: string option;
    admin_chpass_pass2: string option;
    admin_chmail_mail: string option;
    admin_delete_confirm: bool;
    admin_create_user: string option;
    admin_create_pass: string option;
    admin_create_mail: string option;
    admin_create_level: level;
    admin_mass_update: task list;
    forgot_user: string option;
    forgot_token: string option;
    forgot_pass1: string option;
    forgot_pass2: string option;
    mutable session: string option;
    mutable history: history_item list
  }
  type view = viewstate

  let get = function
    | Some value -> value
    | None -> raise (Controller.ArgumentMissing "")
  let get_login_operation { login_operation } = login_operation
  let get_login_user { login_user } = get login_user
  let get_login_pass { login_pass } = get login_pass
  let get_admin_sessionid { session } = get session
  let get_admin_operation { admin_operation } = admin_operation
  let get_admin_chpass_pass1 { admin_chpass_pass1 } =
    get admin_chpass_pass1
  let get_admin_chpass_pass2 { admin_chpass_pass2 } =
    get admin_chpass_pass2
  let get_admin_chmail_mail { admin_chmail_mail } = admin_chmail_mail
  let get_admin_delete_confirm { admin_delete_confirm } =
    admin_delete_confirm
  let get_admin_create_user { admin_create_user } =
    get admin_create_user
  let get_admin_create_pass { admin_create_pass } =
    admin_create_pass
  let get_admin_create_mail { admin_create_mail } =
    admin_create_mail
  let get_admin_create_level { admin_create_level } =
    admin_create_level
  let get_admin_mass_update { admin_mass_update } = admin_mass_update
  let get_forgot_user { forgot_user } = get forgot_user
  let get_forgot_token { forgot_token } = get forgot_token
  let get_forgot_pass1 { forgot_pass1 } = forgot_pass1
  let get_forgot_pass2 { forgot_pass2 } = forgot_pass2
  let view_open_session view session = view.session <- Some session
  let view_close_session view = view.session <- None
  let view_login model view message =
    view.history <- Login { model; message } :: view.history
  let view_admin model view session messages =
    view.history <-
      Admin { model; session; messages } :: view.history
  let view_forgot_form model view ~user ~token badpw =
    view.history <-
      Forgot { model; user; token; badpw } :: view.history
  let make
      ?(login_operation=(NoOperation: login_operation))
      ?login_user ?login_pass
      ?(admin_operation=(NoOperation: admin_operation))
      ?admin_chpass_pass1
      ?admin_chpass_pass2
      ?admin_chmail_mail
      ?(admin_delete_confirm=false)
      ?admin_create_user
      ?admin_create_pass
      ?admin_create_mail
      ?(admin_create_level=Model.User)
      ?(admin_mass_update=[])
      ?forgot_user
      ?forgot_token
      ?forgot_pass1
      ?forgot_pass2
      ?session () =
    { login_operation; login_user; login_pass;
      admin_operation; admin_chpass_pass1; admin_chpass_pass2;
      admin_chmail_mail; admin_delete_confirm; admin_create_user;
      admin_create_pass; admin_create_mail; admin_create_level;
      admin_mass_update; forgot_user; forgot_token;
      forgot_pass1; forgot_pass2; session; history = [] }
end
