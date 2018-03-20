type login_messages = TokenSent of string | LoginFailed | NoMessage

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

type login_operation = Login | Forgot | NoOperation
type admin_operation =
    Logout | SetPass | SetMail | Delete | Create | MassUpdate | NoOperation

module type S = sig
  type model
  type view
  val get_login_operation: view -> login_operation
  val get_login_user: view -> string
  val get_login_pass: view -> string
  val get_admin_sessionid: view -> string
  val get_admin_operation: view -> admin_operation
  val get_admin_chpass_pass1: view -> string
  val get_admin_chpass_pass2: view -> string
  val get_admin_chmail_mail: view -> string option
  val get_admin_delete_confirm: view -> bool
  val get_admin_create_user: view -> string
  val get_admin_create_pass: view -> string option
  val get_admin_create_mail: view -> string option
  val get_admin_create_level: view -> Model.level
  val get_admin_mass_update: view -> Model.task list
  val get_forgot_user: view -> string
  val get_forgot_token: view -> string
  val get_forgot_pass1: view -> string option
  val get_forgot_pass2: view -> string option
  val view_open_session: view -> string -> unit
  val view_close_session: view -> unit
  val view_login: model -> view -> login_messages -> unit
  val view_admin: model -> view -> Model.authdata -> admin_messages list -> unit
  val view_forgot_form: model -> view -> user:string -> token:string -> bool -> unit
end
