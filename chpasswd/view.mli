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

module type S = sig
  type model
  val view_login: model -> login_messages -> string
  val view_admin: model -> Model.authdata -> admin_messages list -> string
  val view_forgot_form: model -> user:string -> token:string -> bool -> string
end
