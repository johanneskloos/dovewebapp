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

module Make(ModelImpl: Model.S): sig
  val view_login: ModelImpl.db -> login_messages -> string
  val view_admin: ModelImpl.db -> Model.authdata -> admin_messages list -> string
  val view_forgot_form: ModelImpl.db -> user:string -> token:string -> bool -> string
end
