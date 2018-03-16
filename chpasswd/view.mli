type login_messages = TokenSent of string | LoginFailed | NoMessage
val view_login: Model.db -> login_messages -> string
  
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
val view_admin: Model.db -> Model.authdata -> admin_messages list -> string

val view_forgot_form: Model.db -> user:string -> token:string -> bool -> string
