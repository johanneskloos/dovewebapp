type level = User | Admin [@@deriving show]
exception AuthorizationNeeded of level
exception ExternalFailure of string
type authdata = {
  auth_session : string option;
  auth_user : string;
  auth_level : level;
} [@@deriving show]

val need_same_user: authdata -> string -> unit
(** [need_same_user auth user] does nothing if
    [auth] and [user] refer to the same user, or
    [auth] describes an admin session. Otherwise,
    an [AuthorizationNeeded User] exception is raised. *)
val need_admin: authdata -> unit
(** [need_admin auth] does nothing if [auth] describes
    an admin session. Otherwise, an
    [AuthorizationNeeded Admin] exception is raised. *)

type user_entry = {
  user_name : string;
  user_token : string option;
  user_expires : int64 option;
  user_alt_email : string option;
  user_level : level;
} [@@deriving show]

type task =
  | TaskSetPassword of { user: string; pass: string }
  | TaskSetEMail of { user: string; mail: string option }
  | TaskCreateToken of string (** Give the user name *)
  | TaskDeleteToken of string (** Give the user name *)
  | TaskSetAdmin of { user: string; level: level }
  | TaskDelete of string (** Give the user name *)
[@@deriving show]

type token_info = { user: string; token: string } [@@deriving show]
type email =
    Address of string (** User is known and has the given address. *)
  | NoAddress (** User is known, but has no e-mail address. *)
  | NoSuchUser (** User does not exist. *)
[@@deriving show]


module type S = sig
  type db
  val session_login : db -> user:string -> pass:string -> string option
  val session_logout : db -> authdata -> unit
  val session_retrieve : db -> string -> authdata option
  val session_from_token :
    db -> user:string -> token:string -> authdata option
  val user_update_password :
    db -> authdata -> user:string -> pass:string -> unit
  val user_update_alternative_email :
    db -> authdata -> user:string -> mail:string option -> unit
  val user_delete : db -> authdata -> string -> unit
  val user_create_token : db -> string -> string
  val user_update_admin :
    db -> authdata -> user:string -> level:level -> unit
  val user_delete_token : db -> authdata -> string -> unit
  val user_create_nopw :
    db ->
    authdata ->
    user:string -> altemail:string option -> level:level -> string
  val user_create_pw :
    db ->
    authdata ->
    user:string ->
    pass:string -> altemail:string option -> level:level -> unit
  val user_list : db -> authdata -> user_entry list
  val expire : db -> unit

  val user_task_run : db -> authdata -> task list -> token_info list

  val user_get_email : db -> string -> email
end

